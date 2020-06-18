+++
title = "procfs ネットワーク周りを覗き見る /proc/net/dev 編"
date = 2020-05-10
aliases = ["/posts/linux/99-proc-net-dev.html"]
[taxonomies]
tags = ["linux","network","procfs"]
+++

procfs からシステムの統計データが色々採取できる。
ネットワーク周りの統計データとしてどんなものがどうやって収集されているのかを追ってみることにした。

```
$ cat /proc/net/dev
Inter-|   Receive                                                |  Transmit
 face |bytes    packets errs drop fifo frame compressed multicast|bytes    packets errs drop fifo colls carrier compressed
  eth0: 5067818364 3512860    0    0    0     0          0         0 17158713  213446    0    0    0     0       0          0
    lo:       0       0    0    0    0     0          0         0        0       0    0    0    0     0       0          0
```

## カーネルとディストリビューション

```bash
$ cat /etc/redhat-release 
CentOS Linux release 7.6.1810 (Core)
$ uname -srvp
Linux 3.10.0-957.12.2.el7.x86_64 #1 SMP Tue May 14 21:24:32 UTC 2019 x86_64
```

## dev ファイルの初期化

net/core/net-procfs.c にてデバイス関連の procfs の初期化を行っている。

dev ファイルに dev_seq_fops というコールバック関数を含む構造体を関連付けるのが主な処理となる。
実際に dev ファイルを読み込む時のアクションはこの関連付けられたコールバック関数の責務となる。
```c
	if (!proc_create("dev", S_IRUGO, net->proc_net, &dev_seq_fops))
```

dev_seq_fops にはいわゆるファイルアクセスで発生する open, read, seek, close に対応した関数の関連付けが定義される。
```c
static const struct file_operations dev_seq_fops = {
	.owner	 = THIS_MODULE,
	.open    = dev_seq_open,
	.read    = seq_read,
	.llseek  = seq_lseek,
	.release = seq_release_net,
};
```

open で呼び出される関数 dev_seq_open をみて、dev ファイルを開いた際に発生するアクションについて確認する。
と言ってもここでは file 構造体にまた dev_seq_ops というコールバック関数を含む構造体を関連付けているだけとなる。
```c
static int dev_seq_open(struct inode *inode, struct file *file)
{
	return seq_open_net(inode, file, &dev_seq_ops,
			    sizeof(struct seq_net_private));
}
```

関連付けられた `dev_seq_ops` は dev ファイルを反復的に読み出す際のアクションを定義している。
```
static const struct seq_operations dev_seq_ops = {
	.start = dev_seq_start,
	.next  = dev_seq_next,
	.stop  = dev_seq_stop,
	.show  = dev_seq_show,
};
```

## dev ファイルの読み込み

読み込み開始のトリガになっていると思われる `dev_seq_start` は下記。
返却されたポインタは `dev_seq_show` に渡される。
```c
/*
 *	This is invoked by the /proc filesystem handler to display a device
 *	in detail.
 */
static void *dev_seq_start(struct seq_file *seq, loff_t *pos)
	__acquires(RCU)
{
	rcu_read_lock();
	if (!*pos)
		return SEQ_START_TOKEN;

	if (get_bucket(*pos) >= NETDEV_HASHENTRIES)
		return NULL;

	return dev_from_bucket(seq, pos);
}
```

`v` は `dev_seq_start` で返却されたポインタが格納されている。
`SEQ_START_TOKEN` が格納されている場合は出力データの先頭から読み出すことを示すため、
表示テーブルの列ヘッダを印字する。
それ以降は `dev_seq_printf_stats` にネットワークデバイス情報が渡される。
```c
/*
 *	Called from the PROCfs module. This now uses the new arbitrary sized
 *	/proc/net interface to create /proc/net/dev
 */
static int dev_seq_show(struct seq_file *seq, void *v)
{
	if (v == SEQ_START_TOKEN)
		seq_puts(seq, "Inter-|   Receive                            "
			      "                    |  Transmit\n"
			      " face |bytes    packets errs drop fifo frame "
			      "compressed multicast|bytes    packets errs "
			      "drop fifo colls carrier compressed\n");
	else
		dev_seq_printf_stats(seq, v);
	return 0;
}
```

ネットワークデバイスごとの統計情報が印字される。
`temp` に統計を書き込んでもらって、それのポインタをまた `const` で受け取ってる。
ちょっと回りくどい感じがするが何でだろう。
```c
static void dev_seq_printf_stats(struct seq_file *seq, struct net_device *dev)
{
	struct rtnl_link_stats64 temp;
	const struct rtnl_link_stats64 *stats = dev_get_stats(dev, &temp);

	seq_printf(seq, "%6s: %7llu %7llu %4llu %4llu %4llu %5llu %10llu %9llu "
		   "%8llu %7llu %4llu %4llu %4llu %5llu %7llu %10llu\n",
		   dev->name, stats->rx_bytes, stats->rx_packets,
		   stats->rx_errors,
		   stats->rx_dropped + stats->rx_missed_errors,
		   stats->rx_fifo_errors,
		   stats->rx_length_errors + stats->rx_over_errors +
		    stats->rx_crc_errors + stats->rx_frame_errors,
		   stats->rx_compressed, stats->multicast,
		   stats->tx_bytes, stats->tx_packets,
		   stats->tx_errors, stats->tx_dropped,
		   stats->tx_fifo_errors, stats->collisions,
		   stats->tx_carrier_errors +
		    stats->tx_aborted_errors +
		    stats->tx_window_errors +
		    stats->tx_heartbeat_errors,
		   stats->tx_compressed);
}
```

統計データ `rtnl_link_stats64` は下記の関数から netdev のメソッド経由で取得される。
`ndo_get_stats64`/`ndo_get_stats` のメソッド実装があればそれを使うしなければ、netdev の stats を使う。
net/core/dev.c
```c
/**
 *	dev_get_stats	- get network device statistics
 *	@dev: device to get statistics from
 *	@storage: place to store stats
 *
 *	Get network statistics from device. Return @storage.
 *	The device driver may provide its own method by setting
 *	dev->netdev_ops->get_stats64 or dev->netdev_ops->get_stats;
 *	otherwise the internal statistics structure is used.
 */
struct rtnl_link_stats64 *dev_get_stats(struct net_device *dev,
					struct rtnl_link_stats64 *storage)
{
	const struct net_device_ops *ops = dev->netdev_ops;

	if (ops->ndo_get_stats64) {
		memset(storage, 0, sizeof(*storage));
		ops->ndo_get_stats64(dev, storage);
	} else if (ops->ndo_get_stats) {
		netdev_stats_to_stats64(storage, ops->ndo_get_stats(dev));
	} else {
		netdev_stats_to_stats64(storage, &dev->stats);
	}
	storage->rx_dropped += atomic_long_read(&dev->rx_dropped);
	return storage;
}
```

`ndo_get_stats64` のメソッドは各デバイスの初期化時にデバイスドライバ側で設定される。

## 統計情報 rtnl_link_stats64

include/uapi/linux/if_link.h
```c
/* The main device statistics structure */
struct rtnl_link_stats64 {
	__u64	rx_packets;		/* total packets received	*/
	__u64	tx_packets;		/* total packets transmitted	*/
	__u64	rx_bytes;		/* total bytes received 	*/
	__u64	tx_bytes;		/* total bytes transmitted	*/
	__u64	rx_errors;		/* bad packets received		*/
	__u64	tx_errors;		/* packet transmit problems	*/
	__u64	rx_dropped;		/* no space in linux buffers	*/
	__u64	tx_dropped;		/* no space available in linux	*/
	__u64	multicast;		/* multicast packets received	*/
	__u64	collisions;

	/* detailed rx_errors: */
	__u64	rx_length_errors;
	__u64	rx_over_errors;		/* receiver ring buff overflow	*/
	__u64	rx_crc_errors;		/* recved pkt with crc error	*/
	__u64	rx_frame_errors;	/* recv'd frame alignment error */
	__u64	rx_fifo_errors;		/* recv'r fifo overrun		*/
	__u64	rx_missed_errors;	/* receiver missed packet	*/

	/* detailed tx_errors */
	__u64	tx_aborted_errors;
	__u64	tx_carrier_errors;
	__u64	tx_fifo_errors;
	__u64	tx_heartbeat_errors;
	__u64	tx_window_errors;

	/* for cslip etc */
	__u64	rx_compressed;
	__u64	tx_compressed;
};
```
最初のひとかたまりのメンバーはよく見るので疑問はない。
一方でエラーの統計以下のメンバーについては詳細をよく知らないものも多いためここで確認しておく。

統計の更新はドライバ側で行う。
今回は Realtek のドライバで確認してみる。
`drivers/net/ethernet/realtek/8139cp.c` 

## RX 系エラー

#### __u64	rx_length_errors;
#### __u64	rx_over_errors;		/* receiver ring buff overflow	*/
#### __u64	rx_crc_errors;		/* recved pkt with crc error	*/
#### __u64	rx_frame_errors;	/* recv'd frame alignment error */
#### __u64	rx_fifo_errors;		/* recv'r fifo overrun		*/
#### __u64	rx_missed_errors;	/* receiver missed packet	*/

## TX 系エラー

#### __u64	tx_aborted_errors;
#### __u64	tx_carrier_errors;
#### __u64	tx_fifo_errors;
#### __u64	tx_heartbeat_errors;
#### __u64	tx_window_errors;

#### __u64	rx_compressed;
#### __u64	tx_compressed;