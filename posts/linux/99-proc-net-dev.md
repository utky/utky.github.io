---
title: procfs ネットワーク周りを覗き見る /proc/net/dev 編
date: 2020-05-10
tags: linux, network
---

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

## net/core/net-procfs.c

"dev" の初期化
```c
	if (!proc_create_net("dev", 0444, net->proc_net, &dev_seq_ops,
			sizeof(struct seq_net_private)))
```

ここが `dev` を読み出す時に呼び出される関数の対応を表す。
```c
static const struct file_operations dev_seq_fops = {
	.owner	 = THIS_MODULE,
	.open    = dev_seq_open,
	.read    = seq_read,
	.llseek  = seq_lseek,
	.release = seq_release_net,
};
```

ここで `file` に `dev_seq_ops` をアタッチする。
```c
static int dev_seq_open(struct inode *inode, struct file *file)
{
	return seq_open_net(inode, file, &dev_seq_ops,
			    sizeof(struct seq_net_private));
}
```

アタッチされる操作は `dev_seq_ops` に定義されている。
```
static const struct seq_operations dev_seq_ops = {
	.start = dev_seq_start,
	.next  = dev_seq_next,
	.stop  = dev_seq_stop,
	.show  = dev_seq_show,
};
```

読み出し開始のトリガになっていると思われる `dev_seq_start` 
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

統計データは下記の関数から netdev のメソッド経由で取得される。

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