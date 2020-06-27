+++
title = "procfs ネットワーク周りを覗き見る /proc/net/dev 編"
date = 2020-06-27
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

ここではネットワークインタフェースのドライバのコードに少し触れるけれど、
動作の詳細は [Linuxカーネル解析入門](https://www.amazon.co.jp/dp/4777516156) の「第６章 ネットワーク・ドライバを読む」に書いてある。

というかもうそこ読んでカウンタ更新部分のコード読めば終わりなのでこんな記事は読まなくても良いのだ。。。

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

統計データ `rtnl_link_stats64` は下記の関数から netdev のハンドラ経由で取得される。
`ndo_get_stats64`/`ndo_get_stats` のハンドラ実装があればそれを使うし、なければ netdev の stats を使う。
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

`ops->ndo_get_stats64` のハンドラは各デバイスの初期化時にデバイスドライバ側で設定されており、
ドライバ固有の操作となる。

## 統計情報 rtnl_link_stats64

印字されているメトリクスは結局のところドライバ側で計測したものであることが分かった。
メトリクスを保存している構造体は下記のような定義となっている。

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

フィールド数が多いので、ここでは rx_packets ~ collisions に絞って、
これらのよく見る統計がどうやって加算されているのかを、具体的なドライバのコードを見つつ確認してみる。
ちなみに multicast のカウンタ更新している箇所は今回のドライバのコードには見つからなかった。

今回は [Realtek のドライバ 8319C+](http://realtek.info/pdf/rtl8139cp.pdf) で確認してみる。

コードは [drivers/net/ethernet/realtek/8139cp.c](https://github.com/torvalds/linux/blob/v3.10/drivers/net/ethernet/realtek/8139cp.c) にある。

### rx_packets, rx_bytes

パケット受信時に更新される下記の指標を表す。

* rx_packets: 受信パケット数
* rx_bytes: 受信データ量(bytes)

`skb` (ネットワークパケットのデータが乗っている) の長さを `rx_bytes` としてカウントしているのがわかる。

```c
static inline void cp_rx_skb (struct cp_private *cp, struct sk_buff *skb,
                  struct cp_desc *desc)
{
    u32 opts2 = le32_to_cpu(desc->opts2);

    skb->protocol = eth_type_trans (skb, cp->dev);

    cp->dev->stats.rx_packets++;
    cp->dev->stats.rx_bytes += skb->len;

    if (opts2 & RxVlanTagged)
        __vlan_hwaccel_put_tag(skb, htons(ETH_P_8021Q), swab16(opts2 & 0xffff));

    napi_gro_receive(&cp->napi, skb);
}
```

カウンタ更新後に `napi_gro_receive` を呼び出してカーネルのネットワークスタック層での処理を依頼する。

`cp_rx_skb` という関数自体は `cp_rx_poll` というパケットの受信ループから呼ばれる。
`cp_rx_poll` はデバイスがパケット受信時の割り込みを契機として起動される。

### tx_packets, tx_bytes, tx_errors, collisions

パケット送信時に更新される下記の指標を表す。

* tx_packets: 送信パケット数
* tx_bytes: 送信データ量(bytes)
* tx_errors: 送信エラー
* collisions: Ethernet の衝突検知

`cp_tx` という関数にて、 ring buffer の中身からとりだした `status` の結果を確認し成功なら送信系のカウンタを更新する。

```c
static void cp_tx (struct cp_private *cp)
{
<snip>
        if (status & LastFrag) {
            if (status & (TxError | TxFIFOUnder)) {
                netif_dbg(cp, tx_err, cp->dev,
                      "tx err, status 0x%x\n", status);
                cp->dev->stats.tx_errors++;
                if (status & TxOWC)
                    cp->dev->stats.tx_window_errors++;
                if (status & TxMaxCol)
                    cp->dev->stats.tx_aborted_errors++;
                if (status & TxLinkFail)
                    cp->dev->stats.tx_carrier_errors++;
                if (status & TxFIFOUnder)
                    cp->dev->stats.tx_fifo_errors++;
            } else {
                cp->dev->stats.collisions +=
                    ((status >> TxColCntShift) & TxColCntMask);
                cp->dev->stats.tx_packets++;
                cp->dev->stats.tx_bytes += skb->len;
                netif_dbg(cp, tx_done, cp->dev,
                      "tx done, slot %d\n", tx_tail);
            }
```

パケットの送信をデバイスに依頼した後、デバイスから完了通知の割り込みを受け取ると
`cp_tx` が呼び出されてカウンタ更新が行われる。

いずれもデバイスから設定されたフラグに応じてカウンタを更新しているだけなので、
ドライバ側では特別な操作は何もしていないことになる。

### rx_errors

何らかの理由で受信に失敗したパケット数を表す。

rx_errors カウンタは専用のエラーハンドル用関数で計測されている。
```c
static void cp_rx_err_acct (struct cp_private *cp, unsigned rx_tail,
                u32 status, u32 len)
{
    netif_dbg(cp, rx_err, cp->dev, "rx err, slot %d status 0x%x len %d\n",
          rx_tail, status, len);
    cp->dev->stats.rx_errors++;
    if (status & RxErrFrame)
        cp->dev->stats.rx_frame_errors++;
    if (status & RxErrCRC)
        cp->dev->stats.rx_crc_errors++;
    if ((status & RxErrRunt) || (status & RxErrLong))
        cp->dev->stats.rx_length_errors++;
    if ((status & (FirstFrag | LastFrag)) != (FirstFrag | LastFrag))
        cp->dev->stats.rx_length_errors++;
    if (status & RxErrFIFO)
        cp->dev->stats.rx_fifo_errors++;
}
```

この関数はデバイスからDMA経由で設定されたフラグのエラービットが立っていた場合に呼び出されるようになっている。
```c
        if (status & (RxError | RxErrFIFO)) {
            cp_rx_err_acct(cp, rx_tail, status, len);
            goto rx_next;
        }
```

### rx_dropped

受信パケットのドロップは以下の二つのケースで更新される。

* セグメンテーションが行われている場合（未サポート）
* デバイスから受け取ったパケットを `skb` にコピーするための領域が確保できない

```c
static int cp_rx_poll(struct napi_struct *napi, int budget)
{
<snip>
        if ((status & (FirstFrag | LastFrag)) != (FirstFrag | LastFrag)) {
            /* we don't support incoming fragmented frames.
             * instead, we attempt to ensure that the
             * pre-allocated RX skbs are properly sized such
             * that RX fragments are never encountered
             */
            cp_rx_err_acct(cp, rx_tail, status, len);
            dev->stats.rx_dropped++;
            cp->cp_stats.rx_frags++;
            goto rx_next;
        }
<snip>
      new_skb = netdev_alloc_skb_ip_align(dev, buflen);
        if (!new_skb) {
            dev->stats.rx_dropped++;
            goto rx_next;
        }
```

### tx_dropped

送信用 ring buffer の skb 用領域を解放するときにデータが残っていれば、
それはデバイスから取り出されなかったものと見なしてドロップ扱いになる。

```c
static void cp_clean_rings (struct cp_private *cp)
{
    struct cp_desc *desc;
    unsigned i;

<snip>

    for (i = 0; i < CP_TX_RING_SIZE; i++) {
        if (cp->tx_skb[i]) {
            struct sk_buff *skb = cp->tx_skb[i];

            desc = cp->tx_ring + i;
            dma_unmap_single(&cp->pdev->dev,le64_to_cpu(desc->addr),
                     le32_to_cpu(desc->opts1) & 0xffff,
                     PCI_DMA_TODEVICE);
            if (le32_to_cpu(desc->opts1) & LastFrag)
                dev_kfree_skb(skb);
            cp->dev->stats.tx_dropped++;
        }
    }
```

この `cp_clean_rings` はドライバの開始・終了時にも呼び出されるが、
送信タイムアウト時のハンドラ `cp_tx_timeout` にも呼び出される。
ネットワークデバイスが起動中に tx_dropped カウンタが増えるのはこの `cp_tx_timeout` 経由での呼び出しの場合が主であるようだ。

## 読んでみて

もう少しドライバのデータシートちゃんと読まないとだなって思いました。
