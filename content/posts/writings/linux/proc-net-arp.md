+++
title = "WIP procfs ネットワーク周りを覗き見る /proc/net/arp 編"
date = 2020-07-02
[taxonomies]
tags = ["linux","network","procfs"]
+++

前回に続きネットワーク回りの procfs のデータの出どころを追いかける。

```
 % cat /proc/net/arp
IP address       HW type     Flags       HW address            Mask     Device
10.0.2.3         0x1         0x2         52:54:00:12:35:03     *        enp0s3
10.0.2.2         0x1         0x2         52:54:00:12:35:02     *        enp0s3
```

net/ipv4/arp.c から procfs の生成をしてはいる。

```c
static const struct seq_operations arp_seq_ops = {
    .start  = arp_seq_start,
    .next   = neigh_seq_next,
    .stop   = neigh_seq_stop,
    .show   = arp_seq_show,
};
```

出力を作っているところは、ヘッダ部とデータ部に分かれている。
データ部を出力する箇所は `arp_format_pneigh_entry` と `arp_format_neigh_entry`
に分かれておりpneighとneighというに2種類のデータがあることが分かる。
```c
static int arp_seq_show(struct seq_file *seq, void *v)
{
    if (v == SEQ_START_TOKEN) {
        seq_puts(seq, "IP address       HW type     Flags       "
                  "HW address            Mask     Device\n");
    } else {
        struct neigh_seq_state *state = seq->private;

        if (state->flags & NEIGH_SEQ_IS_PNEIGH)
            arp_format_pneigh_entry(seq, v);
        else
            arp_format_neigh_entry(seq, v);
    }

    return 0;
}
```

印字する部分はまあそこそこにneighbourテーブルのエントリを整形しているだけなのでそれほど注目しなくてよくて、むしろ重要なのはここで扱われるデータ型だ。
印字用関数のシグニチャを見るとどんなデータがarpテーブルの実体なのかが分かる。
```c
static void arp_format_neigh_entry(struct seq_file *seq,
                   struct neighbour *n)
```

```c
static void arp_format_pneigh_entry(struct seq_file *seq,
                    struct pneigh_entry *n)
```

* pneigh_entry
* neighbour

この2つのデータがarpエントリの実体であることが分かる。

## pneigh_entry

pneighは疑似的なエントリを表す。

## neighbour

## 

`state->flag` を見て呼び出す関数を分けているが、この state は何か。
include/net/neighbour.h
```c
struct neigh_seq_state {
    struct seq_net_private p;
    struct neigh_table *tbl;
    struct neigh_hash_table *nht;
    void *(*neigh_sub_iter)(struct neigh_seq_state *state,
                struct neighbour *n, loff_t *pos);
    unsigned int bucket;
    unsigned int flags;
#define NEIGH_SEQ_NEIGH_ONLY    0x00000001
#define NEIGH_SEQ_IS_PNEIGH 0x00000002
#define NEIGH_SEQ_SKIP_NOARP    0x00000004
};
```

neigh_tableなどのフィールドを持っているようにneighbour管理に必要なデータを持っている。


net/core/neighbour.c に実装がある。


```c
static int __net_init arp_net_init(struct net *net)
{
    if (!proc_create("arp", S_IRUGO, net->proc_net, &arp_seq_fops))
        return -ENOMEM;
    return 0;
}
```