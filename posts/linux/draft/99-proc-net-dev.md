---
title: procfs ネットワーク周りを覗き見る /proc/net/dev 編
date: 2020-04-30
tags: linux, network
---

procfs からシステムの統計データが色々採取できる。
ネットワーク周りの統計データとしてどんなものがどうやって収集されているのかを追ってみる。

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

```c
static int dev_seq_open(struct inode *inode, struct file *file)
{
	return seq_open_net(inode, file, &dev_seq_ops,
			    sizeof(struct seq_net_private));
}
```

fs/proc/proc_net.c
```c
int seq_open_net(struct inode *ino, struct file *f,
		 const struct seq_operations *ops, int size)
{
	struct net *net;
	struct seq_net_private *p;

	BUG_ON(size < sizeof(*p));

	net = get_proc_net(ino);
	if (net == NULL)
		return -ENXIO;

	p = __seq_open_private(f, ops, size);
	if (p == NULL) {
		put_net(net);
		return -ENOMEM;
	}
#ifdef CONFIG_NET_NS
	p->net = net;
#endif
	return 0;
}
```

fs/seq_file.c
```c
void *__seq_open_private(struct file *f, const struct seq_operations *ops,
		int psize)
{
	int rc;
	void *private;
	struct seq_file *seq;

	private = kzalloc(psize, GFP_KERNEL);
	if (private == NULL)
		goto out;

	rc = seq_open(f, ops);
	if (rc < 0)
		goto out_free;

	seq = f->private_data;
	seq->private = private;
	return private;

out_free:
	kfree(private);
out:
	return NULL;
}
```

include/linux/seq_net_file.h
```c
struct seq_net_private {
#ifdef CONFIG_NET_NS
	struct net *net;
#endif
}
```

dev を1行ずつ読み出すイテレーションごとに呼ばれる
```
static inline struct net_device *dev_from_bucket(struct seq_file *seq, loff_t *pos)
{
	struct net_device *dev;
	unsigned int bucket;

	do {
		dev = dev_from_same_bucket(seq, pos);
		if (dev)
			return dev;

		bucket = get_bucket(*pos) + 1;
		*pos = set_bucket_offset(bucket, 1);
	} while (bucket < NETDEV_HASHENTRIES);

	return NULL;
}
```
