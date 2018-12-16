---
title: Network Device
date: 2018-12-16
---

ネットワークデバイスを表す構造体は`include/linux/netdevice.h`にある`net_device`という名前で定義されている。

怪物のような構造体。低レベルなデータと高レベルなデータが混ざった複雑な構造をしている。
やばい。
プロトコル固有データが詰め込まれており、すごく混沌としている。

ライフサイクル
-----------

1. init : デバイスの登録
2. open : デバイスのup
3. stop : デバイスのdown
4. uninit : デバイスの削除

デバイスの作成
--------

物理デバイスの場合はデバイスドライバにより作成される。
ここではサンプルとして平田豊さんの「Linuxカーネル解析入門」でも触れられていた
Realtekの8139チップ用ドライバの実装をのぞいてみる

`drivers/net/ethernet/realtek/8139cp.c`

ドライバの初期化処理においてデバイス構造体のメモリ領域を確保している。

```
static int cp_init_one (struct pci_dev *pdev, const struct pci_device_id *ent)
<snip>
	struct net_device *dev;
	struct cp_private *cp;
<snip>
	dev = alloc_etherdev(sizeof(struct cp_private));
```

```
	rc = register_netdev(dev);
```

このアロケーションはLinux本体で定義されている。
デバイス固有の構造体のサイズに加えて共通的な構造体のサイズも確保する。

`etherdevice.h`
```
#define alloc_etherdev(sizeof_priv) alloc_etherdev_mq(sizeof_priv, 1)
#define alloc_etherdev_mq(sizeof_priv, count) alloc_etherdev_mqs(sizeof_priv, count, count)
```

送受信キューの数に応じてメモリサイズが変わるためこの時点でキューの数を渡している。

`net/ethernet/eth.c`
```
struct net_device *alloc_etherdev_mqs(int sizeof_priv, unsigned int txqs,
				      unsigned int rxqs)
```

`core`の汎用関数で実際のメモリ確保を行う。

`net/core/dev.c`
```
struct net_device *alloc_netdev_mqs(int sizeof_priv, const char *name,
		unsigned char name_assign_type,
		void (*setup)(struct net_device *),
		unsigned int txqs, unsigned int rxqs)
```

デバイスの登録
---------

```
int register_netdevice(struct net_device *dev);
```

`net/dev.c`
