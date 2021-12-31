+++
title = "「Maglev: A Fast and Reliable Software Network Load Balancer」読んだ"
date = 2021-12-29
[taxonomies]
tags = ["linux","network"]
+++

https://www.usenix.org/system/files/conference/nsdi16/nsdi16-paper-eisenbud-update.pdf

## モチベーション
個人的によくスケールするソフトウェアロードバランサの理想形みたいなイメージを持っていたMaglevについて、

1. 何が問題だったのか
2. どうやって解いたのか

を中心に論文を読んでみる。

## 面白かったところ
Maglevの独自性は

- ハッシュアルゴリズムによる、バランシングの均等性とbackendの増減に対する安定性
- Linux kernelのネットワークスタックをbypassすることによる高スループット

あたりであると読んだ。

### 3.2 Fast Packet Processing
Packet Poolに実データが乗っていて、ring bufferはポインタだけ持っている、っていう風に理解した。ということはドライバからPacket Poolにアクセスしたりしてるのか？
全てがPacker Pool上で完結するのでRX/TXのキューにデータのコピーが発生しないので高速、とのこと。
確かにLinuxだとsk_buffの確保時にドライバからコピーが走っているはずなので。
ただちょっと仕組みについて細かく理解できてないのでもう少し知りたい。
### 3.4 Consistent Hashing
consistent hashing自体をちょっと自分で実装してみた方が理解が早そう。

ポイントは

- backendができるだけ均等にハッシュテーブル上に出現する
- backendの変更があった場合に既存のハッシュテーブルのマッピングはできるだけ変更しない
    - backendの変更によってハッシュの対応が変わるとこれまでの5-tupleで識別されていたトラフィックは異なるbackendにルーティングされてしまいTCPのようなstatefulな通信は失敗する

参考

HAProxy consistent hasing https://www.haproxy.com/blog/haproxys-load-balancing-algorithm-for-static-content-delivery-with-varnish/

(ただこれはcacheサーバとしての利用らしい)

### 4.1.1 Failover
Maglevも昔はactive-standby構成でVIP振って動かしていたらしい。それはいずれやめたくなるのは同意。
これがECMPの導入によって複数active設計へと変貌するわけだが、ECMPしかないのかな？という疑問はある。
というか前段のルータにてBGP設定するしかないのをなんとかしたいというか。
### 4.1.2 Packet Processing
かつてはLinuxのsocket経由でパケット処理をしていたそうだが、kernelからuserにデータコピーが起こるのでオーバヘッドが非常に大きかったらしい。
これは現在同じ仕組みでやるならAF_XDPやXDPなどで解決するだろうか？
### 4.1.2 Packet Processing

Blackbox: VIPの到達性の外形監視
Whitebox: Maglev上のHTTP Serverに対してメトリクスの収集

### 5.2 Single Machine Throughput

### 6 Related Work
NginxやHAProxy、LVSもECMPで複数サーバに対応しておりscale-outモデルを導入できるし、consistent hashingも利用可能であるが、均等なバランシングとdisruption対応をMaglevほどうまくは扱えていないらしい。

> minimum disruption over even load balancing as is done by [28] and [38]

## 疑問に思うところ


### 

さらっと気になること書いてあるんだよな。

> In Maglev we im-plement a flexible I/O layer which does not require kernel
> modification and allows us to conveniently switch among
> different NICs. As with other techniques, Maglev takes
> over the NIC once started. It uses the TAP interface to
> inject kernel packets back to the kernel.
