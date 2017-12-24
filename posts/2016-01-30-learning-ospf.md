------------------
title: OSPF reading note
date: 2016-01-30
tags: [network]
------------------

OSPFの処理ステップ

- LSDBの作成
- SPF計算
- 経路テーブルの作成


Messages
--------

### 共通のヘッダ

- Version
- Type
- Packet length
- Router ID
- Area ID
- Checksum
- AuType
- Authentication

ChecksumはOSPFヘッダからOSPFパケットの末尾までを含めて算出するが、
Authenticationは含まないらしい。なんで……。

### Type

- 1: Hello
- 2: Database Description
- 3: Link-State Request
- 4: Link-State Update
- 5: Link-State Acknowledgement

こうしてみるとシンプルですね。
データの要求、広告、レスポンス。

なんとなく見ただけで

```
A == Request ==> B
A <== Update == B
A == Ack ==> B
```

という動作が想像できる。。。違うかもしれないけれど。

### AuType

- 0: 認証なし
- 1: パスワード認証
- 2: 暗号を用いた認証

暗号を用いた認証ってなんだろう？
平文でないということか？
ふつうパスワードって暗号化するんじゃないの？
盗聴されてないとかいう性善説な想定なの？

謎。そして解説がない。他の資料読まないとな。


Hello
------

Helloパケットを送信するルータはOSPFルータである。

NeighborとはHelloパケットの交換に成功した2つのOSPFルータの関係を指す。
つまり`¬Neighbor(A, B)`とは`¬OSPF(A) || ¬OSPF(B)`を表す。

### フィールド

- Network Mask
- HelloInterval
- Options
- Rtr Pri
- RouterDeadInteval
- Designated Router
- Backup Designated Router
- [Neighbor]

### Options

どうやらここでTraffic Engineeringのcapabilityとかを表現している？

### Rtr Pri

DRへのなりやすさを表す数値。高いほどなりやすい。
デフォルトは1。同じPriorityを持つルータがいる場合は、ルータIDの大きい方が優先される。
マルチアクセスネットワークの場合にのみ使う。

### Designated RouterとBackup Designated Router

ルータIDではなく、DRのインタフェースのうちそのネットワークに所属するインタフェースのIPアドレスになる。
あー、マルチアクセスネットワークだと一つのサブネットの中に複数のルータがいて、
それらのどれかがDRになるというシナリオを想定しているから、絶対にDRと共有しているネットワークが一つはある、という発想なのね。

Database Description
--------------------

Adjacencyの初期化をするときに交換される。

ほー、HelloでNeighborを確立して、Database DescriptionでAdjacencyを確立するということか。

つまりは、、、

```
任意のルータA, Bについて
Adjacency(A, B) => Neighbot(A, B)
が成り立つ
```

ということはわかった。

- Interface MTU
- Options
- Iビット
- Mビット
- MSビット
- DD sequence number
- LSA Header


### MSビット

Master/Slaveを表すビット。相手がマスターの場合は1でスレーブの場合は2という0ということのようだ。
しかしMaster/Slaveという立場によってどう意味が違うのか書いていない。
Masterとは何をする役割なのか。

[RFC2328](https://www.ietf.org/rfc/rfc2328.txt)

7.2.  The Synchronization of Databases

> During this process, the two routers form a master/slave relationship. 
> Each Database Description Packet has a sequence number. 
> Database Description Packets sent by the master (polls) are acknowledged by the slave
> through echoing of the sequence number.

どうやらmasterがシーケンスを払い出してslaveがそのシーケンスを使ってLSAckを返すらしい。

ではMaster/Slaveの関係自体はどうやって確立されるんだろう。

> Master/Slave
>     When the two neighbors are exchanging databases, they form a
>     master/slave relationship.  The master sends the first Database
>     Description Packet, and is the only part that is allowed to
>     retransmit.  The slave can only respond to the master's Database
>     Description Packets.  The master/slave relationship is
>     negotiated in state ExStart.

どうやらmasterが最初にDD(Database Description)を発行できたり再送できたりするらしい。シーケンスはmasterが決められる。
逆にslaveはmasterのDDに応答するだけ。

で、もしslave側のリンク状況が変化した場合はmasterは自分の持っているデータベースが古いと判断してLSRをslaveに出す
ということだろうか。

どちらがmasterになるかはDatabase Description送信前に決まっている必要があると思うのだが、
それを決定する仕組みはHelloには無いようなのでよくわからない。

```
Hello交換開始(Init)
Hello交換成功(2-way)
masterの決定(ExStart)★ここの仕組みが解らない！！！
```

さらに言うとDDは分割して送信ができるが分割が必要かどうかはMTUによる。
しかし送信側からMTUを判断するには相手のインタフェースのMTUを知る必要がある。

そしてそれは相手がmasterとなってDDを送らない限り解らない。
よって自分はslaveとなる。

相手がmasterとなってDDを送るためには。。。以下同文

なんだこの仕組みは！

### Link-State Update

Requestはあまりおもしろくないので飛ばす。
というかテキストにはほとんど説明がない。これもRFCか。。。。

ということでUpdateの定義を確認。

LSDBを作るための重要なレコードの集まりを表す。

レコードはいくつかのタイプに分かれる。

1. Router-LSA
2. Network-LSA
3. Summary-LSA(IP network)
4. SUmmary-LSR(ASBR)
5. AS-external-LSA

#### Router-LSA

OSPFルータが所有するインタフェースのうち同一エリアに属するOSPFインタフェースの数だけリンクをカウントする。

さらにそれぞれのリンクにはいくつかの種類がある。

1. P2P
2. トランジットネットワークへの接続
3. スタブネットワークへの接続
4. バーチャルリンク

さて、これらはなんだっけな。

こんなにリンクの種別いるのかな。
もっている経路渡せばいいだけでは？

P2Pはまあ解る。

トランジットネットワークはDRから発出されるらしい。
IS-ISではpseudo nodeと呼ばれるダミーノードからの経路を表す時に使う。
スタブネットワークは一般的に収容しているLANとかだろうな。

バーチャルリンクはエリアをひとつまたいで向こうのエリアへと繋がるリンクのこと。
経路を計算する時に考慮するのかな。

トランジットとなにが違うのだろう。

### Network-LSA

ネットワークに接続しているルータのIDリストを含む。
DRから配信されるネットワークの状況を知らせる。


### Summary-LSA (IP network)

0.0.0.1 -> 0.0.0.0

みたいに下位エリアのABRからバックボーンに伝達される経路とか。
ABRから広告される経路らしい。

### Summary-LSA (ASBR)

ASBRから広告される。
ネットマスクは0.0.0.0らしい。

ASBRのルータIDとそこまでのメトリックを通知する。

詳細はType 5で通知される。

### AS-external-LSA

AS外部の経路をエリア内へ教える。

- Link State ID: Network Address
- Netmask
- Forwarding Address: 外部ネットワークへと転送可能なルータのアドレス

OSPFルータがサービス回線を直接収容している場合などは、
そのインタフェースをパッシブにしておくとこのType 5で広告できるってこと？

それともその向こうにルータがいる場合にそのルータが持つ経路をredistributeするために使う？
