+++
title = "OpenDaylight Tokyo User Group Kickoff Meetup"
date = 2015-01-24
aliases = ["/posts/note/2015-01-24-opendaylight-tokyo-user-group-kickoff-meetup.html"]
[taxonomies]
tags = ["sdn","tech"]
+++

## 第一回 ODL Tokyo User Group

OSSのSDNプラットフォームOpendDaylightのユーザグループ会。
っていうか第一回なのね。

### Time Table

* 1:30-1:40　オープニング 
* 1:40-2:00　Heliumプロジェクトご紹介 
* 2:00-2:30　MD-SAL技術紹介 
* 2:30-2:40　DellのOpenDaylightへの取組み 
* 2:40-3:00　沖縄オープンラボ　ODL PoCご紹介

## 参加の目的

* ODLはどういった問題を解決するものなのかを知る。
* 導入までの実例を知る。
* エッセンシャルな機能を知る。
* 逆に要らない、削って良さそうな機能を見つける。

## オープニング

リチウムっていうのが今年の6月くらいに出るらしい。

### Neela Jacquesによるプロジェクト推進の鍵

* コミュニティ
* コード
* ユーザアクセプタンス（現実のユーザの問題を解決する）

* Group Based Policy
* Network Intent
* Congress
 
## Heliumプロジェクトご紹介 (田井秀幸さん)

Heliumっていうのは全体のどこの部分なんだろうな。

5ヶ月でHeliumを開発しているのだな。

誰でも機能の提案ができる。技術委員会が審査してマイルストンとして取り込まれる。

SALはいわば「カーネル」の位置づけらしい。

トリプルA機能ってなんだ。
=> Authentication, Authorization, Accouting

[資料](https://drive.google.com/file/d/0B1KtwIIbDsZXSDdpZjFQWDJUWXc/edit)

L2SWプロジェクト

OpenFlowネットワークをL2スイッチとして運用できる。

Service Function Chaining

    Firewall -> DPI -> NAPT44

など、フローに対してどんなサービスをどの順で適用するか。

Helium4か月も脆弱性があったのに放置されてきた。
CIにともなうセキュリティテストはもしかしたら、contributeできるかも。

AD−SALからMD-SALへの移行。

ODL Forege

新機能の開発を気軽にためせる。インフラを利用できるので、チャレンジしやすい。
2月上旬に公開されるらしい。

## MD-SAL技術紹介 

Yangで構成されるJavaクラスは結局nullableだったり、いろいろだめやな。

なんかYangはいいのかもしれないけれど、Javaなのが良くない気がしている。

機能が必要以上に冗長に思える。

## DellのOpenDaylightへの取組み （鈴木様）

LACPインタフェース
時系列データリポジトリ - influxDB でいいのでは？
2ノードクラスタリング開発

VNFがHeliumだとServiceFunctionと呼ぶ

標準化されていないヘッダを使って（NSHヘッダ）VNFと通信しSFCを適用する。


## 沖縄オープンラボ　ODL PoCご紹介

OpenStack
OfficeのOFS
Proprietary Appliance

といった違う

LLDPによるトポロジ検知ができなかった。
こういう場合のトラブルシューティングってどうやってやるのだろう。

## Hands On

Karaf以外にもONOFというOSGi系プラットフォームがよくSDN界隈で使われる。
依存関係を分離してクラスローダを綺麗に保つのだろうなー。

Karafでは管理下のルータへのトランスポートやシェルをサポートするかもしれない。いまはできない。

ログを参照する場合はkarafのタスクとして実行する。

featureには相互共存が難しい機能もある。
たとえばL2 SwitchやVTN Managerは同時に起動することができない。

featureのcompatibilityをよく知らなければ使えない。

## 個人的まとめ

* ODLはどういった問題を解決するものなのかを知る。
SDNコントローラの環境とプラグイン用のAPIを提供するフレームワーク。
そしてまたそのAPIがいけてないのなんの。
* 導入までの実例を知る。
全然商用に耐えない状態。
AAAは開発中だし、UIは壊れているし、ロギングは未整備。まだ基盤的ができあがっただけで、
* エッセンシャルな機能を知る。
MD−SALがODLと基盤となるようだ。
しかし、MD-SAL自体のコンポーネント構成が複雑であり、拡張や開発のためのboilerplateが多いイメージ。
* 逆に要らない、削って良さそうな機能を見つける。


## 会としての評価

1. 時間のなさがすごい
2. ハンズオンは手順を流すまでの足並みもそろわず、まるで新人研修を思い出した。（すいませーん、動かないんですけど的な）

## note

どういう層というか分野の人たちが集まっているのだろう。
ネットワークプログラマビリティに参加している人たちもいるような気配。
次こそ行かなくては。

