---
title: "OpenContrail Meetup"
date: 2015-09-25
aliases: ["/posts/note/2015-09-25-opencontrail-meetup.html"]

tags: ["sdn"]
---

## Zabbix w/ Contrail

ContrailもHypermedia APIを持っていて
リソースをトラバースできるようになっているらしい。

ContrailのAPIよりAPI経由でVMのリソース状態を取得できる。
（え、VMも管理下におけるんだっけ？）

あ、Keystoneとか言っているからOpenStackの連携が話の前提のようだ。

Pythonスクリプトを使ってContrailへBulkリクエストを発行し、データまとめてsenderにおくる。
→ これってPythonスクリプトを常駐させる？

## Cannonical

Ubuntu w/ Contrail

Amazon EC2のインスタンスだとUbuntuがトップらしい。
OpenStackをのせるOSとしてはUbuntuがトップとか。
なんかもうバイアスあるんじゃないかっていうくらいの覇道っぷりだな。


OIL (Openstack Interoperability Lab)
相互接続をするためのテストベッドラボをボストンに構築している。

Juju(provisioning front end) over MaaS (<-backend service provider)

ここでホストの環境を作り上げて、
その後にContrailで仮想レイヤを作り上げるらしい。

ポイントになってくるのはユーザの要求に対して反応的にデプロイメントを行うこと。
ユーザの要求にAwareな仕組みってあるのだろうか？

Auto Pilot?

OpenStackのUpgradeは手動だと大変。VMWareではそれがサポートされているのでよい。
Auto PilotではOpenStackのその方面をサポートする予定らしい。

## Juniper 中島さん

Config NodeはNeutron ServerからAPIコールされるらしい。

ContrailはActive-Activeモデル。
データの整合性とかどうするんだろう？

haproxy使って冗長系を組んでいるらしい？

Control Nodeを多重化して冗長をとっていても、
Contrail内部でイベントがバーストした場合にデータの整合がとれるのか？

SNMP Collectorが収集したデータをZookeeperにどんどん突っ込むらしい。
Zookeeperのパワー、何気に凄いのでは?

### ToR Service Node

OVSDBに対応する集約スイッチ
ToRスイッチのことで、これを制御するのがTSN。

### Ansibleでプロビジョニング

ChefでなくPuppetでなくAnsible

Ansibleはターゲットにpythonの実行環境が必要だったと思うが、
Juniperルータを構成する場合はどうするのか？

Chefだとノードから要求してプロビジョニングを実行できるが、
Ansibleではそういう機構がないはず？

Vif Driver?


OpenStackとContrailを安定して相互接続するのはこれからの課題。
OpenStackのバージョンが変わるだけで連携が死んだりする。


