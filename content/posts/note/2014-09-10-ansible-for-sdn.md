+++
title = "AnsibleでSDNコントローラ開発環境をセットアップする"
date = 2014-09-10
aliases = ["/posts/note/2014-09-10-ansible-for-sdn.html"]
+++

## AnsibleでSDNコントローラ開発環境をセットアップする

ansibleならターゲットノードにchef-clientみたいなの入れなくても、
さっさと開発用サーバを準備できるかなと思い導入してみました。

https://github.com/utky/ansible-sdn

### やりたいこと

Mininetが入った環境がすぐほしい

同じホスト内で *今* 開発しているコードをOpenFlowコントローラとしてデプロイしたい

できればTest Suiteとしてmininetのnetwork構成もバンドルして自動で動かしたい

### OpenFLowコントローラのプラットフォーム

Java7

Scala2.10

Play2.2

で動かしたい。

とりあえずplay以外はインストできてるとこまで確認。

### 課題

playがなんかうまくインストルされてない？

あれ、今のmininetとopenvswitchってopenflow1.3対応しているんだっけ？
（確認できていない）

一応OpenDaylightのチュートリアルに従って、mininetのnode.pyにpatch当てることで、
openflow1.3に対応できるはずなんだが。