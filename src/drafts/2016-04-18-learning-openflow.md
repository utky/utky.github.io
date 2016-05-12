------------------
title: OpenFlow再入門[W.I.P]
date: 2016-04-18
tags: [network, openflow]
------------------

## 動機

自分のいる職場ではOpenFlowよりもほかのアーキテクチャの方が
スケーラブルだという判断もあってあまりOpenFlowを使ってこなかった。

しかし業界全体としてはまだまだOpenFlowで何かできると考えている人は多いし、
使わないだろうなという判断は早計過ぎると思うようになった。

せっかくそう気づいたタイミングなのでOpenFlowについて実践を交えながら学習をしたい。

## ゴール

- OpenFlowを使った時の難しさを体感する
- OAMのアプローチを考えるため素材を揃える

## 条件

下記を達成する。

- OpenFlowメッセージコーデック作成
  - データ構造からプロトコルの機能を理解する
- Mininetを使ってシミュレーションする
  - Hop by Hop方式
  - Overlay方式
  - 障害検知のシナリオ
  - 経路迂回のシナリオ

## 計画

- Haskell: ADT作成
- Haskell: Codec作成
- Haskell: Domainの記述
- Haskell: 操作的意味論

## 抽象データ型の作成

ひとまずごく基本的なHelloやEchoRequest/Replyを作って感触をつかむ。

## Codec作成

Aesonを参考にしながら作ることとする。Aesonでは性能対策のためにさまざまな工夫をしているようでまだ何をやっているかは解らない。

### Decode

attoparsecを使って実現する

### Encode

bytestringに入っているBuilderを使う

## memo

- paddingは0である必要はない
