---
title: AWSで実施する負荷試験ツール JMeter と locust
date: 2020-03-29
tags: aws, testing
---

お仕事でAWS環境での負荷試験を行っていたので、負荷生成に使ったツールと構成方法について説明する。
使ったツールは下記の2つ。

1. JMeter
2. locust

分散ノードでの負荷生成を行い大体数十万rpsくらいの負荷を作ることを意図しています。

## 分散構成の違い

JMeter

masterがslaveにRMIで接続し負荷テストシナリオを注入することでテストを起動する。
masterよりも先にslaveが起動していなければならない。

locust

slaveがmasterに接続しテストをsubscribeする。
slaveよりも先にmasterが起動していなければならない。
