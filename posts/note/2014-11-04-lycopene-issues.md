---
title: Lycopeneで作っていく機能について
date: 2014-11-04
---

[Lycopene](https://github.com/utky/lycopene)

うあーちなみにリポジトリ名もろかぶりしてるHerokuアプリがあった。
かえよう。

もっと短くLycoとかにしよう

## このpostで解決したいこと

何を優先して作るか。

何を重要視して作るか。

どれくらいの作業があるのか。

## 列挙

Agileにおけるバックログ（というかユーザストーリーだっけ？）

* Infrastructure
  + define global configuration
  + define preference
  + establish database session
* Application
  + Show appication version
  + provide configuration as Reader Monad
  + provide database session as Reader Monad
  + enable to signal error to contextual monad
  + transform domain model to JSON and vice-versa
* Pure Domain
  + Statistics
    - calculate velocity
  + Project
    - create
    - modify
    - delete
    - list current issues
  + Release
    - create
    - modify
    - delete
  + Issue
    - create
    - modify
    - delete
  + Record
    - create
    - modify
    - delete
* Command line interface
* RESTful interface
  + export statistics as JSON