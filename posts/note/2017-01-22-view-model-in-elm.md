---
title: Elmの設計で悩むところメモ
date: 2017-01-22
tags: programming, elm
---

View Modelのあるべき姿が解らなくなってきたので整理。

## View Modelとは何か

何なのだろう。Viewの状態を表すものだと考えるとする。

つまりViewに依存したデータモデルということになる。
はじめからViewありきのデータ構造だということだ。

ということはViewと密結合していてむしろ良いわけだ。
Viewが変わればView Modelも変わる。そういうものだと考える。

## いままで

Viewの構造とは切り離された、もっとPureなドメインを
View Modelで表現するのだと思っていた。

でもこれはいずれ、検討を進めるに従って違和感を生んだ。

まずリモートサーバには既に蒸留されて丁寧に整理されたドメインがあるということ。
これと同じ質のデータをブラウザ側で保持することには何の意味もない。
だからブラウザ側が持つView Modelはリモートのpureなドメインと比べて、
何らかの付加情報がなければいけない。

## View Modelに付加されるもの

付加情報とはViewの状態だ。

ユーザとのインタラクションを保存する。
バッファみたいなものだ。ユーザの意志とか意図をView Modelに溜めてあげる。
ユーザの短期記憶の代わりとなってあげる。
今までの操作の過程を視覚化してユーザにフィードバックする。
ユーザがそれを受けて次のアクションを想起する。

そんな感じだ。ユーザに寄り添うモデルであるべきだ。

その一例は

_ユーザがチェックを入れた選択肢_

とか

_ユーザが捨てると決めてゴミ箱に入れたチケット_

とかそういうものが状態としてView Modelに保存される。

これはリモートのドメインからすれば過渡的な状態に過ぎない。
コミットしていない操作だから永続化する前の状態だ。
だからリモートのドメインはこのような半端な状態について感知する必要がない。

View Modelはそういう過渡的な状態を保存するのだろう。
だとすると、View Modelはどのように決定されるのか。

Viewの設計が前提としてそこから導かれるだろう。そのはず。

## 結論

まずはViewの設計をしよう。

そしてViewの設計とはどうするのだろう。

UI/UXの分野は逃げてきたのでからっきしだ。まさかElmを触っているうちにUI/UXから逆襲くらうことになるとは。

## 懸念

Viewの設計をおこなうにはユーザの中にあるメンタルモデルの整理とか、
ユーザがデータを捉える視座の学習とかが必要になる。

これってドメインモデリングと何が違うのだろう。

みたいな謎が浮かぶ。

単純なスコープの違いなのだろうか？

ちょっとUI/UXの学習をしないと見えてこなさそうだ。
