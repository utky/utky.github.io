------------------
title: Freeモナドを活用して問題を記述する[WIP]
date: 2016-03-20
tags: [haskell]
------------------

## ここで書くこと

Haskellでここ1年くらいずーっと悩んでいた問題が
少し解けてきている気がするので、
その問題の捉え方と解決までの歩みについて書きます。

同じことで悩む人の参考になれば幸いです。

問題とは……

**ドメイン(問題領域)のロジックがHaskellらしく書けない** ということです。

私が思う"Haskellらしい"コード
: - 純粋なコードと副作用を持つコードが 8:2 くらいの比率で書かれている
- 型によって問題が分解されて表現されている
- fugafuga

私が思う"Haskellらしくない"コード
: - 至るところに`IO`モナドが跋扈しているコード
- 関数の中にたくさんの文脈が含まれている

### こんな人に読んでほしい

『すごいHaskellたのしく学ぼう！』を読み終わったくらいの人。

- MaybeやListモナドの存在を知っている

基本的な書き方は解ったけど何かを作ろうと思うと

### キーワード

- Free Monad
- 純粋なドメイン

### 参考にした書籍, 記事

今の理解にたどり着くまでに読んだ書籍・記事の中で特に腑に落ちたものを列挙しておきます。

[Purify code using free monads](http://www.haskellforall.com/2012/07/purify-code-using-free-monads.html)
: IOまみれになりがちなコードを純粋に保つための方法を教えてくれました

[5 Ways to Test Applications that Access a Database in Haskell](http://functor.tokyo/blog/2015-11-20-testing-db-access)
: コードを純粋に保つことでどんな恩恵があるかを教えてくれました

[Scala関数型デザイン&プログラミング](http://www.amazon.co.jp/dp/4844337769) - 13.4.1 フリーモナド
: ドメインの翻訳(translation)という考え方について教えてくれました

[【型レベルWeb DSL】 Servantの紹介](http://qiita.com/lotz/items/883b41fa79f060e59efa)
: 型を使って問題を表現することの好例を知りました

## 要点

## 肥大化する関数

## レイヤの分離

## ドメインを純粋に保つ

## モノとコトとKind


## ダメだったやりかた
