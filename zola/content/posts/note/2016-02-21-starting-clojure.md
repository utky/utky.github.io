+++
title = "Starting clojure"
date = 2016-02-21
aliases = ["/posts/note/2016-02-21-starting-clojure.html"]
[taxonomies]
tags = ["clojure"]
+++

Clojureを始める
---------

最近『プログラミング Clojure』を買った。本格的なClojureの学習が始まった。
Lisp方言は今までSchemeを少し書いたくらいで、まだ馴染むという雰囲気はなかった。

でも重い腰を上げてLisp方言であるClojureを書けるように学習することにした。

Python讃歌
---------

今の職場ではJavaScriptとPythonをメインで使っている。

JSはフロント作る都合場、必要に迫られてというスタンスだけれど、
Pythonは割と進んで使いたくなる程度には素敵な言語だと思っていた。

PythonはJavaよりも簡潔で適当に書いても動く。
標準ライブラリがとにかく充実しているし、ネットワーク系ライブラリもよくできたものが多い。

特に以下のようなライブラリは数々のネットワーク機器を操作するツール作りにとても役立った。

- [paramiko](http://www.paramiko.org/)
- [ncclient](http://ncclient.readthedocs.org/en/latest/)
- [SQLAlchemy](http://www.sqlalchemy.org/)
- [nosetests](https://nose.readthedocs.org/en/latest/)

※ 意外とDjangoは使っていないというか、そんなフルスタックは必要なさげ

### Pythonの好きなところ

1. ライトな関数型プログラミングをすぐ導入できる
2. dictがあるのでjsonデータをそのまま雑に加工できる
3. だいたい何でも標準ライブラリに入ってる

### Pythonの嫌いなところ

1. selfが冗長
2. 標準的なデータがmutable
3. 一度2.x系を使いだすと3.x系へのキャズムをなかなか超えられない（あんまり困らないけど）

Pythonはとても好きです。
好きなのですがなんだかすごく甘やかされているような気分にもなるのです。
いたれりつくせりというか。

一方、自分の書くコードが徐々に「手続き」の記述から「式」の記述へと移行してきました。
式ばっかり書いているのであれば、いっそLisp系方言の方がブレなくていいかなと思いました。

Haskellが一番いいけどまだ職場に投入する勇気がありませんでした。
（だからと言ってLispならOKという理屈や根拠はやはり無いですけど）

ということでJVMで動くLisp方言Clojureを使い始めてみようと思うようになりました。

これからのClojureとの付き合いかた
---------

なんといっても業務で使えなくてはなりません。
最低限の条件は下記でしょう。

1. JSON APIサーバがすぐ作れる
2. 安定したSSHクライアントライブラリがある

Java資産も使えることもあってあまり困らなそうですがまだまだ安心できません。

いずれNETCONFのAPIサーバを書ければいいな、という希望を胸にClojureを始めます。

手始めに勉強したいところ。

1. deftypeとdefrecordの使い分け
2. マクロの使いどころ
3. 並行処理と並列処理の使い分け
4. Haskellのようなデータ駆動なDSLの作り方

フロントエンドも
-----------

ClojureScriptいいですね。
omよさげ。

Reactで培った思想を素直に活かせそう。
そしてHaskellへと凱旋しElmの道へ。。。。。

とか夢みている。
