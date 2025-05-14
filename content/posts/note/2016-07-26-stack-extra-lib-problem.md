---
title = "Stackと外部ライブラリの問題"
date = 2016-07-26
aliases = ["/posts/note/2016-07-26-stack-extra-lib-problem.html"]

tags = ["haskell"]
---

Haskellのビルドユーティリティであるstackを使っているけど、
気持ちの萎える問題に直面している。
まだ解決はみていない。

## 問題

ghcが外部ライブラリを読み込めないケースがある。
事象としてはこんな感じ。

```
$ stack build
lycopene-0.1.0.0: build
Preprocessing library lycopene-0.1.0.0...
[19 of 24] Compiling Lycopene.Database.Schema ( src/Lycopene/Database/Schema.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.2.0/build/Lycopene/Database/Schema.o )
<command line>: can't load .so/.DLL for: libsqlite3.so (libsqlite3.so: cannot open shared object file: No such file or directory)

--  While building package lycopene-0.1.0.0 using:
      /home/ilyaletre/.stack/setup-exe-cache/x86_64-linux-nix/setup-Simple-Cabal-1.22.2.0-ghc-7.10.1 --builddir=.stack-work/dist/x86_64-linux-nix/Cabal-1.22.2.0 build lib:lycopene exe:lyco --ghc-options " -ddump-hi -ddump-to-file"
    Process exited with code: ExitFailure 1
```

`libsqlite3.so`をロードできないといっている。
`stack ghci`でも同様のメッセージがでる。

```
$ stack ghci
lycopene-0.1.0.0: build
Preprocessing library lycopene-0.1.0.0...
[19 of 24] Compiling Lycopene.Database.Schema ( src/Lycopene/Database/Schema.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.2.0/build/Lycopene/Database/Schema.o )
<command line>: can't load .so/.DLL for: libsqlite3.so (libsqlite3.so: cannot open shared object file: No such file or directory)

--  While building package lycopene-0.1.0.0 using:
      /home/ilyaletre/.stack/setup-exe-cache/x86_64-linux-nix/setup-Simple-Cabal-1.22.2.0-ghc-7.10.1 --builddir=.stack-work/dist/x86_64-linux-nix/Cabal-1.22.2.0 build lib:lycopene exe:lyco --ghc-options " -ddump-hi -ddump-to-file"
    Process exited with code: ExitFailure 1
Warning: build failed, but optimistically launching GHCi anyway
Using main module: 1. Package `lycopene' component exe:lyco with main-is file: /home/ilyaletre/workspace/haskell/lycopene/main/Main.hs
Configuring GHCi with the following packages: lycopene
GHCi, version 7.10.1: http://www.haskell.org/ghc/  :? for help
<command line>: can't load .so/.DLL for: libsqlite3.so (libsqlite3.so: cannot open shared object file: No such file or directory)
```

下記の条件を満たすと発生することが解っている。

```
(プロジェクトコードや依存ライブラリが外部ライブラリに依存している)
AND
(
  (Template Haskellで外部ライブラリを呼び出すコードがある && `stack build`を行う)
  OR
  (`stack ghci`を実行する)
)
```

今回自分が書いていたプロジェクトであれば、
[haskell-relational-record](https://github.com/khibino/haskell-relational-record)の実行に
[HDBC-sqlite3](https://hackage.haskell.org/package/HDBC-sqlite3)を利用しており、
これが外部ライブラリの依存(`libsqlite3.so`)にあたる。

直接的な原因は下記のghcの問題だと考えている。

[Template Haskell / GHCi does not respect extra-lib-dirs](https://ghc.haskell.org/trac/ghc/ticket/11042)

関連するチケットは下記のようだ。

[GHCi fails to load shared object (the 'impossible' happened)](https://ghc.haskell.org/trac/ghc/ticket/10458)

後者のチケットの議論をまだ追えてないので解らないけれど、
少なくとも前者はまだクローズされていない。

## アクション

いまのプロジェクトを進めるためできることはなにか。

1. いったんsqlite依存を切り離し別のバックエンドを考える(JSONとか？)
2. 別のバックエンドから後でsqlite3に戻れるだけの抽象化の壁を作っておく
3. バックエンドをCDBにするとか？[pure-cdb](https://hackage.haskell.org/package/pure-cdb)

うーん、またこれで進まなくはなるけど、実装とインタフェースの分離は
haskell書いててもいろいろ設計上の課題を自分なりに抱えているのでいい練習になるかな。

急がば廻れ。

**追記**

ちょっと考えたけどやっぱりやりたいことを実現するにはsqliteがほしいので、
`stack build`だけは少なくとも通るように`sqlite + Template Haskell`の条件を回避することとする。
まずはHDBCで生のSQLを書いて対応する。
