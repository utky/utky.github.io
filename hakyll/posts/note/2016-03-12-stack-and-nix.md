---
title: stackとNixを使って安定したビルド環境を作る
date: 2016-03-12
tags: haskell, stack, nix
---

## 問題点

### stackはいいんだけれど

私はHaskellを使ったプロジェクトのビルドには[stack](https://github.com/commercialhaskell/stack)をよく使います。
`stack`を使う恩恵としてビルドが安定するのはもちろんあるのですが、
それ以外にも`cabal`を使っていた頃と比べて煩瑣なコマンドの入力が減っているので
ターミナル上の操作も随分楽になりました。(主に`cabal configure`が邪魔だと思っていた)

- [stackについての充分な説明](http://qiita.com/tanakh/items/6866d0f570d0547df026)
- [stackの使い方についての充分な説明](http://tune.hateblo.jp/entry/2015/07/13/034148)

Haskellの依存パッケージをビルドするのにこの`stack`を使うことで快適になりました。
しかしそれでもまだ時々ストレスになることがあります。

Cの呼び出しを含むようなパッケージをビルドする場合の依存関係の解決です。

### FFIを含むパッケージの依存関係

例えば[hdbc-sqlite3](https://github.com/hdbc/hdbc-sqlite3)は`sqlite3`のヘッダファイルを
インポートするようなC呼び出しのコードを含んでいます。
`stack`ではこの`sqlite3`のヘッダファイルの解決まではしてくれません。
自分で`sqlite3`をシステムにインストールするなどしてロードできるようにしてあげる必要があります。

開発環境のシステムにグローバルにインストールすることでこの依存を解決できるかもしれません。
ただし新しいインストールよってシステム上の他の依存関係が壊れることもあります。
開発目的で必要なライブラリをグローバルにインストールするのは避けたいものです。

サンドボックスを作ってそのプロジェクトに必要なCライブラリだけを
インストールできないものでしょうか。

解決策の一つに *Nixを使う*  というアプローチがあります。

## 解決方法

### Nix パッケージマネージャ

*"純粋関数型パッケージマネージャ"*と呼ばれる[Nix](http://nixos.org/nix/)を使うことで
こうした外部ライブラリへの依存周りの問題を解決することができます。
(何をもって"純粋関数型"とするかは謎ですが公式はそう自称しています)

`Nix`はOSX, Linuxディストリビューション上で利用できるパッケージマネージャです。
`Nix`では新しいパッケージをインストールしても既存のパッケージの *依存関係を上書きしない* ため、
各パッケージの依存関係が不整合になって壊れることがありません。

これはパッケージとその依存パッケージが *それぞれ分離されてインストールされている* ことによる恩恵です。
この *分離* の特徴によってHaskellプロジェクトが依存する外部ライブラリは、
システムグローバルの領域とは *分離* されてそのプロジェクト用にインストールされます。


そして何より`Nix`は`stack`と相性がよいです。
`stack v0.1.10.0`からこの`Nix`との統合機能が導入されたためです。

### Nixの導入

#### 環境情報

```
OS: Linux debian-jessie 3.16.0-4-amd64 #1 SMP Debian 3.16.7-ckt20-1+deb8u3 (2016-01-17) x86_64 GNU/Linux
Nix: 1.11.2
ghc: 7.10.3
stack: 1.0.2
cabal: 1.22.9.0
```

#### 導入手順

`Nix`と`stack`を統合するためには

[Nixのトップページ](http://nixos.org/nix/)にある「Get Nix」の手順でインストールすることができます。

Vagrantの仮想マシン`debian/jessie64`でインストールする例を示します。

#### Nixのインストール

```
$ curl https://nixos.org/nix/install | sh
```

#### Nixの提供するコマンドへとパスを通す

```
$ . /home/vagrant/.nix-profile/etc/profile.d/nix.sh
```

`bashrc`や`zshrc`など、お使いのシェルに応じて
ログイン時に上記のコマンドを実行するようにしてください。

#### 3. Haskell開発に必要な最低限のパッケージをインストール

```
$ nix-env -iA nixpkgs.ghc nixpkgs.stack nixpkgs.cabal-install
```

この時点でGHCをバイナリでインストールできるので、
`stack setup`は不要になります。

### stackとの統合

#### ビルド前の準備

例えば外部ライブラリに依存するHaskellプロジェクトである`hdbc-sqlite3`をビルドしたいとしましょう。
今までであれば使い慣れたパッケージマネージャで`sqlite3`をインストールするのですが、
ここではシステムグローバルにインストールせずにそれをやる方法を考えます。

```
git clone https://github.com/hdbc/hdbc-sqlite3.git
cd hdbc-sqlite3
```

`stack.yaml`を生成しましょう。

```
# 2016/03/12 時点では Nixのパッケージリポジトリに
# lts-5.5がないのでlts-5.4を明示します。

$ stack init --resolver lts-5.4
Using cabal packages:
- HDBC-sqlite3.cabal

Downloaded lts-5.4 build plan.    
Caching build plan
Fetched package index.                                                                                    
Populated index cache.    
Initialising configuration using resolver: lts-5.4
Writing configuration to file: stack.yaml
All done.
```

#### ビルド

とりあえず何も考えずにビルドしようとすると失敗します。

```
$ stack build
HDBC-sqlite3-2.3.3.1: configure
Configuring HDBC-sqlite3-2.3.3.1...
setup-Simple-Cabal-1.22.5.0-ghc-7.10.3: Missing dependency on a foreign
library:
* Missing C library: sqlite3
This problem can usually be solved by installing the system package that
provides this library (you may need the "-dev" version). If the library is
already installed but in a non-standard location then you can use the flags
--extra-include-dirs= and --extra-lib-dirs= to specify where it is.

--  While building package HDBC-sqlite3-2.3.3.1 using:
      /home/vagrant/.stack/setup-exe-cache/x86_64-linux/setup-Simple-Cabal-1.22.5.0-ghc-7.10.3 --builddir=.stack-work/dist/x86_64-linux/Cabal-1.22.5.0 configure --with-ghc=/home/vagrant/.nix-profile/bin/ghc --with-ghc-pkg=/home/vagrant/.nix-profile/bin/ghc-pkg --user --package-db=clear --package-db=global --package-db=/home/vagrant/.stack/snapshots/x86_64-linux/lts-5.4/7.10.3/pkgdb --package-db=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/pkgdb --libdir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/lib --bindir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/bin --datadir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/share --libexecdir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/libexec --sysconfdir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/etc --docdir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/doc/HDBC-sqlite3-2.3.3.1 --htmldir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/doc/HDBC-sqlite3-2.3.3.1 --haddockdir=/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux/lts-5.4/7.10.3/doc/HDBC-sqlite3-2.3.3.1 --dependency=HDBC=HDBC-2.4.0.1-9022dccb33fa7027f85b22fa779bd1cb --dependency=base=base-4.8.2.0-0d6d1084fbc041e1cded9228e80e264d --dependency=bytestring=bytestring-0.10.6.0-c60f4c543b22c7f7293a06ae48820437 --dependency=mtl=mtl-2.2.1-3af90341e75ee52dfc4e3143b4e5d219 --dependency=utf8-string=utf8-string-1.0.1.1-df4bc704a473da34292b0ea0e21e5412 --enable-tests --enable-benchmarks
    Process exited with code: ExitFailure 1
```

`sqlite3`という外部ライブラリが見つからないというメッセージが出力されています。
まだ`Nix`との統合が設定されていないためです。

#### stack と Nix の統合

`stack.yaml`に下記のような設定を追記することで`stack`は依存性の解決に`Nix`も使うようになります。

```
nix:
  enable: true
  packages: [ sqlite ]
```

`packages: [ sqlite ]`の部分はこのプロジェクトが依存する外部ライブラリの名前を
*スペース区切り* で列挙します。

*注意点*

cabalファイルでは`sqlite3`という名前のライブラリを参照するように宣言していますが、
これをそのまま`stack.yaml`に記載すると *パッケージ名が発見できずエラー* となってしまいます。

`Nix`のパッケージリポジトリ上は`sqlite`と定義されているためです。
`stack.yaml`に記述するパッケージ名は`Nix`パッケージシステム上で有効な`sqlite`という名前で指定する必要があります。

```
$ stack build
〜略〜
Preprocessing library HDBC-sqlite3-2.3.3.1...
[1 of 7] Compiling Database.HDBC.Sqlite3.Types ( Database/HDBC/Sqlite3/Types.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Types.o )
[2 of 7] Compiling Database.HDBC.Sqlite3.Utils ( .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Utils.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Utils.o )
[3 of 7] Compiling Database.HDBC.Sqlite3.Statement ( .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Statement.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Statement.o )
[4 of 7] Compiling Database.HDBC.Sqlite3.ConnectionImpl ( Database/HDBC/Sqlite3/ConnectionImpl.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/ConnectionImpl.o )
[5 of 7] Compiling Database.HDBC.Sqlite3.Connection ( Database/HDBC/Sqlite3/Connection.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Connection.o )
[6 of 7] Compiling Database.HDBC.Sqlite3.Consts ( .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Consts.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3/Consts.o )
[7 of 7] Compiling Database.HDBC.Sqlite3 ( Database/HDBC/Sqlite3.hs, .stack-work/dist/x86_64-linux-nix/Cabal-1.22.5.0/build/Database/HDBC/Sqlite3.o )
In-place registering HDBC-sqlite3-2.3.3.1...
HDBC-sqlite3-2.3.3.1: copy/register
Installing library in
/home/vagrant/hdbc-sqlite3/.stack-work/install/x86_64-linux-nix/lts-5.4/7.10.3/lib/x86_64-linux-ghc-7.10.3/HDBC-sqlite3-2.3.3.1-J7BRkfYWClZD8ttodrMiVd
Registering HDBC-sqlite3-2.3.3.1...
Completed 8 action(s).
```

無事コンパイルできました。

この過程で`sqlite3`をインストールしながらビルドされますが、
グローバルにはインストールされません。
このプロジェクトのビルド時にスコープを絞ってロードされます。

試しに`Nix`でシステムグローバルにインストールされたパッケージを一覧化します。

```
$ nix-env -q
cabal-install-1.22.9.0
ghc-7.10.3
nix-1.11.2
stack-1.0.2
```

明示的にコマンドでインストールしたもののみ列挙されています。
`sqlite`はあくまでもこのプロジェクト用にインストールされているのが解りました。

#### 有効なパッケージ名の検索

`Nix`のリポジトリ上でどんなパッケージが有効なのか探すための方法は、
「リポジトリをひたすら検索すること」です。

自分で書いていて「なんかここは微妙だな」と思えてきました……。

[公式ページ](https://nixos.org/wiki/Howto_find_a_package_in_NixOS#Simple_package_query)では`.bashrc`などに下記の関数を書いて、
パッケージ検索を少し楽にするといいよと言っていました。

```
nix? () {
  nix-env -qa \* -P | fgrep -i "$1"
}
```

このコマンドでは`Nix`のパッケージをリストアップして特定の文字列で絞り込んでいます。
早速`nix?`コマンドを使ってみましょう。

試しに`gcc`で検索すると…… (結構時間かかります)

```
$ nix? gcc
nixpkgs.avrgcclibc                                                      avr-gcc-libc
nixpkgs.distccMasquerade                                                distcc-masq-gcc-4.9.3
nixpkgs.gcc-arm-embedded-4_7                                            gcc-arm-embedded-4.7-2013q3-20130916
nixpkgs.gcc-arm-embedded-4_8                                            gcc-arm-embedded-4.8-2014q1-20140314
nixpkgs.gcc-arm-embedded                                                gcc-arm-embedded-4.9-2015q1-20150306
nixpkgs.gcc_debug                                                       gcc-debug-wrapper-4.9.3
nixpkgs.gcc44                                                           gcc-wrapper-4.4.7
nixpkgs.gcc45                                                           gcc-wrapper-4.5.4
nixpkgs.gcc46                                                           gcc-wrapper-4.6.4
nixpkgs.gcc48                                                           gcc-wrapper-4.8.5
nixpkgs.gcc49                                                           gcc-wrapper-4.9.3
nixpkgs.gcc                                                             gcc-wrapper-4.9.3
nixpkgs.gcc_multi                                                       gcc-wrapper-4.9.3
nixpkgs.gcc5                                                            gcc-wrapper-5.3.0
nixpkgs.gccgo48                                                         gccgo-wrapper-4.8.5
nixpkgs.gccgo                                                           gccgo49-wrapper-4.9.3
nixpkgs.xorg.gccmakedep                                                 gccmakedep-1.0.3
```

左の列は`Nix`リポジトリの名前空間上のパッケージ識別子です。
右の列はパッケージの名前です。

こうしてパッケージを名前で検索できます。
でもなんかコレジャナイ……。

パッケージの説明はこんな感じで表示できます。

```
$ nix-env -qa --description gcc-wrapper-5.3.0
gcc-wrapper-5.3.0  GNU Compiler Collection, version 5.3.0 (wrapper script)
```

`Nix`のコマンドがよく解らないという方は下記のチートシートが参考になるかもしれません。

[Nix Cheatsheet](https://nixos.org/wiki/Cheatsheet)

## まとめ

`stack`を使って外部ライブラリに依存するプロジェクトをビルドする場合は、
`Nix`と統合することでシステム全体にインストールされたパッケージを破壊するリスクをなくすことができます。

`stack`と`Nix`の統合はとても簡単ですが、
`Nix`上での適切なパッケージ名を知らないと正しく依存関係を解決できません。

*参考記事*

[Stack + Nix = portable reproducible builds](http://www.tweag.io/blog/stack-nix-portable-reproducible-builds)

