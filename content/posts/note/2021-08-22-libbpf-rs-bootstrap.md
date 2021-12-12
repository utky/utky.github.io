+++
title = "libbpf-rsを使ってeBPFプログラミングに入門する"
date = 2021-08-22
[taxonomies]
tags = ["wip","linux","ebpf"]
+++

## はじめに

この記事ではeBPFを活用してLinuxカーネルにフック用プログラムを注入することにより、ネットワークパケット処理を拡張する例を示します。またその実装にあたり、Rustとlibbpfの統合を行うlibbpf-rsを使った開発体験を記したいと思います。

## eBPF
TODO:
カーネル内部のイベントをトリガとして呼び出されるプログラムをユーザ空間から注入できる機能です。
専用のバイトコードをカーネル内の仮想マシンに解釈させることで命令を実行します。


eBPFの拡張性を活用したCiliumがGKEの新たなコンテナネットワーキングの実装に選ばれた実績を見ると、それなりに注目度が高いことが伺えます。

[コンテナのセキュリティと可視性が強化された GKE Dataplane V2 が登場](https://cloud.google.com/blog/ja/products/containers-kubernetes/bringing-ebpf-and-cilium-to-google-kubernetes-engine)

## TL;DR

最初の準備がめんどくさい。（いろいろ環境をつなぎあわせるのがね。。。)
いっかいビルドできるようになると後は部品を足すだけなのでスケールする。
とにかく最初の準備がめんどくさい。
なのでlibbpf-bootstrapなどを参考にある程度決まった型に慣れておく必要がある。

## 課題設定

サーバ上のTCP 8080ポートに対する通信をTCP 8081ポートでlistenしているプロセスへとリダイレクトします。
とても現実にありえる正気のユースケースとは思えないが、発展させるとロードバランシングになる。

## eBPFプログラミングの登場人物

eBPFプログラムはカーネル空間側で実行されますが、実用上はそのeBPFプログラムをカーネルにロードしたり、実行結果を取り出すためにユーザ空間側で動作するプログラムも必要となります。

- clang
- llvm
- libbpf
- libbpfの依存関係
- vmlinux.h
- eBPFプログラム (カーネル側)
- Rustプログラム (ユーザ側)

これらの登場人物をうまく統合するための道具として真っ先に思いつくのはmakeですが、cargoでも[ビルドスクリプトを便利に書けそう](https://doc.rust-lang.org/cargo/reference/build-scripts.html)なのでできるだけcargoで頑張ってみましょう。

というかこの記事の核心はこれらのツールを統合するビルドスクリプトかもしれません。

## libbpf-rs

https://docs.rs/libbpf-rs/0.15.0/libbpf_rs/#libbpf-rs

## eBPFプログラミング環境のセットアップ

[Building BPF applications with libbpf-bootstrap](https://nakryiko.com/posts/libbpf-bootstrap/)


libbpfの依存関係をインストールしておく
https://github.com/libbpf/libbpf-sys#building


### libbpf (いらないかも)

RustからeBPF関連のシステムコールを便利に呼び出すためのAPIが揃ったlibbpfをリンクできるようにしておく。

```
git submodule add https://github.com/libbpf/libbpf.git
git submodule update --init --recursive 
```


### Cargo.tomlの追記
### `src/bpf` を作る
```
mkdir src/bpf/
```

gitignore
```
src/bpf/*.rs
```

`.bpf.c` ファイルを作る
```
touch src/bpf/tcpconnect.bpf.c
```
ビルドする
```
cargo libbpf make
```

### cargo-libbpf
cargo-libbpfはeBPFプログラムのコンパイルやRustへのインテグレーションをおこなうワークフローを自動化するためのcargo用ライブラリです。
https://docs.rs/libbpf-cargo/latest/libbpf_cargo/#build

上記にあるように
```
cargo libbpf make
```
などでcargoコマンド経由でclangを呼び出すことができます。
しかし普通にRustプログラムを書いていてcargo libbpf makeも忘れずに実行するのは普通にめんどくさいです。

幸いにしてビルドスクリプトから呼び出すことができるAPIがcargo-libbpfにもあるのでこれを使ってcargo build時に合わせてeBPFプログラムもビルドできるようにします。

公式にも下記のようにあるので、むしろそちらが正攻法のようです。

> The build script interface is recommended over the cargo subcommand interface because:

https://docs.rs/libbpf-cargo/latest/libbpf_cargo/#

### ビルドスクリプトを用意する


#/bin/sh

$(dirname "$0")/bpftool btf dump file ${1:-/sys/kernel/btf/vmlinux} format c

ヘッダを作る


成果物を確認する
```
 % ls -l target/bpf/tcpconnect.bpf.o
-rw-r--r-- 1 ilyaletre ilyaletre 488 Dec 10 10:17 target/bpf/tcpconnect.bpf.o
10:18:12 ilyaletre@workspace:~/projects/bpf-tools-rs *[main] 
 % file target/bpf/tcpconnect.bpf.o 
target/bpf/tcpconnect.bpf.o: ELF 64-bit LSB relocatable, eBPF, version 1 (SYSV), not stripped
```

https://github.com/iovisor/bcc/blob/99bfe8ac0b3f5d0422e47e09abc073425dc22968/tools/tcpconnect.py#L86-L204
をパクッてつくる

### カーネル空間: eBPFプログラムを書く

今回はパケット操作なので `__sk_buff` を操作するのに必要な操作のみに注目します。

https://github.com/libbpf/libbpf/blob/master/src/bpf_helpers.h
https://github.com/libbpf/libbpf/blob/master/src/bpf_helper_defs.h

`bpf_skb_` で始まるヘルパ関数を利用することができます。

https://www.youtube.com/watch?v=ZNtVedFsD-k&t=240s

### ユーザ空間: eBPFプログラムをロードする

## minimalでやっていることのまとめ

本記事の方を見れば詳細まで分かるがざっと説明しておくと下記のようになる。

1. minimal.bfp.cをclang/llvmでeBPFオブジェクトコードにコンパイルする
2. オブジェクトコードを元にbpftoolがスケルトンを生成してオブジェクトのバイナリをスケルトンヘッダに埋め込む
3. ユーザ側プログラムがこのスケルトンをincludeしつつコンパイルされる
4. libbpfと静的リンクされて実行ファイルが作られる

それぞれのステップがlibbpf-rsではどうなるのか確認していく。

## いろいろな課題
### テスト
verifierはあるのでカーネルがクラッシュするようなコードは弾かれるのですが、そうはいっても正しく動くかのテストはしたくなります。
特にeBPFでカバーする層が厚くなってくればなおさらだし、カーネルで動作させたい程度には重要なプログラムではあるはずなので、あまりtest suiteが提供されていないのにはもやっとしています。

Ciliumではユニットテスト環境をコンテナで作ってモックをはさみながらテストしているようです。

https://github.com/cilium/cilium/pull/16862

eBPFプログラムはイベントを受け取ってmapの状態を書き換える状態機械(ミーリ・マシン)とみなすことができそうなので、そういうtest suiteがあればいいような気もしなくもない。

### 普通にセットアップが面倒くさい
pure RustをうたったeBPFライブラリAyaというのがあるので、そっちをもう少し試してみるのが良いのかもしれない。
ちょうどタイムリーに記事が出てました [RustでeBPFを操れるayaを触ってみた](https://qiita.com/moriai/items/c96636d35ae39809b45e)
読んだ感じだとプロジェクトテンプレートでboilerplateを自分で書く手間を削減しているっぽいですね。
