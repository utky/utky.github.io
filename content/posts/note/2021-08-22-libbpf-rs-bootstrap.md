+++
title = "(WIP)libbpf-bootstrapのminimalを参考にlibbpf-rsを使う"
date = 2021-08-22
[taxonomies]
tags = ["wip","linux","ebpf"]
+++

[Building BPF applications with libbpf-bootstrap](https://nakryiko.com/posts/libbpf-bootstrap/)

上記を読むとlibbpfとその他のツールチェインを組み合わせながらどのようにしてeBPFプログラムの開発をしていくかのプラクティスがおさらいできるようになっている。
記事内では古めのLinuxカーネルでも動作するminimalとBTFの機能を駆使してよりポータブルになったbootstrapというプログラムの例が解説されている。

この例のうちminimalをlibbpfのRust版ラッパーであるlibbpf-rsで動作させるように作ってみることで、libbpf-rsでの開発プラクティスを学んでみることとする。
minimalのみに絞ったのは、仕事でちょっと古めのカーネル向けにもeBPFを活用していきたいため。より便利な機能は新しいカーネルほど実装されているが、なかなか既存のproduction環境のカーネルをむやみにアップグレードできないのでminimalの方に拘泥することにする。

[libbpf-bootstrap](https://github.com/libbpf/libbpf-bootstrap)

## minimalでやっていることのまとめ

本記事の方を見れば詳細まで分かるがざっと説明しておくと下記のようになる。

1. minimal.bfp.cをclang/llvmでeBPFオブジェクトコードにコンパイルする
2. オブジェクトコードを元にbpftoolがスケルトンを生成してオブジェクトのバイナリをスケルトンヘッダに埋め込む
3. ユーザ側プログラムがこのスケルトンをincludeしつつコンパイルされる
4. libbpfと静的リンクされて実行ファイルが作られる

それぞれのステップがlibbpf-rsではどうなるのか確認していく。

## libbpf-rsへの移植

