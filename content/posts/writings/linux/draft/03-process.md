---
title = "Process"
date = 2017-12-24
aliases = ["/posts/linux/03-process.html"]
---

プロセスディスクリプタとは, `task_struct`, のこと

PDは, `thread_info`, とスタック情報をもった2ページ(8KB)の領域へのポインタを持っている。
espレジスタはこのスタックの現在位置を常に保持している。
`thread_info`, をスタックが決まったレイアウトになっていることにより、
espのアドレスにマスクをかけることでPDへのアドレスを参照することができるようになっている。

よくできているなー。

###, スレッドグループ

*, リーダ
, , , , +, pid, a
, , , , +, tgid, a
*, フォロワー
, , , , +, pid, b
, , , , +, tgid, a

のようにtgid, (thread, group, id)
には最初のスレッドであるリーダーのPIDが入るようになっている。

`ps, -eLf`, うつとこんな感じ

```
ilyalet+, , 2080, , , , , 1, , 2080, , 0, , , , 1, Oct07, ?, , , , , , , , 00:00:00, /usr/bin/zsh
ilyalet+, , 2181, , 2080, , 2181, 38, , , 61, Oct07, ?, , , , , , , , 08:45:57, /usr/lib/firefox/firefox
ilyalet+, , 2181, , 2080, , 2210, , 0, , , 61, Oct07, ?, , , , , , , , 00:00:00, /usr/lib/firefox/firefox
ilyalet+, , 2181, , 2080, , 2211, , 0, , , 61, Oct07, ?, , , , , , , , 00:07:08, /usr/lib/firefox/firefox
ilyalet+, , 2181, , 2080, , 2212, , 0, , , 61, Oct07, ?, , , , , , , , 00:00:00, /usr/lib/firefox/firefox
```

PID, PPID, LWP
の順でIDが並ぶ。

firefoxとしてのPIDが2181
zshからforkしているもとのPPIDが2080そして
firefox内のスレッドが2181や2210といった軽量プロセスの番号になっている。
一番上のPIDがstimeが長いスレッドのリーダらしい。
これはPIDとLWPのIDが同じことからも推察できる。

###, Task, struct

include/linux/sched.h
に定義されている
