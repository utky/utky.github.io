---
title = "メインPCをArchLinuxからWindowsに移行する"
date = 2020-05-31
aliases = ["/posts/note/2020-05-31-migrate-from-archlinux-to-windows.html"]

tags = ["windows","linux"]
---

いつも環境移行する時に当時の動機とかを忘れるので記録しておく。

## やったこと

作業環境をArchLinuxラップトップからWindowsデスクトップに移行した。
これまでの環境をVMの中に構築した。

特に目立って変わったことはやってないので、Windowsで開発している人は大体こんな感じだと思われる。 
想像していたよりもつっかえる部分が少なくて良い環境だな、と思えるようになってきた。

## 背景

あんまりない。ArchLinuxそれ自体にはそんなに不満ない。

一方でDAWが入っているデスクトップが有効活用できていなかったのでもやもやしていた。

エディタをVSCodeに移行したことにより、Remote Developmentが使えるようになってVM上で
十分不満なく開発できそうな気運が高まった。
今までArchLinux上に作っていた環境をVM上に再現すればよくなったので、
Windowsを使うことに障壁がなくなった。

## ArchLinuxの良い所

* 1) 無駄なものがなくて必要なものだけインストールできる
* 2) サーバサイド開発の道具立てがそのまま使える
* 3) ローリングリリースなのでメジャーバージョンのアップグレード作業が要らない

この辺がWindowsでも再現できれば問題ないことになる。

## 移行してみた結果

VSCodeのおかけですごく良い感じに使えている。
ArchLinuxで感じていたメリットは結局VM上にそれを再現することで得られているので、
ありがとうVirtualBoxというだけの話だった。

### 1) 無駄なものがなくて必要なものだけインストールできる

Windows自体には無駄なものがある。。。
こればっかりはどうしようもなかった。
せめてVMくらいは最小限の環境を作るようにする。

### 2) サーバサイド開発の道具立てがそのまま使える

これもVM上での作業を中心にすることで解消した。
Windows上で直接開発する気はさらさらない。

### 3) ローリングリリースなのでメジャーバージョンのアップグレード作業が要らない

まあ、Windows10もローリングリリースだし。
ローリングリリースしているLinux DistributionをVMに採用すればそれで良いのだった。

## Windows環境の構成

開発関連の作業は専用VM上で行うようにする。

* VM管理: VirtualBox
* エディタ環境: VSCode Remote Development
* ターミナル: Windows Terminal

### VirtualBox

[VirtualBox](https://www.virtualbox.org) を [Vagrant](https://www.vagrantup.com/) と組み合わせて使っている。WSL w/ Hyper-V よりは慣れた道具で作れる方を選んだ。

VirtualBox 6.1.8 + Vagrant 2.2.9 だとVM起動後のSSH接続でタイムアウトしてしまう問題があったので、
VirtualBox 6.0.22 を使ったら解消した。

イメージはUbuntu 20にしてみたけど、今考えたらArchLinuxにすればよかった。。。
新しいものが使いたくて脳死でUbuntu 20入れてしまった。
あとでArchLinuxで再構築したい。

### VSCode

これがあったからWindows移行する気持ちになった。
こんな良いエディタが無料で使えるとはどういうこと。。。

#### Vim Extension

そんなに使い込めてるわけじゃないけどVimキーバインドが欲しくて使っている。

#### IME の制御

[zenhan](https://github.com/iuchim/zenhan)

こちらにお世話になった。

```json
    "vim.autoSwitchInputMethod.obtainIMCmd": "C:\\\\Users\\\\xxx\\\\.local\\\\bin\\\\zenhan.exe",
    "vim.autoSwitchInputMethod.switchIMCmd": "C:\\\\Users\\\\xxx\\\\.local\\\\bin\\\\zenhan.exe {im}",
    "vim.autoSwitchInputMethod.defaultIM": "0",
    "vim.autoSwitchInputMethod.enable": true,
```

下記の選択肢もあったけど、キーボードの配列をスイッチする動作になってしまいちょっと使い心地が悪かったので使わなかった。

[im-select](https://github.com/daipeihust/im-select)

## Windows Terminal

開発用VMにSSHでつなぐために使っている。
(VSCodeのterminal機能でもよさそうだけど)

今まで使ってたLinux環境でのterminalとそんなにギャップなく使えるのですごく良い。