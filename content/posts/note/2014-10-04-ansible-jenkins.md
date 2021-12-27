+++
title = "Ansible でCIサーバを構築する"
date = 2014-10-04
aliases = ["/posts/note/2014-10-04-ansible-jenkins.html"]
+++

## Ansible でCIサーバを構築する

ansibleでjenkinsをインストールすればいい。

本当にそれだけです。

## それだけではない

重要なポイントがあります。

それは

**Dockerのコンテナを使って各ビルド+テストプロセスを分離する**

ということです。

そして

**Jenkins自体もDockerコンテナとして動作させる**

ということです。

## 必要な登場人物（コンテナ）の整理

Jenkins

Jenkins home (各Jobなどのvariableなものはこちら）

各Job用環境

## ユースケース

自分の会社とかでホストしてるGitLab的なものがあるとする。

1. gitプロジェクトを作る
2. jenkinsにそのプロジェクト用のビルドjobを作る
3. git push発生
4. web hookでJenkinsのサービスに通知
5. 更新のあったプロジェクトのjob起動
6. docker run実行 ★
7. テスト実行
8. レポートをjenkinsのworkspaceに書き込み
9. ビルド実行
10. 成果物をjenkinsのworkspaceに書き込み
11. docker run終了 exit codeがjenkinsに返りビルドの結果が判定される

ポイントは★のところです。

疑問が湧いてきます。

- A) **jenkinsはコンテナなので、ホストOS側にインストールされたdockerコマンドは使えません。**
- B) **走らせるdockerコンテナのイメージはどこからくるのでしょうか。**

## 解決

の方法はまだ考え中です（ごめんなさい）。

問題Aはcontainer -> host の方向でdocker APIが呼べれば解決かもしれないと思っています。

あー、イメージの作成時にDockerfile渡すのとかも、このAPIでいけるのでしょうか。

ということでこれからDocker APIの可能性を調査してみます。

## 追記

ほかにもいろいろ解決するべき問題があるなー。

そもそもJenkinsのworkspaceにcloneされたソースをrun対象のコンテナに転送しないといけないし。