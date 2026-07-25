---
title: "AIフレンドリーなObsidian環境を考える"
date: "2026-07-11"
tags: [obsidian, ai, claude, git]
---

Obsidianを使っているのにClaudeの支援を受けられる環境ができていなくてつらい。

# 課題
ObsidianはPCとスマホそれぞれから起動できる。
現在のメイン端末はスマホでありdaily noteに書いたり、Claudeから出力されたMarkdownを移動してnote化したりしている。

スマホとPC両方でClaudeが直接Obsidian vaultを編集する環境を作りたい。

# 解決方法
Obsidian vaultをGitリポジトリ化してClaude Codeから操作させる。

## リポジトリの構成
Claudeのハーネス類を置くだけでなく、 `System/sync/sync.sh` にスクリプトも配置する。
AndroidからもGitリポジトリのローカル変更を同期するための手順をスクリプト化してある。

```
CLAUDE.md
.claude/
  skills/
System/
  sync/
    sync.sh -- ローカル変更をcommit & git pull --rebase & pushするスクリプト
*.md
```

## 部分問題: PCのGit同期
ObsidianのコミュニティプラグインでObsidian Gitというのがあるのでそれを使う。
自動的にcommit & pushしてくれるのでひとまずこれでいい。

## 部分問題: スマホのGit同期
スマホ版のObsidian GitはJSで実装されたGitが動作するらしく怪しいので使うのをやめる。

よって事例の多いTermux + Gitで構成することにした。
TermuxはAndroid上で動作するターミナル。これのために入れた。
Googleのストアのものは古いらしく、F-Droidというパッケージ管理アプリ経由で入れる必要がある。

[GitSync](https://github.com/ViscousPot/GitSync) も使ってみたが、
cloneの挙動が怪しかったので使うのをやめた。
シンプルに `git clone` しちゃった方がわかりやすいのでそうする。

Termuxのインストールや、Termux:Widgetで `sync.sh` を呼び出せるようにしておくなどのセットアップが必要でやや手間ではあった。
が一度やってしまえば後はあまりメンテも不要なのでいったん快適に使えてはいる。

# 今後の本質的問題
ClaudeからObsidianの文書を読み書きさせる場合、
結局Claudeからどういう価値を引き出したいのかでClaudeにやらせるスコープがかなり変わると感じる。

- (A) フロー的な文書をストック的文書に集積させる
- (B) 文書同士のリンクを作ってグラフにしてコンテキスト構築する

Aはわかる。
Bはどんどん整合性を維持するスケーラビリティがネックになる気がしている。
かといって増えていく文書同士の内容的関連を常に追跡するのは人間でも無理。

Wikipediaはボランティアでこれがなされてると思うとすごい。
