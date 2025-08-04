# CLAUDE.md

このファイルは、このリポジトリでコードを作業する際のClaude Code (claude.ai/code) へのガイダンスを提供します。

## プロジェクト概要

HaskellのShakeビルドシステムとSlickライブラリを使用して構築された静的サイトジェネレーターです。Markdownで書かれた記事をMustacheテンプレートでレンダリングして個人ブログを生成します。

## アーキテクチャ

メインアプリケーションは `app/Main.hs:175` で定義されたShakeベースのビルドシステムで、以下の主要コンポーネントがあります：

- **ビルドシステム**: 依存関係ベースのビルドにHaskell Shakeを使用
- **コンテンツパイプライン**: `content/` 内のMarkdownファイルをSlickを使ってHTMLに変換
- **テンプレートシステム**: `templates/` 内のMustacheテンプレートで継承構造 (post.html -> base.html)
- **静的アセット**: `static/` 内のファイルを出力ディレクトリにコピー
- **出力**: 生成されたサイトファイルは `_site/` ディレクトリに配置

### 主要なデータ構造

- `Post`: タイトル、コンテンツ、日付、オプションのタグを持つ個別のブログ記事を表現 (app/Main.hs:91)
- `Section`: ネストしたセクション対応の記事コレクション/カテゴリを表現 (app/Main.hs:80)
- `SiteMeta`: 作者、URL、ソーシャルハンドルを含むグローバルサイト設定 (app/Main.hs:60)

## 開発でよく使うコマンド

### サイトのビルド
```bash
cabal run build-site
```

このコマンドはサイトジェネレーターをコンパイル・実行し、以下を行います：
1. `templates/post.html` と `templates/base.html` から記事テンプレートをビルド
2. `content/posts/` 内の全Markdownファイルを処理
3. `static/` から `_site/` へ静的アセットをコピー

### 開発環境のセットアップ
```bash
# 依存関係のインストール
cabal build

# インタラクティブ開発用のGHCi起動
cabal repl
```

### コンテンツ構造

- 記事は `content/posts/` に配置、整理用にサブディレクトリあり
- 記事はメタデータ用のフロントマターを持つMarkdownファイル (.md) である必要
- 静的アセット (CSS、JS、画像) は `static/` に配置、そのままコピーされる
- テンプレートはMustache記法を使用、エスケープなしHTMLには `{{{ content }}}` を使用

## テンプレートシステム

テンプレート継承は以下のように動作します：
1. `templates/post.html` - タイトル、日付、コンテンツを持つ記事レイアウト
2. `templates/base.html` - ナビゲーション、フッター、メタタグを持つベースページレイアウト
2. `templates/section.html` - ページのコレクションをリストで表示する索引ページのレイアウト

コンテンツの流れ: Markdown -> 記事テンプレート -> ベーステンプレート -> 最終HTML

## サイト設定

サイトメタデータは `app/Main.hs:39-47` にハードコードされており、以下を含みます：
- サイトタイトル: "Hash λ Bye"  
- ベースURL: "https://utky.github.io/"
- 作者とソーシャルメディアハンドル

サイトはGoogle Analyticsをサポートし、ベーステンプレートにオプトアウトクッキー機能が組み込まれています。
