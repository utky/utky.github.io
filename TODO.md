# 要件
- `content/about.md` `content/contact.md` もHTML変換して生成すること。
- `templates/index.html` を使ってトップページを生成すること。
- `templates/index.html` のUpdatesには `content/posts/note` の最新記事10件を表示する。

# タスクリスト

## 必須タスク

- [x] `content/about.md` をビルドして `_site/about.html` を生成する。
- [x] `content/contact.md` をビルドして `_site/contact.html` を生成する。
- [x] `templates/index.html` を使ってトップページ (`_site/index.html`) を生成する。
    - [x] `content/posts/note` ディレクトリから最新10件の記事を読み込む。
    - [x] 読み込んだ記事リストを `templates/index.html` のコンテキストに渡す。
    - [x] `templates/base.html` をレイアウトとして適用する。

## テストと検証

- [x] `cabal run build-site` を実行してサイトをビルドする。
- [x] 生成された `_site/about.html` と `_site/contact.html` が正しく表示されることを確認する。
- [x] 生成された `_site/index.html` を開く。
- [x] `Updates` セクションに `content/posts/note` の記事が最新10件、日付の降順で表示されていることを確認する。

## 機能改善・追加

- [ ] **GitHub Pages:** GitHub Pagesのデプロイ用ワークフローを使ってビルド結果のアーティファクトからPagesを公開するように変更する。 (参考: https://docs.github.com/ja/pages/getting-started-with-github-pages/configuring-a-publishing-source-for-your-github-pages-site#%E3%82%AB%E3%82%B9%E3%82%BF%E3%83%A0-github-actions-%E3%83%AF%E3%83%BC%E3%82%AF%E3%83%95%E3%83%AD%E3%83%BC%E3%81%AB%E3%82%88%E3%82%8B%E5%85%AC%E9%96%8B )
- [ ] **RSS/Atomフィードの生成:** 新しい記事が公開されたときに通知できるよう、RSS/Atomフィードを生成する機能を追加する。
- [ ] **タグ別アーカイブページの作成:** 各タグに属する記事の一覧ページ (`/tags/haskell.html` のような) を生成する機能を追加する。

## リファクタリング・メンテナンス

- [ ] **ビルドロジックの改善:** `app/Main.hs` のビルドロジックをモジュール化し、可読性とメンテナンス性を向上させる。
- [ ] **依存関係の棚卸しと更新:** `utky-github-io.cabal` ファイルを確認し、不要な依存関係を削除し、ライブラリを最新バージョンに更新する。
- [ ] **テストカバレッジの向上:** 現在テストが存在しないため、主要な機能（特にコンテンツのパースやテンプレートの適用ロジック）に対するテストを追加する。
