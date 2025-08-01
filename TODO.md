# 要件
- `content/about.md` `content/contact.md` もHTML変換して生成すること。
- `templates/index.html` を使ってトップページを生成すること。
- `templates/index.html` のUpdatesには `content/posts/note` の最新記事10件を表示する。

# タスクリスト

## 必須タスク

- [ ] `content/about.md` をビルドして `_site/about/index.html` を生成する。
- [ ] `content/contact.md` をビルドして `_site/contact/index.html` を生成する。
- [ ] `templates/index.html` を使ってトップページ (`_site/index.html`) を生成する。
    - [ ] `content/posts/note` ディレクトリから最新10件の記事を読み込む。
    - [ ] 読み込んだ記事リストを `templates/index.html` のコンテキストに渡す。
    - [ ] `templates/base.html` をレイアウトとして適用する。

## テストと検証

- [ ] `cabal run build-site` を実行してサイトをビルドする。
- [ ] 生成された `_site/about/index.html` と `_site/contact/index.html` が正しく表示されることを確認する。
- [ ] 生成された `_site/index.html` を開く。
- [ ] `Updates` セクションに `content/posts/note` の記事が最新10件、日付の降順で表示されていることを確認する。

## 機能改善・追加

- [ ] **サイト設定の外部ファイル化:** `app/Main.hs` にハードコードされているサイトメタデータ (`SiteMeta`) を、`config.toml` や `config.yaml` のような設定ファイルから読み込むように変更する。
- [ ] **RSS/Atomフィードの生成:** 新しい記事が公開されたときに通知できるよう、RSS/Atomフィードを生成する機能を追加する。
- [ ] **タグ別アーカイブページの作成:** 各タグに属する記事の一覧ページ (`/tags/haskell.html` のような) を生成する機能を追加する。

## リファクタリング・メンテナンス

- [ ] **ビルドロジックの改善:** `app/Main.hs` のビルドロジックをモジュール化し、可読性とメンテナンス性を向上させる。
- [ ] **依存関係の棚卸しと更新:** `utky-github-io.cabal` ファイルを確認し、不要な依存関係を削除し、ライブラリを最新バージョンに更新する。
- [ ] **テストカバレッジの向上:** 現在テストが存在しないため、主要な機能（特にコンテンツのパースやテンプレートの適用ロジック）に対するテストを追加する。
