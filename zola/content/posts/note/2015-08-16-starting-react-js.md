+++
title = "Starting React.js"
date = 2015-08-16
aliases = ["/posts/note/2015-08-16-starting-react-js.html"]
[taxonomies]
tags = ["js","react"]
+++

## 今抱えている問題

jQueryベースのDHTMLによるDOM Manipulationをおもに使っていた。
Viewとデータが完全に結合しておりほぼテストが難しい。
テストが存在しないためAPIが増えていくにつれてどんどん変更するモチベーションが減っている。

データ構造としてのModelとそのDOM表現としてのViewで分離する。

## 解決策

- Vue.js
- Knockout.js
- Ractive.js
- React.js

Fluxアーキテクチャだとある程度ロールが整理されているので、
自分で考える必要がなくなっている。
省エネでいきたいのでReactにする。
データフローが一方向なのでテストパタンも減りそうだしいいかなと思っているのもある。

## Reactを使う場合

### 環境の準備

JS周りは環境準備に一苦労する（という所感）。
でも基本となる原則はあまり難しくないかもしれない。

まず処理系としてnode.jsとnpmを揃える。
プロジェクトのライフサイクル管をおこなうもろもろのエントリポイントは全部npmに任せてしまうのが正しい。
webpackやgulpなどの各種ツールチェインが生成する実行ファイルはすべて
node_modulesにインストールさせてそれらを`npm run XXX`から叩く。

必要なツールはpackage.jsonに宣言しておけばいいので、
開発者が自分でインストールするべきはnpmのみでよい、という整理になる。beautiful。

### まずはFluxを整理する

Componentの設計の流れは下記の記事である程度整理される。

[Thinking in React](https://facebook.github.io/react/docs/thinking-in-react.html)

Storeがイベントを受信するためのコールバックはDispatcherに登録する。
これによってComponentが発行したActionを受信できるようになる。

ComponentはStoreからデータを引いてきてStateのsourceとする。

UIの大枠を組み上げて、部品に分解。

