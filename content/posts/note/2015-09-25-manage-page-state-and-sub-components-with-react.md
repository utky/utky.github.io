+++
title = "Manage page state and sub-components with React.js"
date = 2015-09-25
aliases = ["/posts/note/2015-09-25-manage-page-state-and-sub-components-with-react.html"]
[taxonomies]
tags = ["js","react"]
+++

*注意事項*

下記の投稿ではReactにおけるComponentのstateの所在を求めて
ぐだぐだと書いています。

が2015/10現在ではほとんどアプローチに決着がついており、
下記のフレームワークでそれぞれにそのアプローチが表現されています。

- [flux](https://github.com/facebook/flux)
- [redux](https://github.com/rackt/redux)

とりあえず使ってみて思想を感じるのがよいと思います。

問題の発見
--------------

React.jsでUIを組み上げる過程ですごく悩んだのが、
複数のComponentを組み合わせて一つのページを作成するときに
どこにstateを持たせておくか、ということです。

```
Home.js
  LoginForm.js
```

という包含関係だった場合に、stateを`Home`と`LoginForm`のどちらで
管理させるかという問題です。

Reactは原則的に「_`state`は最小限にして、できる限り全てを`props`として入力せよ_」というような哲学を推奨しているようです。

これを極端に推し進めるとトップレベルのComponentが保持するstateをowneeのpropsに渡していく方法が正解ということになります。
公式の解説は下記のリンクをご参照ください。

[Multiple Components](https://facebook.github.io/react/docs/multiple-components.html#data-flow)

[Reactのprops/stateとFluxのStore](http://mizchi.hatenablog.com/entry/2015/08/24/233919)

ここでポイントとなるのは*owneeで起こる入力イベントをどうハンドルするか*です。

React上でpropsを伝搬させていく上でのポイントは下記の三点だと理解していますが、
特にイベントのハンドルで重要なのは3です。

1. データは常にOwnerからOwneeへと流れます。
2. Owneeは常に`props`しか受け取らず`state`を持ちません。
3. Owneeはローカル`state`を持つ代わりに変更のイベントを上位コンポーネントを通知します。

データフロー
-------------

React.jsではTwo-way bindingの項でこれに言及していました。
[Two-Way Binding Helpers](https://facebook.github.io/react/docs/two-way-binding-helpers.html)

たぶんこんな感じでデータが流れるのが _React-way_ なのだと理解しています。

```
    Data Flow

      props       
Home ----> LoginForm
              | onChange
  <-----------+

    Event Flow
```

上位から下位へとデータをパスしていきDOMを生成する一種のストリーム処理みたいですね。

fluxの有無に関わらずReactに閉じて考えてもデータのデータのフローが一方向になります。


よりよい方法
------------

とにかく内部的な状態を可能な限り排除して、ReactComponentを純粋関数のように仕上げていくことが、より堅牢なUIを構築するためには必要なようです。

つまりトップレベルのComponentですらstateを持たないということになります。

現時点では「Fluxアーキテクチャを導入した上でStoreに全ての状態を集約するのかな」と想像していますがまだ答えを見つけていません。
