------------------
title: [WIP]ElmのHttp処理部をスタブする
date: 2016-10-17
tags: [elm]
------------------

## 問題

Elmでアプリケーション書いていると当然リモートのHTTP APIへリクエストを飛ばす部分がでてくる。
けれどチュートリアルの章を読むと`Http`パッケージへhard wireしてある。このポリシーに従ってそのまま実装していくと、
`elm-reactor`で動かした際に、APIのエンドポイントに本当にリクエストを飛ばしてしまう。

APIの実装がまだない場合や、テストでダミーのJSONを渡したい場合はこれだと困る。

## 解法

なにか大仰なモックライブラリが必要か、とか悩んでしまったけど、簡単な話だった。

HTTPリクエスト処理部の関数だけ抜き出して、後から渡せるようにすればいい。つまり **高階関数** で解決する。
`Http`ライブラリを呼び出す関数またはダミー用データを返す関数、いずれかを後から注入する感じ。

1. HTTPリクエストを表すメッセージ用update
2. その他のイベントを表すメッセージ用update

update関数を二つに分割する。というか、HTTPリクエスト処理部だけを別関数に分割する。

## サンプルケース

たとえば外部のAPIエンドポイントと通信して"Hello"という文字列を取り出してくることを考える。

### モデル

外部から受け取った文字列を保存するだけのモデル。

```elm
type alias AppState = { out : String }

init : ( AppState, Cmd Msg )
init = ( { out = "" }, Cmd.none )
```

### ビュー

`say hello`ボタンを押すと`FetchHello`というメッセージをトリガする。
モデルが保持する文字列をテキストとして書き出す。

```elm
view : AppState -> Html Msg
view model =
  div [ ]
      [ button
          [ onClick FetchHello ]
          [ text "say hello" ]
      , span
          []
          [ text model.out ]
      ]
```

### メッセージ

```elm
type Msg
  = FetchHello
  | GotHello String
  | FailHello Http.Error
```

- `FetchHello`: リモートAPIに"Hello"という文字列を送ってもらう要求。**これの応答をスタブしたい**
- `GotHello`: 正常に文字列を受信した場合の応答。
- `FailHello`: 文字列を受信に失敗した場合の応答。

## メッセージのハンドラ: update関数

```elm
update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg model =
  case msg of
    FetchHello -> -- 普通だったらここで Http.get とかやるけど。。。。
    GotHello s -> { model | out = s } ! []
    FailHello e -> { model | out = "failure!" } ! []
```

ようやく改善したいポイントに到着。
ここにHTTPリクエストの発行を埋め込んでしまうと、呼び出し先のHTTP APIがどこかで起動している必要がある。
開発中は`elm-reactor`で気軽に動作確認したいのに、外部API呼び出しが走るのは避けたい。

そこだけ外から制御できるようにしよう。

```elm
update : Cmd Msg -> Msg -> AppState -> ( AppState, Cmd Msg )
update cmd msg model =
  case msg of
    FetchHello -> (model, cmd)
    GotHello s -> { model | out = s } ! []
    FailHello e -> { model | out = "failure!" } ! []
```

テスト用と実際のAPI呼び出し用の関数を用意する。

```elm
-- | 実際に外部APIを呼び出してリモートからデータを受け取る
fetchHello : Cmd Msg
fetchHello =
  Http.get "http://some.endpoint/hello" string
    |> Task.perform FailHello GotHello

-- | ダミーのデータを返す
dummyHello : Cmd Msg
dummyHello =
  Http.succeed "Hello"
    |> Task.perform FailHello GotHello
```



## 雑感

論旨とずれるけれど、Elmは少ないbuilding blocksと関数型プログラミングだけで本当にだいたいのことができてしまう。
MonadやFunctorなどの概念は一切出てこない（これは[evancのポリシー](https://groups.google.com/forum/m/#!topic/elm-discuss/1acyOfxvasA)だと思う)
