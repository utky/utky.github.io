---
title = "WIP ElmのHttp処理部をスタブする"
date = 2016-10-17
aliases = ["/posts/note/2016-10-17-testing-elm-http.html"]

tags = ["elm"]
---

[Elm Advent Calendar 2016](http://qiita.com/advent-calendar/2016/elm) 2日目の記事です。

## 問題

Elmでアプリケーション書いているとリモートのWeb APIへリクエストを飛ばす部分がでてくる。
けれど[チュートリアルの章](https://www.elm-tutorial.org/en/06-fetching-resources/04-players-cmds.html)を読むと`Http`パッケージの関数を直接埋め込んで埋め込んである。この流儀に従ってそのまま実装していくと、`elm-reactor`で動かした際に、**Web APIのエンドポイントに本当にリクエストを飛ばしてしまう**。

Web APIの実装がまだない場合や、テストでダミーのJSONを渡したい場合はこれだと困る。

できればテスト用に動かす時は`Http`処理部分のスタブがほしい。

## 解法

なにか大仰なモックライブラリが必要か、とか悩んでしまったけど、簡単な話だった。

HTTPリクエスト処理部の関数だけ抜き出して、呼び出し元から渡せるようにすればいい。つまり **高階関数** で解決する。
`Http`ライブラリを呼び出す関数またはダミー用データを返す関数、いずれかを後から注入する感じ。

## サンプルケース

たとえばWeb APIエンドポイントと通信して`"Hello"`という文字列を取り出してくるようなサンプルを考える。

サンプルは下記の環境で書いている。

```json
    "dependencies": {
        "elm-lang/core": "4.0.0 <= v < 5.0.0",
        "elm-lang/html": "1.0.0 <= v < 2.0.0",
        "evancz/elm-http": "3.0.1 <= v < 4.0.0"
    },
    "elm-version": "0.17.0 <= v < 0.18.0"
```

では前提となるビューやモデルについて提示しておく。

### モデル(状態を表す)

外部から受け取った文字列を保存するだけのモデル。

```elm
type alias AppState = { out : String }
```

初期状態ではただ空の文字列を出力するだけとしておく。

```elm
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

このあたりから本題。

```elm
type Msg
  = FetchHello
  | HalloDone String
  | HelloFail Http.Error
```

- `FetchHello`: Web APIに"Hello"という文字列を送ってもらう要求。**これの応答をスタブしたい**。
- `HalloDone`: 文字列を **正常に** 受信した場合の応答。
- `HelloFail`: 文字列を受信に **失敗した** 場合の応答。

### メッセージのハンドラ: update関数

```elm
update : Msg -> AppState -> ( AppState, Cmd Msg )
update msg model =
  case msg of
    -- FIXME: 普通だったらここで Http.get とかやるけど。。。。
    FetchHello -> Http.get string "http://hogefuga/hello" |> Task.perform HelloFail HalloDone

    -- ここ以降はWeb APIからのデータ受信後の動作なのであまり重要ではない
    HalloDone s -> { model | out = s } ! []
    HelloFail e -> { model | out = "failure!" } ! []
```

**ようやく改善したいポイントに到着。**

`FetchHello`のパターンマッチに注目。
チュートリアルだとここで`Http`モジュールの関数を呼び出して、
Web APIの呼び出しをおこなう`Task`が発生する。

しかしここにHTTPリクエストの発行を埋め込んでしまうと、メッセージを受信した瞬間にWeb APIの呼び出しが走る。
そのため呼び出し先のHTTP APIがどこかで起動している必要がある。*これが困る。*
開発中は`elm-reactor`で気軽に動作確認したいのに、外部API呼び出しが走るのは避けたい。

そこだけ呼び出し元から制御できるようにしよう。

```elm
update : Cmd Msg -> Msg -> AppState -> ( AppState, Cmd Msg )
update helloRequest msg model =
  case msg of
    FetchHello -> (model, helloRequest)
    HalloDone s -> { model | out = s } ! []
    HelloFail e -> { model | out = "failure!" } ! []
```

updateの第一引数が`FetchHello`メッセージを受信した時のアクションになる。
そこだけ呼び出し元から受け取れるようにしておく。

テスト用と実際のAPI呼び出し用の関数を用意する。

```elm
-- | 実際に外部APIを呼び出してリモートからデータを受け取る
fetchHello : Cmd Msg
fetchHello =
  Http.get "http://hogefuga/hello" string
    |> Task.perform HelloFail HalloDone

-- | ダミーのデータを返す
dummyHello : Cmd Msg
dummyHello =
  Http.succeed "Hello"
    |> Task.perform HelloFail HalloDone
```

Productionで動かす時は

```
update fetchHello : Msg -> AppState -> (AppState, Cmd Msg)
```

開発環境で動かす時は

```
update dummyHello : Msg -> AppState -> (AppState, Cmd Msg)
```

のように渡しわけることになる。

## さらなる改善

### 複数のHTTPリクエストに対応するための準備

今までのコードだと`FetchHello`しかスタブ処理に対応できない。
きっと他にもHTTPリクエストは発生するだろうからこれに対処する。

なのでちょっとデータ型に調整を入れる。

```elm
type Msg
  = FetchHello
  | HalloDone String
  | HelloFail Http.Error
```

これを

```elm
type Msg
  = Req Request
  | Res Response

type Request
  = FetchHello

type Response
  = HelloDone String
  | HelloFail Http.Error
```

こうする。

- `Msg`: アプリケーション全体で発生しうるメッセージを表す
- `Request`: HTTPリクエストを発行せよ、という命令を表す
- `Response`: HTTPレスポンスで受け取ったデータを保持するコンテナを表す

要求を表すメッセージと応答を表すメッセージを*データ型として*分離している。
これにより今回スタブしたい部分の型をより正確に表現できるようになる。

```elm
Request -> Cmd Response
```

なんらかのHTTPリクエストを発行する要求を受け取って、HTTPレスポンスを返すような関数だ。

`FetchHello`以外のメッセージが増えた場合にはこんな風に書けるだろう。

```elm
dispatch : Request -> Cmd Response
dispatch req =
  case req of
    FetchHello -> ...
    FetchHola -> ...
    FetchHallo -> ...
```

上手く複数パターンのHTTPリクエストに対応できそうだ。

### updateの対応

これに合わせて`update`関数はこのように書き直される。

```elm
update : (Request -> Cmd Response) -> Msg -> AppState -> ( AppState, Cmd Msg )
update disp msg model =
  case msg of

    Req request ->
      (model, (disp request |> Cmd.map Res))

    Res response ->
      case response of
        HelloDone s -> { model | out = s } ! []
        HelloFail e -> { model | out = "failure!" } ! []
```

大事なポイントを抜粋してみる。

```elm
    Req request ->
      (model, (disp request |> Cmd.map Res))
```

何かしらのHTTPリクエストを発行した場合には、この分岐に入る。

ちょっと型が見えにくいので整理しておこう。

```elm
request : Request
disp : Request -> Cmd Response
```

`request`の値は`FetchHello`かもしれないし`FetchHola`かもしれない。
いずれにしても`disp`に`request`に適用することでHTTPレスポンスを得ることができる。

```elm
disp request : Cmd Response
```

これだけで終わりではなくて、`Cmd.map`という関数も使っている。

```elm
Cmd.map Res : Cmd Response -> Cmd Msg
```

`Cmd.map Res`の型は`Cmd Response -> Cmd Msg`を返すのでこれで`update`関数の型に合わせることができる。
これらを組み合わせて適用すると下記のような型になるはずだ。

```elm
disp request |> Cmd.map Res : Cmd Msg
```

これで`update`関数の返り値の型に整合したのでコンパイルエラーはなくなる。

### ダミーデータを返す

開発用ではダミーデータを返す必要がある。
これは`Task`モジュールの関数`succeed`を活用することで実現できる。

```elm
dispatch : Request -> Cmd Response
dispatch req =
  case req of
    FetchHello -> 
      Task.succeed "Hello"
        |> Task.perform HelloFail HelloDone
```

これで`update`に渡す関数`dispatch`が出来上がった。

本当にHTTPリクエストを発行したい場合は、
同じ型だけど実際に`Http`モジュールの関数を使ってHTTP通信を行うような新しい関数を用意してあげればいい。

### 関数を組み合わせてAppを作る

`update`と`dispatch`関数を組み合わせるのは、`Program`を作る段階で行う。

```elm

main : Program Never
main =
  Html.App.program
    { init = init
    , view = view
    , update = update dispatch
    , subscriptions = subscriptions
    }
```

今回は`Main.elm`という1ファイルに全て詰め込んだけれど、
開発用と商用で分けるなら`main`関数定義だけそれぞれのファイルに書いてやる。

- `Main.elm`
- `Test.elm`

それぞれ`dispatch`関数を開発用と商用で呼び分けるようにしておくといい。

## 振り返り

とりあえずこれで開発用にHTTP通信部をスタブしたり、本当にWeb APIを呼ぶように切り替えたりできるようになった。

実際に自分がElmのコードを書く時はこんな風にしてスタブを作りながら`elm-reactor`で表示を確認している。

こんなこと書いておいて何だけど「いや実はもっとまともなモック用のライブラリがあってね。。。」という情報あったら是非教えてほしい。

### この方法のメリット

- HTTP通信部をスタブ処理で置き換えることができる

### この方法のデメリット

- 直接HTTP通信部を埋め込むよりも1サイクルだけupdate関数の呼び出しが増えるのでもしかしたらパフォーマンスに影響があるかも


コード全体は[Gist](https://gist.github.com/utky/e96db0d4b8845c4bc877b5a3f6b3a2ae)にあげました。

## 雑感 (読み飛ばしていい)

「なんだやってることは当たり前で普通じゃないか」と思うかもしれない。

普通に高階関数を使っているだけだ。 **でもそれでいい。それがいい**。

複雑な概念の導入や技法もなく関数を組み合わせるだけで要求を満たせてしまう。
そういう関数型プログラミングの良さを実感できたのが今回の収穫だった。

もちろんElm Architectureがとても良くできていたおかげでもあるんだけど。
