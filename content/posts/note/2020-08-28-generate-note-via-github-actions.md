---
title = "Evernoteのノートを自動生成して振り返りをする"
date = 2020-08-28

tags = ["scala","evernote"]
---

## 背景

毎日の記録を取るためにEvernoteを使ってる。
日記というほどまとまっていない走り書きの蓄積のようなもの。
毎日記録は取るのは別にいいのだが、活用に問題を抱えていた。
記録を取るだけとって振り返ることができず次に活かせるサイクルができていなかった。
振り返りがうまく回っていないために「何のために記録しているのだろう」ともやっとする時期がしばらく続いていた。
理想的には毎週振り返りを行って次にやるべきことの軌道修正をしたり、その週達成した成果を次のモチベーションにつなげたりしたいという思いはありつつも、
「まあ振り返りはいずれ。。。」みたいな感じで腰が重いまま後回しにしていた。

ところがここ最近やりたいことや勉強したいこと、しなきゃいけないことが輻輳して頭が混乱気味になることが増えてきた。
さすがにこまめにやることを振り返って進むべき道を調整していかないといけないと思い、振り返りイベントをEvernote上で表現するための作戦を考えることにした。

## 問題

Evernoteには既に振り返りなどにつけるテンプレートがあるようだ。

[Evernote テンプレート](https://evernote.com/intl/jp/templates)

ところがこれらのテンプレートは自分の用途にはいまいち合わなそうだった。
かといって過去1週間の内容を改めてサマライズを手書きでやるのも面倒くさがって絶対に続かないことがわかっていた。

ということで自分のやりかたに適したノートを何とか自動生成する方法を考える必要があった。

毎週振り返りを行うことを前提に下記のような内容のノートが作られるイメージを持って取り組むことにした。

```
2020-08-30 の振り返り

## todo

(その週のやるべきこと)

* Aを読む
* Bの書籍の練習問題を解く

## done

(その週の雑多なトピック)

* 2020-08-24
    + Aを読んだ
* 2020-08-25
    + 呼吸をした
* 2020-08-26
    + おなか減った
* 2020-08-27
    + 早く寝た
* 2020-08-28
    + 仕事終わらなかった
* 2020-08-29
    + 特になにもしなかった

## plan

(翌週やりたいこと)

* 

```

## 解決方法

[utky-note-processor](https://github.com/utky/utky-note-processor)

普通にEvernote Java SDKを使ったScalaのバッチ処理を作ってGitHub Actionsで回すようにした。

コード書くよりはトークン使うまでの準備の方が面倒だったかもしれない。

### トークンを使えるようにするまで (2020-08時点)

API Keyを発行するに当たってApplication Nameを決めておく必要があるが、
無邪気に決めるとEvernoteの商標登録とぶつかってサポートから突っ込みを受けることがある。
なので無駄なやり取りを省くためにも [商標登録に関するページ](https://evernote.com/legal/trademark-use) を読んでおくこと推奨する。

1. API Key の発行
    + [このページ](https://dev.evernote.com/doc/#start) の右上にある「GET AN API KEY」からフォームを埋めて発行依頼を出す
2. Activate
    + デフォルトではテスト用のSandboxでしかこのAPI Keyを使えないので、Activateする
    + [サポートページ](https://dev.evernote.com/support/) から「Activate an API Key」のフォームを開き必要事項を入力して依頼を出す
3. Developer Tokenの発行
    + [Token発行用ページ](https://dev.evernote.com/get-token/) で「Production」を指定して発行依頼を出す

2はサポート側での承認がおりるまで数日かかる。

### Scalaの実装まわり

#### Tagless Final : Cats

日々そんなにScalaをたくさん書くわけではないので、毎回探り探りでやっている。
多少お作法的なものに従うべく、[Cats](https://typelevel.org/cats/ )を入れたうえでTagless Final形式でDIするようにした。

`F[Unit]` を返す大きなfor式がバッチ処理のメインロジックになる。

[こういう感じ](https://github.com/utky/utky-note-processor/blob/8d73ee06a16012b1c62cb263886ba719f29b89b8/src/main/scala/ilyaletre/evernote/tasks/Task.scala#L31-L65)

`F` には下記のような型クラスの制約をつけている。
```scala
  def apply[F[_]
    : GetTemplate
    : CreateNote
    : FindNotes
    : GetNote
    : Monad]
    (date: ZonedDateTime): F[Unit]
```

これは `F` がノートを作ったり検索したりMonadであったりすることを満たしてくれるものであれば何でもいいということ。
実際にこれらの制約を満たす実装の選択はこのapplyを呼び出す側に委ねられる。

例えばこの `F` が要求される制約を満たす `Evernote` というデータ型を定義した上で、上記のapplyを呼び出してみるとこうなる。
```scala
val result: Evernote[Unit] = WeeklyReview(date) // WeeklyReview() でapplyが呼ばれる
```

型推論により `F` は `Evernote` であると決定されるため、applyに渡される型クラスのインスタンスは `Evernote` になる。

もしテスト用に `Evernote` とは別の実装を使いたい場合は、やはり上記の型クラスの制約を満たすデータ型 `Test` を定義した上で
```scala
val result: Test[Unit] = WeeklyReview(date)
```
とすることでapply内で使われる型クラスの実装を切り替えることができる。
という感じでScalaの型推論を援用した実装の切替を使っている。

なんかでもCatsのコード覗いていると [simulacrum](https://github.com/typelevel/simulacrum) のマクロ使っているっぽいところがあり、「ああ、やっぱりみんな型クラスの定義するのめんどくさいんだな」っていう波動を感じた。めんどくさいよね。主に型クラスのメソッドをオブジェクトのメソッドに生やし直すところ（[ここ](https://scalac.io/typeclasses-in-scala/)でいうOpsクラスの定義をしなければいけないところ)

#### ノートの生成 : Scalate

EvernoteのノートはHTMLっぽいXMLで表現される。
今回は過去1週間のノートの内容を基にこの振り返り用のノートの内容を生成するので [Scalate](https://scalate.github.io/scalate/) というテンプレートエンジンを使った。
下記の3点が特徴的であり、ライブラリとしては結構巨大な印象だった。

* 動的HTMLページのレンダリングなどで使われることを想定
* テンプレートへの不正な変数束縛を防止するコンパイラ支援
* 複数のテンプレート構文をサポート

自分に必要なのはただmustache構文でテキストデータを作る機能だけなので
やや大仰ではあったがAPI自体は特に難しい印象なかったので使った。

テンプレートは [こんな感じ](https://github.com/utky/utky-note-processor/blob/master/templates/weekly-review.xml.mustache) でいい加減に書いたがまあ動くのでよし。

## GitHub Actions

特に変なところはなくプレーンに書いた。

[ci.yml](https://github.com/utky/utky-note-processor/blob/8d73ee06a16012b1c62cb263886ba719f29b89b8/.github/workflows/ci.yml#L12-L28)

cron schedule で起動しないことがあったり、job登録しなおしたりがちゃがちゃいじってしまった。
ActionsタブからScala用のworkflowを作るUIに導かれてYAMLファイルを作り直したら何故か普通に動くようになったので、若干もやっとしている。

## まとめ

ScalaでEvernote APIを読んだけどトークン作る過程がなんかめんどかった。
まあ大体動く。

ちまちました定期処理にGitHub Actions使うのは悪くなさそう。
計算リソースをありがとう。

## 他にやりたいこと

タニタのHealthPlanetでデータ集めて真面目にトラックしたい。