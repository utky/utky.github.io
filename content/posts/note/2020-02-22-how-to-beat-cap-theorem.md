+++
title = "「How to beat the CAP theorem」読んだ"
date = 2020-02-22
aliases = ["/posts/note/2020-02-22-how-to-beat-cap-theorem.html"]
[taxonomies]
tags = ["architecture","pipeline","bigdata"]
+++

ビッグデータを支える技術を読んでいて言及された「ラムダアーキテクチャ」を提唱した記事（だったはず）

ラムダアーキテクチャは既に市井で実用されていると思うし、自分も職場で見てるけどデザインの背景とかよく知らなかったので見直しの意味で読む。

[How to beat the CAP theorem](http://nathanmarz.com/blog/how-to-beat-the-cap-theorem.html)

## 大体の概要

### CAP定理から見つかる問題

下記で述べるように可用性を犠牲にするのはそもそも運用に耐えないし、結果整合性でもまだ複雑過ぎる。
CAP定理はデータシステムを作る本質的な難しさを表していると言える。

#### 可用性がないと困る

データベースの可用性よりも整合性を採用すると仮定する。
この場合、書き込みはバッファリングされる。しかしバッファリングしているアプリケーションがバッファごと故障して消えてしまうリスクは依然としてある。
バッファリングされた書き込みは他のアプリケーションから見えないためやはり不整合を招いてしまう。
仮にバッファリングをやめればこれは解決できるが、それはすなわちアプリケーション全体が利用できなくなることにつながる。

#### 結果整合性でもやはり辛い

結果整合性という方法によって整合性を保証する。これは成功した書き込み結果が他の読み込みではすぐには見えないことを意味する。
書き込みが成功した直後にそのキーに対して2つの読み込みがあったとしても異なる結果が返ってくるかもしれない。
このように分岐した読み取り結果をマージして修復するread repairが必要になる。

アプリケーション側でこのread repairを行うのはとても重たい作業である。
仮に誤ったread repairを行ってしまうと正しい結果は失われてしまい取り返しのつかない破壊を起こす。

### データシステムとは

改めてデータシステムが何かというのを捉え直してみる。

単純な定義は下記のようになる。

```
Query = Function(All Data)
```

すなわちデータシステムはデータセットに対する問い合わせに応答するものである。
この問い合わせをクエリと呼ぶ。

この等式は抽象化し過ぎて一見すると役に立たないが、クエリとデータという役割を捉え直すことがCAP定理を克服することにつながる。

#### データ

データは2つの性質を持つ。

時刻ベースである。
すなわちある時点での事実を表している。時間が進んだとしても過去に発生した事実のデータが不正とはならない。

データは不変である。
これはデータがある時点での事実を表していること、過去の事実を改変できないことからも明らかである。
CRUDと操作のうち許容されるのはすなわちCRのみとなる。

Updateは新しい事実の生成で代用できる。Deleteは例えば「Twitterのフォロワーが消えた」ことを表すためにunfollowの事実を新しく記録すればよい。
このデータの不変性こそがCAT定理を克服する鍵になる。

#### クエリ

過去も含めたデータセットに対する関数であると考えられる。
それはデータを集約したり結合したりしてサブセットを抽出する。

### CAP定理を克服する

もともと問題だったのはインクリメンタルな更新とCAP定理の組み合わせだった。
可変のデータを更新するのがread repairを招いた。

しかしデータを不変にすればレプリカ間で不整合になることはない。
一時的な障害があってもいずれ追記された新しいデータが読めるようになるしread repairも必要ない。
クエリから見るとデータがあるかないかどちらかの状態しかないというように単純化できる。

可変データをやめて不変データを使いクエリで一から結果を計算することでCAP定理から生まれた問題を克服することができる。

勿論これは実験である。実際はクエリで結果を一から計算するのは非効率だ。
すなわちここから検討すべきなのは最適化である。
バッチ処理やストリーミング処理などはこの最適化の技術に含まれる。

すべてのデータセットに作用して素早く完了する関数は効率が悪く非現実的である。

### バッチ処理

あらかじめクエリの結果を計算しておくことで非効率なクエリを取り除くことができる。
このようなアプローチの実装はHadoopとして存在している。
Hadoopではデータの保存にはHDFSを用いて事前計算にはMapReduceを用いることができる。
計算結果をアプリケーションから利用しやすいようにElaphantDBなどに保存する。

バッチ処理では任意の関数を任意のデータに対して作用させられる。ただし出力される結果はバッチ処理の所要時間のため数時間古いものになるだろう。

### バッチシステムとCAP定理、人的エラーへの耐性

CAP定理の観点でこのバッチ処理を見直してみると、結果整合性をもたらしてはくれるものの極端である。
書き込み結果がクエリで見えるようになるのに数時間くらいはかかる可能性があるためだ。

人的エラーへの耐性について。
クエリの不具合があった場合でも修正版をデプロイして元なるデータセットから再計算すればよい。

### リアルタイムレイヤ

上記のバッチシステムは数時間前のデータをクエリで利用可能なようになっている。
一方で直近数時間のデータを埋め合わせてクエリの結果に含められるようにする必要がある。
直近数時間に対するクエリであれば、データセット全体に対するクエリは簡単になる。

直近数時間のデータを埋め合わせるためにはバッチシステムと並行して実行されるリアルタイムシステムが必要になる。そしてクエリの応答結果を完成させるためには、バッチのビューとリアルタイムのビューをマージする必要がある。

リアルタイムレイヤにはRiakやCassandraなどのデータベースを使う。そしてこのレイヤにはインクリメンタルな更新を用いる。

Hadoopにおけるリアルタイムな計算をサポートするものとしてStormが挙げられる。Stormは無限のストリームをスケーラブルにかつ堅牢に実行することができる。
さらにその計算結果をCassandraに書き込むことでクエリ向けの直近数時間のビューを提供する。

### バッチレイヤ + リアルタイムレイヤの評価

リアルタイムレイヤでは依然と同様に可変データを更新するため複雑性を導入する。しかしそれらが使われるのは直近数時間のデータであり、時間の経過とともにバッチレイヤの処理結果で上書きされる。
そのためリアルタイムレイヤにおいて誤りがあったとしてもバッチレイヤが訂正してくれる。つまり複雑性は一時的なものとなる。

- リアルタイムレイヤ: データ処理は速い代わりに複雑
- バッチレイヤ: データ処理は遅いがシンプルで堅牢

### ガベージコレクション

追記型あのストレージを用意したことにより不変データが無限に増えていずれ保存できないサイズになる。
そのため意味や価値のなくなったデータを取り除く必要がある。
この手法を用いて最新のデータ以外を消すようにすれば可変性を模倣することもできる。
こうしたガベージコレクションもバッチタスクとして実装することができる。

### 結論

データシステムの構築を難しくしているのはCAP定理ではない。インクリメンタルな更新と可変データである。
これに対してCRUDからCRだけを許容した不変データのストレージを使ったバッチレイヤとリアルタイムレイヤを組み合わせたアーキテクチャを提案した。
これは誤りにも強いため人的エラー(バグの混入)などがあっても訂正することができる堅牢さを持つ。

## これに対する反論

いわゆる「カッパアーキテクチャ」サイドの主張。

[ラムダアーキテクチャに関する疑問](https://www.infoq.com/jp/news/2014/09/lambda-architecture-questions/)

ラムダアーキテクチャの堅牢性はほぼ永続的に残る追記型ストレージ(HDFS)から再計算ができることに依存している。
一方でカッパアーキテクチャではストリーム処理レイヤのストリーム再送による再計算で堅牢性を謳うことになる。
ストリーム処理レイヤを例えばメッセージブローカーであるKafkaなどに担わせた場合、そのメッセージを永続的に保存しておくのだろうか。

## 感想

不変データの威力を思い知る内容だった。
追記型ストレージを軸に据えることでプロセッシング部分の誤りも後からやり直せば訂正できる堅牢性が手に入る。

課金処理の絡むビジネスの基幹システムなどにおいてはこの堅牢性はなんとかして保証したいものだと思う。
自分も広告配信ログの蓄積において同じようなモチベーションを抱えていたりするので同様の堅牢性は重要な要件だと認識している。

ただし堅牢性のトレードオフとしてデータフローが2経路になるという別の複雑性がもたらされている。
この記事自体は数年前の話なので今となってはもう少し洗練されたアプローチがあるように思う。

ということで過去の提案について把握することができたので最近の動向を探って行きたいと思う次第。
(今までデータ系の基盤は全く知らなかったけど面白いなあ)