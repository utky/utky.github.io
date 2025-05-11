---
title = "SimplenoteからEvernoteへ移行した"
date = 2020-07-13
[taxonomies]
tags = ["scheme","evernote"]
---

数年前にEvernoteのiOS版アプリの動作が遅すぎて発狂した結果、[Simplenote](https://app.simplenote.com/)に移行した。SimplenoteはMarkdownプレビューモードもあるしタグによる分類もあるし、 正直メモとるにはそれで十分ではあった。

ただその後、一部のメモが[Scrapbox](https://scrapbox.io/)にも散乱してきたりして、正直何がどこにあるのか自分でも混乱するようになってきた。加えて読みたい記事のブックマーク管理をはてなブックマークから移行する先を探しているところでもあった。ブックマークと読書メモはセットになるのでメモ機能がまともにあった方がいい。

という背景からこうしたnote takingアプリとして再度Evernoteを使うことにしたのだった。

幸いSimplenoteにもエクスポート機能は具備されており、さらにJSON形式でコンテンツを吐き出してくれるので意味不明な独自規格のEvernoteと違ってとても優しい。つまりこのエクスポートされたJSONからEvernoteがインポート可能な形式を生成すればしれっとデータを移行できると踏んだ。

Evernoteのエクスポートを表現するマークアップの説明は下記のリンクが参考になる。

* [ENEX エクスポート用マークアップ](https://evernote.com/blog/how-evernotes-xml-export-format-works/)
* [ENML Note記述用マークアップ](https://dev.evernote.com/doc/articles/enml.php)

これを大して読まずに雰囲気で作ったSimplenote JSON -> ENEX変換用のSchemeプログラムがこちら。

[simplenote-json2evernote.scm](https://gist.github.com/utky/09d3ab02fc164ae64fe5abd688d7e023)

標準入力からJSONをとって標準出力にENEXを吐き出すの下記のような感じで使うと大丈夫。

```bash
cat source/notes.json | gosh simplenote-json2evernote.scm > export.enex
```

これで *だいたい* インポートできたのだけど、Evernoteへの取り込みの途中でエラーになったので多分まだなんか潜んでいる。
まあ、いいか。。。。

津島善子の誕生日中に記事を書きれるかと思ったけど開発マシンの不調で間に合わんかった。。。