---
title = "サーバ構築時の初期設定について"
date = 2014-10-06
aliases = ["/posts/note/2014-10-06-initial-server-provisioning.html"]
---

ある開発環境でベアメタルサーバやVMサーバを追加することはよくある。
（今そんなんばっか）

そして、その時に行う初期設定作業にはBoilerplateが多すぎる。


## 初期設定のBoiler Plate

1. 共有されるadmin的なユーザの追加
2. パッケージの更新
3. ssh鍵のデプロイ
4. 鍵認証のみに限定
5. NTPサーバの指定
6. DNSサーバの指定

## プロビジョニングにあたっての懸念

上記の定型作業をこなすためには、ansibleなどを使いたい、となるのが人情。

でも、あれ……？

ansibleでは鍵を使った認証にしてなかったっけ？

その鍵の配給や生成はじゃあ、どうすんの？

## Ansibleのドキュメント読んでみた

なんだ、ちゃんとInventoryにpassword定義したり、実行時にaskさせたりできるじゃん。

[List of Behavioral Inventory Parameters](http://docs.ansible.com/intro_inventory.html#list-of-behavioral-inventory-parameters)

Inventoryファイルにパスワードを書いておくのもあれなので、

    --ask-pass

のオプションをつけてplaybookを実行するのがいいそうな。



Paasになんかデプロイする場合は、この辺りの工程をすっとばしているはずなのであまり気にしないけど、社内で開発サーバデプロイする時とか、いちいちめんどくてめっちゃ「うがー」ってなってた。

真面目な解法に取り組みたい。
