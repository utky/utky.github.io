---
title: "Claude Codeで作る幼稚園向けエージェント"
date: "2026-02-27"
tags: ["claude-code", "ai-agent", "typescript", "dbt"]
---

# 幼稚園のめんどいあれこれ
幼稚園のお知らせがPDFでくる。
予定や期限つきで対応が必要なものなどいろいろ。
ばらばらのタイミングで来るので「あの件どこに書いてあったっけ？」がほんとにわからなくなる。
すぐにタスク管理に登録できればよいのだが、割と忘れる。

# 解決の方向性
PDFのテキストデータを解析して以下のような情報の抽出・検索を支援する。

1. 幼稚園の提示する予定を検索できる
2. 幼稚園の提示する予定を任意でカレンダーに登録できる

1の点はせっかくなのでAIエージェントによるエージェンティックサーチに任せるようにしてみる。

# 解決手段
Claude Code Proプランを突っ込む！

# AIエージェントを使うということ
仕事でも使っているけれど、プライベートでも使ってみると学べることもあった。

## 学び1) plan modeで解法を練りきれば割と差分は確認せず放置でもいいやとなる
planを練るのが楽しい。
自分の意図があっているかを十分に検証できる。設計の本質はここかもなーとすら思う。

## 学び2) 詳しいところはツッコめるけど詳しくないところはツッコめない
あたりまえ。
自分はデータパイプラインに関しては言えることがある。
一方でフロントエンド開発に対しては経験が浅い。

するとどうなるか。

- フロントエンドのコードはほぼ雰囲気でマージする。
- データパイプラインのコードやデータモデルはかなり練る。

つまりアプリケーション前提で設計の練度には _かなりの濃淡が出る_ ことになる。
一般にLLMは学習データの分布からTypescriptのコードを上手く書けることは多いらしいので、たまたま今のバランスで困ってない。
というかフロントエンド疎い人はVibe Coding向きなのではとすら思う。

## 学び3) Skiilの使い方はもっと慣れたい
skillはいつどういうタイミングで増やすのかわからん。
意図を的確に実行計画に表現してくれない場合に明確な手順を渡してもっとうまくやってもらうためのものだと理解している。

今のところdbtの設計に関してはある程度の方針があるので、そこをskill化して運用している。

# 成果
React + Viteでスルッと画面ができる驚異。

![home](images/top.png)

![chat](images/chat.png)

構成は以下だけど、mermaidレンダリングは後でやります。

```mermaid
flowchart TD
    subgraph Workspace["Google Workspace"]
        GDrive["Google Drive"]
        GCal["Google Calendar API"]
        GAS["Apps Script\n毎時 Drive → GCS 転送"]
    end

    subgraph GCP["☁️ Google Cloud Platform"]

        subgraph Storage["Cloud Storage"]
            GCS_PDF["PDF Uploads バケット"]
            GCS_API["API Data バケット\n(NDJSON)"]
            GCS_FE["Frontend バケット\n(JS/CSS)"]
        end

        subgraph BatchLayer["バッチ処理"]
            Scheduler["Cloud Scheduler\n毎時起動"]
            Workflows["Cloud Workflows"]
            CRJob["Cloud Run Jobs\ndbt コンテナ"]
        end

        subgraph VertexAI["Vertex AI [aiplatform]"]
            Gemini["Gemini Flash\nML.GENERATE_TEXT\n(テキスト抽出・イベント抽出・チャット)"]
            Embedding["Embedding モデル\nML.GENERATE_EMBEDDING\n(ベクトル化)"]
        end

        BQ["BigQuery\n(DWH + Vector Index)"]

        subgraph AppLayer["アプリケーション"]
            CRApp["Cloud Run\nHono バックエンド"]
        end

    end

    subgraph Browser["ユーザーブラウザ"]
        React["React SPA"]
    end

    %% Drive → GCS
    GDrive -->|"PDFを保存"| GAS
    GAS -->|"PDFをアップロード"| GCS_PDF

    %% バッチ処理フロー
    Scheduler -->|"ワークフローを起動"| Workflows
    Workflows -->|"ジョブを実行"| CRJob
    CRJob -->|"dbt run"| BQ
    GCS_PDF -->|"PDFからテキストを抽出"| Gemini
    Gemini -->|"抽出テキストを格納"| BQ
    BQ -->|"チャンクをベクトル化"| Embedding
    Embedding -->|"ベクトルを格納"| BQ
    BQ -->|"JSONをエクスポート"| GCS_API

    %% API サービング
    GCS_API -->|"NDJSONを読み込み"| CRApp
    CRApp -->|"ベクトル検索を実行"| BQ
    CRApp -->|"LLM推論を要求"| Gemini
    CRApp <-->|"イベントを登録・取得"| GCal

    %% フロントエンド配信
    CRApp -->|"index.htmlを配信"| React
    GCS_FE -.->|"JS/CSSを直接配信（LBなし）"| React

    %% スタイル
    classDef storage fill:#4285F4,color:#fff,stroke:#1a73e8
    classDef compute fill:#34A853,color:#fff,stroke:#1e8e3e
    classDef ai fill:#EA4335,color:#fff,stroke:#c5221f
    classDef data fill:#FBBC04,color:#000,stroke:#f29900
    classDef ext fill:#FF6D00,color:#fff,stroke:#e65100

    class GCS_PDF,GCS_API,GCS_FE storage
    class CRApp,CRJob compute
    class Gemini,Embedding ai
    class BQ data
    class GDrive,GCal,GAS ext
```

# 課題
コストがやばい。

![cost](images/cost.png)

Gemini 2.5 Flashを使っているがThinking Budgetをnon-zeroにしたまま動かしていた。
やってるタスクは幼稚園からもらったPDFからのテキスト抽出なのでThinking絶対いらない。

ということで引き続き調整。
