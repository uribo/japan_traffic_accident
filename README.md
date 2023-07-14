日本の交通事故に関するデータセット
==================

日本国内が対象となっている交通事故に関するデータセットのなかから、下記に示すものを手元で使いやすくするためのコードを置いています（データのダウンロードは各自で行います）。

## 1. 【警察庁】交通事故統計データ

- 元データの公開URL: https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html

```r
# データのダウンロードおよびdataフォルダへのparquet形式でのファイル保存を行います
source("data-raw/npa_opendata.R")
```


