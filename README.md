日本の交通事故に関するデータセット
==================

日本国内が対象となっている交通事故に関するデータセットのなかから、下記に示すものを手元で使いやすくするためのコードを置いています（データのダウンロードは各自で行います）。

## 1. 【警察庁】交通事故統計データ

- 元データの公開URL: https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html

```r
# データのダウンロードおよびdataフォルダへのparquet形式でのファイル保存を行います
source("data-raw/npa_opendata.R")
```

コード表の読み解き

```r
source(here::here("R/npa.R"))
code_files <- 
  fs::dir_ls(here::here("data-raw/npa/code_tbl/"), 
             recurse = TRUE, 
             regexp = ".csv$") |> 
  naturalsort::naturalsort()

code_files[1]
#> /Users/suryu/Documents/projects2020/japan_traffic_accident/data-raw/npa/code_tbl/1_koudohyou_siryoukubun.csv
  
read_npa_code_tbl(code_files[1])
#> コード表の情報
#>
#> ℹ 項目名: 資料区分
#> ℹ 適用: 本票、補充票、高速票
#> # A tibble: 3 × 3
#>
#>   コード 区分   備考 
#>   <chr>  <chr>  <chr>
#> 1 1      本票   NA   
#> 2 2      補充票 NA   
#> 3 3      高速票 NA  
```

parquetファイルの処理は`src/npa_opendata.R`を参考にしてください（整理中）。
