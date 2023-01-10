library(dplyr)
source(here::here("R/npa.R"))

files <-
  fs::dir_ls(here::here("data-raw/npa"), 
             recurse = TRUE, 
             regexp = "(hoju|hon|kosoku)hyo_.+.csv$")
code_files <- 
  fs::dir_ls(here::here("data-raw/npa/code_tbl/"), 
             recurse = TRUE, 
             regexp = ".csv$") |> 
  naturalsort::naturalsort()

df_code <- 
  readr::read_csv(here::here("data/npa/コード表一覧.csv"), 
                col_types = "cc")

df_code |> 
  filter(stringr::str_detect(name, "事故類型"))

read_npa_code_tbl(code_files[5])
read_npa_code_tbl(code_files[29])
read_npa_code_tbl(code_files[3]) |> 
  filter(stringr::str_detect(都道府県名, "徳島"),
         stringr::str_detect(警察署等名, "徳島中央"))
read_npa_code_tbl(code_files[2]) |> 
  filter(stringr::str_detect(都道府県名, "徳島"))

d <- 
  read_npa_honhyo(files[2]) |> 
  filter(都道府県コード == "80", 警察署等コード == "101", 市区町村コード == "201") |> 
  filter(事故内容 == 2, 事故類型 == "21") |> 
  mutate(datetime = lubridate::make_datetime(year = `発生日時_年`, 
                                             month = `発生日時_月`,
                                             day = `発生日時_日`,
                                             hour = `発生日時_時`,
                                             min = `発生日時_分`)) |> 
  select(!c(資料区分, 都道府県コード, 警察署等コード, 市区町村コード)) |>
  select(!c(事故内容, 死者数)) |> 
  select(!c(ゾーン規制, 環状交差点の直径, 中央分離帯施設等,
            天候, 地形, 上下線, 路面状態, 道路形状, 車道幅員, 道路線形,
            歩車道区分)) |> 
  select(!c(starts_with("当事者種別"), starts_with("車両形状"), 
            starts_with("発生日時"), starts_with("一時停止規制"),
            starts_with("サイドエアバッグの装備"), starts_with("エアバッグの装備"),
            starts_with("車両の衝突部位"), starts_with("速度規制"),
            starts_with("用途別"), starts_with("車両の損壊程度"),
            starts_with("人身損傷程度")))

glimpse(d)

