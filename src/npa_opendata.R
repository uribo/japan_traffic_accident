library(dplyr)
source(here::here("R/npa.R"))

files <-
  fs::dir_ls(here::here("data-raw/npa"), 
             recurse = TRUE, 
             regexp = "(hoju|hon|kosoku)hyo_.+.csv$")
code_files <- 
  fs::dir_ls(here::here("data-raw/npa/code_tbl/"), 
             recurse = TRUE, 
             regexp = ".csv$")

read_npa_code_tbl(code_files[33]) |> 
  filter(stringr::str_detect(都道府県名, "徳島"),
         stringr::str_detect(警察署等名, "徳島中央"))
read_npa_code_tbl(code_files[22]) |> 
  filter(stringr::str_detect(都道府県名, "徳島"))
code_files |> basename()

d <- 
  read_npa_honhyo(files[2]) |> 
  filter(都道府県コード == "80", 警察署等コード == "101", 市区町村コード == "201") |> 
  mutate(datetime = lubridate::make_datetime(year = `発生日時_年`, 
                                             month = `発生日時_月`,
                                             day = `発生日時_日`,
                                             hour = `発生日時_時`,
                                             min = `発生日時_分`)) |> 
  select(!c(資料区分, 都道府県コード, 警察署等コード, 市区町村コード)) |> 
  select(!c(ゾーン規制, 環状交差点の直径, 中央分離帯施設等)) |> 
  select(!c(starts_with("当事者種別"), starts_with("車両形状"), 
            starts_with("発生日時"), starts_with("一時停止規制"),
            starts_with("サイドエアバッグの装備"), starts_with("エアバッグの装備"),
            starts_with("車両の衝突部位"), starts_with("速度規制"),
            starts_with("用途別"), starts_with("車両の損壊程度")))

glimpse(d)

