library(dplyr)
library(arrow)
source(here::here("R/npa.R"))
# code_files <-
#   fs::dir_ls(here::here("data-raw/npa/code_tbl/"),
#              recurse = TRUE,
#              regexp = ".csv$") |>
#   ensurer::ensure(length(.) == 59L) |>
#   naturalsort::naturalsort()

# 本票 ----------------------------------------------------------------------
# df_code <- 
#   readr::read_csv(here::here("data/npa/コード表一覧.csv"), 
#                 col_types = "cc")
# 
# df_code |> 
#   filter(stringr::str_detect(name, "事故類型"))
# 
# read_npa_code_tbl(code_files[5])
# read_npa_code_tbl(code_files[29])
# read_npa_code_tbl(code_files[3]) |> 
#   filter(stringr::str_detect(都道府県名, "徳島"),
#          stringr::str_detect(警察署等名, "徳島中央"))
# read_npa_code_tbl(code_files[3]) |> 
#   filter(stringr::str_detect(都道府県名, "徳島"),
#          stringr::str_detect(警察署等名, "徳島中央"))
# read_npa_code_tbl(code_files[2]) |> 
#   filter(stringr::str_detect(都道府県名, "徳島"))
# read_npa_code_tbl(code_files[2]) |>
#   filter(stringr::str_detect(都道府県名, "東京"))
# read_npa_code_tbl(code_files[11])

npa_ta_schema <- 
  list(honhyo = arrow::schema(
    source_file = arrow::utf8(),
  資料区分 = arrow::int32(),
  警察署等コード = arrow::utf8(),
  本票番号 = arrow::utf8(),
  事故内容 = arrow::int32(),
  死者数 = arrow::int32(),
  負傷者数 = arrow::int32(),
  路線コード = arrow::utf8(),
  上下線 = arrow::int32(),
  地点コード = arrow::utf8(),
  市区町村コード = arrow::utf8(),
  year = arrow::int32(),
  month = arrow::int32(),
  発生日時_日 = arrow::int32(),
  発生日時_時 = arrow::int32(),
  発生日時_分 = arrow::int32(),
  昼夜 = arrow::int32(),
  天候 = arrow::int32(),
  地形 = arrow::int32(),
  路面状態 = arrow::int32(),
  道路形状 = arrow::utf8(),
  環状交差点の直径 = arrow::utf8(),
  信号機 = arrow::int32(),
  一時停止規制_標識_当事者A = arrow::utf8(),
  一時停止規制_表示_当事者A = arrow::int32(),
  一時停止規制_標識_当事者B = arrow::utf8(),
  一時停止規制_表示_当事者B = arrow::int32(),
  車道幅員 = arrow::utf8(),
  道路線形 = arrow::int32(),
  衝突地点 = arrow::utf8(),
  ゾーン規制 = arrow::utf8(),
  中央分離帯施設等 = arrow::int32(),
  歩車道区分 = arrow::int32(),
  事故類型 = arrow::utf8(),
  年齢_当事者A = arrow::utf8(),
  年齢_当事者B = arrow::utf8(),
  当事者種別_当事者A = arrow::utf8(),
  当事者種別_当事者B = arrow::utf8(),
  用途別_当事者A = arrow::utf8(),
  用途別_当事者B = arrow::utf8(),
  車両形状等_当事者A = arrow::utf8(),
  車両形状等_当事者B = arrow::utf8(),
  速度規制_指定のみ_当事者A = arrow::utf8(),
  速度規制_指定のみ_当事者B = arrow::utf8(),
  車両の衝突部位_当事者A = arrow::utf8(),
  車両の衝突部位_当事者B = arrow::utf8(),
  車両の損壊程度_当事者A = arrow::int32(),
  車両の損壊程度_当事者B = arrow::int32(),
  サイドエアバッグの装備_当事者A = arrow::int32(),
  サイドエアバッグの装備_当事者B = arrow::int32(),
  人身損傷程度_当事者A = arrow::int32(),
  人身損傷程度_当事者B = arrow::int32(),
  曜日_発生年月日 = arrow::int32(),
  祝日_発生年月日 = arrow::int32(),
  日の出時刻_時 = arrow::int32(),
  日の出時刻_分 = arrow::int32(),
  日の入り時刻_時 = arrow::int32(),
  日の入り時刻_分 = arrow::int32(),
  オートマチック車_当事者A = arrow::int32(),
  オートマチック車_当事者B = arrow::int32(),
  サポカー_当事者A = arrow::utf8(),
  サポカー_当事者B = arrow::utf8(),
  認知機能検査経過日数_当事者A = arrow::utf8(),
  認知機能検査経過日数_当事者B = arrow::utf8(),
  運転練習の方法_当事者A = arrow::int32(),
  運転練習の方法_当事者B = arrow::int32(),
  pref_code = arrow::utf8(),
  datetime = arrow::timestamp(unit = "s"),
  latitude = double(),
  longitude = double()))

d_raw <- 
  arrow::open_dataset("data/npa/honhyo/",
                      schema = npa_ta_schema$honhyo) |> 
  # 本票
  filter(資料区分 == 1) |> 
  select(!資料区分)

# 東京都
d_tokyo22 <- 
  d_raw |> 
  filter(pref_code == "30", year == 2022) |>
  collect()

d_tokyo22 |> 
  filter(!is.na(longitude), !is.na(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mapview::mapview()

d_city36201 <-
  d_raw |>
  # 徳島県徳島市
  filter(pref_code == "80", 市区町村コード == "201") |> 
  select(!c(pref_code, year, month, starts_with("発生日時"))) |>
  select(datetime, latitude, longitude, 警察署等コード, 市区町村コード, 死者数, 負傷者数) |> 
  collect()

d_city36201 |> 
  glimpse()

d_city36201 |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mapview::mapview()

d <- 
  d_raw |> 
  filter(stringr::str_detect(source_file, "data-raw/npa/2022/honhyo_2022.csv")) |> 
  collect() |> 
  select(!source_file)


# 高速票 ---------------------------------------------------------------------
files <-
  fs::dir_ls(here::here("data-raw/npa/"), 
             recurse = TRUE, 
             regexp = "kosokuhyo_.+.csv$")

tibble::tibble(
  path = files,
  year = stringr::str_extract(basename(files), "[0-9]{4}")
) |> 
  purrr::pmap(
    read_npa_kosokuhyo
  ) |> 
  purrr::list_rbind()


# 補充票 ---------------------------------------------------------------------
files <-
  fs::dir_ls(here::here("data-raw/npa/"), 
             recurse = TRUE, 
             regexp = "hojuhyo_.+.csv$")
tibble::tibble(
  path = files,
  year = stringr::str_extract(basename(files), "[0-9]{4}")
) |> 
  purrr::pmap(
    read_npa_hojuhyo
  ) |> 
  purrr::list_rbind()
