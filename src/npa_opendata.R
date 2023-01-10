

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
# read_npa_code_tbl(code_files[2]) |> 
#   filter(stringr::str_detect(都道府県名, "徳島"))

library(arrow)
d <- 
  arrow::open_dataset("data/npa/honhyo/",
                      schema = arrow::schema(
                        資料区分 = double(),
                        警察署等コード = utf8(),
                        本票番号 = utf8(),
                        事故内容 = double(),
                        死者数 = double(),
                        負傷者数 = double(),
                        路線コード = utf8(),
                        上下線 = double(),
                        地点コード = utf8(),
                        市区町村コード = utf8(),
                        year = double(),
                        month = double(),
                        発生日時_日 = double(),
                        発生日時_時 = double(),
                        発生日時_分 = double(),
                        昼夜 = double(),
                        天候 = double(),
                        地形 = double(),
                        路面状態 = double(),
                        道路形状 = utf8(),
                        環状交差点の直径 = utf8(),
                        信号機 = double(),
                        一時停止規制_標識_当事者A = utf8(),
                        一時停止規制_表示_当事者A = double(),
                        一時停止規制_標識_当事者B = utf8(),
                        一時停止規制_表示_当事者B = double(),
                        車道幅員 = utf8(),
                        道路線形 = double(),
                        衝突地点 = utf8(),
                        ゾーン帰省 = utf8(),
                        中央分離帯施設等 = double(),
                        歩車道区分 = double(),
                        事故類型 = utf8(),
                        年齢_当事者A = utf8(),
                        年齢_当事者B = utf8(),
                        当事者種別_当事者A = utf8(),
                        当事者種別_当事者B = utf8(),
                        用途別_当事者A = utf8(),
                        用途別_当事者B = utf8(),
                        車両形状_当事者A = utf8(),
                        車両形状_当事者B = utf8(),
                        速度規制_指定のみ_当事者A = utf8(),
                        速度規制_指定のみ_当事者B = utf8(),
                        車両の衝突部位_当事者A = utf8(),
                        車両の衝突部位_当事者B = utf8(),
                        車両の損壊程度_当事者A = double(),
                        車両の損壊程度_当事者B = double(),
                        サイドエアバッグの装備_当事者A = double(),
                        サイドエアバッグの装備_当事者B = double(),
                        人身損傷程度_当事者A = double(),
                        人身損傷程度_当事者B = double(),
                        曜日_発生年月日 = double(),
                        祝日_発生年月日 = double(),
                        pref_code = utf8(),
                        datetime = arrow::timestamp(unit = "s"),
                        latitude = double(),
                        longitude = double())
                      ) |> 
  filter(pref_code == "80") |> 
  collect()

d |>
  filter(!is.na(longitude), !is.na(latitude)) |>
  sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326) |>
  mapview::mapview()

# 入力値の誤り？
d |> 
  dplyr::filter(is.na(latitude) | is.na(longitude)) |> 
  select(latitude, longitude) |> 
  distinct(latitude, longitude, .keep_all = TRUE) |> 
  ensurer::ensure(nrow(.) == 3L)


# 高速票 ---------------------------------------------------------------------
files <-
  fs::dir_ls(here::here("data-raw/npa"), 
             recurse = TRUE, 
             regexp = "kosokuhyo_.+.csv$")
read_npa_kosokuhyo(files[3])


# 補充票 ---------------------------------------------------------------------
files <-
  fs::dir_ls(here::here("data-raw/npa"), 
             recurse = TRUE, 
             regexp = "hojuhyo_.+.csv$")
read_npa_hojuhyo(files[3])
