read_npa_honhyo <- function(path, year = 2022, col_name_fix = TRUE) {
  rlang::arg_match0(as.character(year),
                   as.character(seq.int(2019, 2022)))
  if (year >= 2022) {
    column_feature <-
      list(type = paste0(c("iccci",
                           "ddccc",
                           "ddddd",
                           "ddddd", # 20
                           "iiici",
                           "ccccc",
                           "iccdi",
                           "ciicc", # 40
                           "ccccd",
                           "dcccc",
                           "cccci",
                           "iiidd", # 60
                           "ddddc",
                           "ccc"), 
                         collapse = ""),
           name = c("資料区分", "都道府県コード", "警察署等コード", 
                    "本票番号", "事故内容", "死者数", "負傷者数", 
                    "路線コード", "地点コード", "市区町村コード", 
                    "発生日時_年", "発生日時_月", "発生日時_日", 
                    "発生日時_時", "発生日時_分", 
                    "昼夜", 
                    "日の出時刻_時", "日の出時刻_分", 
                    "日の入り時刻_時", "日の入り時刻_分", 
                    "天候", 
                    "地形", "路面状態", "道路形状", "信号機", 
                    "一時停止規制_標識_当事者A", 
                        "一時停止規制_表示_当事者A", "一時停止規制_標識_当事者B", 
                        "一時停止規制_表示_当事者B", "車道幅員", "道路線形", 
                        "衝突地点", "ゾーン規制", "中央分離帯施設等", 
                        "歩車道区分", "事故類型", "年齢_当事者A", "年齢_当事者B", 
                        "当事者種別_当事者A", "当事者種別_当事者B", "用途別_当事者A", 
                        "用途別_当事者B", "車両形状等_当事者A", "車両形状等_当事者B", 
                        "オートマチック車_当事者A", "オートマチック車_当事者B", 
                        "サポカー_当事者A", "サポカー_当事者B", "速度規制_指定のみ_当事者A", 
                        "速度規制_指定のみ_当事者B", "車両の衝突部位_当事者A", 
                        "車両の衝突部位_当事者B", "車両の損壊程度_当事者A", 
                        "車両の損壊程度_当事者B", "エアバッグの装備_当事者A", 
                        "エアバッグの装備_当事者B", "サイドエアバッグの装備_当事者A", 
                        "サイドエアバッグの装備_当事者B", "人身損傷程度_当事者A", 
                        "人身損傷程度_当事者B", "地点_緯度_北緯", "地点_経度_東経", 
                        "曜日_発生年月日", "祝日_発生年月日", "認知機能検査経過日数_当事者A", 
                        "認知機能検査経過日数_当事者B", "運転練習の方法_当事者A", 
                        "運転練習の方法_当事者B"))
  } else {
    column_feature <-
      list(type = paste0(c("dcccd",
                           "ddcdc",
                           "cdddd",
                           "ddddd", # 20
                           "ccdcc",
                           "cccdc",
                           "cddci",
                           "icccc", # 40
                           "ccccc",
                           "cccdd",
                           "ddddi",
                           "idd"),
                         collapse = ""),
           name = c("資料区分", "都道府県コード", "警察署等コード",
                    "本票番号", "事故内容", "死者数", "負傷者数",
                    "路線コード", "上下線", "地点コード", "市区町村コード",
                    "発生日時_年", "発生日時_月", "発生日時_日", "発生日時_時",
                    "発生日時_分",
                    "昼夜", "天候", "地形", "路面状態",
                    "道路形状", "環状交差点の直径", "信号機",
                    "一時停止規制_標識_当事者A", "一時停止規制_表示_当事者A",
                    "一時停止規制_標識_当事者B", "一時停止規制_表示_当事者B",
                    "車道幅員", "道路線形",
                    "衝突地点", "ゾーン規制", "中央分離帯施設等",
                    "歩車道区分", "事故類型",
                    "年齢_当事者A", "年齢_当事者B",
                    "当事者種別_当事者A", "当事者種別_当事者B",
                    "用途別_当事者A", "用途別_当事者B",
                    "車両形状_当事者A",
                    "車両形状_当事者B",
                    "速度規制_指定のみ_当事者A",
                    "速度規制_指定のみ_当事者B", "車両の衝突部位_当事者A",
                    "車両の衝突部位_当事者B", "車両の損壊程度_当事者A",
                    "車両の損壊程度_当事者B", "エアバッグの装備_当事者A",
                    "エアバッグの装備_当事者B", "サイドエアバッグの装備_当事者A",
                    "サイドエアバッグの装備_当事者B", "人身損傷程度_当事者A",
                    "人身損傷程度_当事者B",
                    "地点_緯度_北緯", "地点_経度_東経",
                    "曜日_発生年月日", "祝日_発生年月日"))
  }
  if (col_name_fix) {
    column_feature$name <- 
      stringr::str_replace_all(column_feature$name,
                             "車両形状",
                             "車両形状等")
  }
  d <- 
    readr::read_csv(
      path,
      locale = readr::locale(encoding = "cp932"),
      col_types = column_feature$type)
  colnames(d) <- column_feature$name
  d
}

read_npa_kosokuhyo <- function(path, year = 2022) {
  rlang::arg_match0(as.character(year),
                    as.character(seq.int(2019, 2022)))
  column_feature <-
    list(type = "ddccccccddccccdcc",
         name = c("資料区分", "都道府県コード", "警察署等コード", 
                  "本票番号", "発生地点", "道路管理者区分", "道路区分", 
                  "道路構造", "曲線半径", "縦断勾配", "トンネル番号", 
                  "当事者車両台数", "事故類型", "車両単独事故の対象物", 
                  "臨時速度規制の有無", "速度規制_臨時のみ", 
                  "トンネル延長距離"))
  if (year >= 2022) {
  } else {
    column_feature$type <- "ddcccccddccccdcc"
    column_feature$name <- 
      column_feature$name |> 
      stringr::str_subset("道路構造", negate = TRUE)
  }
  d <-
    readr::read_csv(
      path,
      locale = readr::locale(encoding = "cp932"),
      col_types = column_feature$type)
  colnames(d) <- column_feature$name
  d
}

read_npa_hojuhyo <- function(path) {
  d <-
    readr::read_csv(
      path,
      locale = readr::locale(encoding = "cp932"),
      col_types = "ddccccdcdddcccd")
  colnames(d) <-
    c("資料区分", "都道府県コード", "警察署等コード", 
      "本票番号", "補充票番号", "当事者種別", "乗車別", 
      "乗車等の区分", "エアバッグの装備", "サイドエアバッグの装備", 
      "人身損傷程度", "用途別", "車両形状", "車両の衝突部位", 
      "車両の損壊程度")
  d
}

parse_meta <- function(x, type) {
  type <- rlang::arg_match(type,
                           c("item", "cover", "description"))
  if (length(x) == 3L) {
    res <- 
      x |> 
      stringr::str_extract("((項目名|適用|説明)),(.+)", group = 3) |>
      na.omit() |> 
      stringr::str_remove_all(",") |> 
      stringr::str_squish()
    switch (type,
            "item" = res[1],
            "cover" = res[2],
            "description" = res[3])
  } else {
    code_str_l <- 
      x |> 
      stringr::str_which("^コード")
    end_x <- 
      ifelse(identical(code_str_l, integer(0)),
             length(x),
             code_str_l)
    if (type != "description") {
      res <- 
        x[seq.int(1, end_x-1)] |> 
        stringr::str_extract("((項目名|適用)),(.+)", group = 3) |>
        na.omit() |> 
        stringr::str_remove_all(",") |> 
        stringr::str_squish() |> 
        stringi::stri_trans_nfkc() |> 
        stringr::str_remove_all('\\"')
      if (type == "item")
        res[1]
      else if (type == "cover")
        res[2]
    } else {
      x[seq.int(x |> 
                  stringr::str_which("^(説明|,説明)"), 
                end_x-1)] |> 
        stringr::str_remove("^(説明|,説明)") |> 
        stringr::str_remove_all(",") |> 
        stringr::str_subset("^$", negate = TRUE) |> 
        stringr::str_c(collapse = "\n")
    } 
  }
}

metadata_npa_code <- function(path) {
  x <- 
    readr::read_lines(path, 
                      n_max = 10)
  c("item", "cover", "description") |> 
    purrr::set_names(c("item", "cover", "description")) |> 
    purrr::map(
      \(type) parse_meta(x, type))
}

read_npa_code_tbl <- function(path) {
  meta <- 
    metadata_npa_code(path)
  item <- 
    meta |> 
    purrr::pluck("item")
  cli::cli_inform(
    c("コード表の情報",
      "i" = glue::glue("項目名: {item}"),
      "i" = glue::glue("適用: {meta[[2]]}")))
  if (item %in% c("地点コード", "市区町村コード", "発生日時",                                                                                                   
                  "車両の衝突部位", "地点 緯度(北緯)", "地点 経度(東経)", 
                  "補充票番号", "発生地点", "本票番号", "トンネル延長距離")) {
    NULL
  } else if (item %in% c("路線コード", "路線(高速自動車国道自動車専用道(指定))コード",
                         "事故内容", "上下線", "昼夜", "天候", "地形", "路面状態",                                                                                              
                         "道路形状", "環状交差点の直径", "資料区分", "信号機", 
                         "一時停止規制 標識", "一時停止規制 表示", "車道幅員", 
                         "道路線形", "衝突地点", "ゾーン規制", "中央分離帯施設等", 
                         "歩車道区分", "事故類型", "年齢", "当事者種別", 
                         "用途別", "車両形状", "速度規制(指定のみ)", "車両の損壊程度", 
                         "エアバッグの装備", "サイドエアバッグの装備", 
                         "人身損傷程度", "曜日", "祝日", "乗車別", "乗車等の区分", 
                         "道路区分", "道路構造", "曲線半径", "縦断勾配", 
                         "当事車両台数", "事故類型", "車両単独事故の対象物", 
                         "臨時速度規制の有無", "速度規制(臨時のみ)")) {
    read_npa_code_commons(path)
  } else {
    switch (item,
            "警察署等コード" = read_npa_code_keisatusyotoukoudo(path),
            "都道府県コード" = read_npa_code_todouhukenkoudo(path),
            "道路管理者区分" = read_npa_code_dourokanrisyakubun(path),
            "トンネル番号" = read_npa_code_tonnerubangou(path)
    )
  }
}

modify_npa_code_df <- function(df) {
  df |> 
    dplyr::filter(!is.na(コード)) |> 
    dplyr::filter(!`コード` %in% c("項目名", "適用", "説明", 
                                "コード", "コード（都道府県）")) |> 
    dplyr::mutate(dplyr::across(tidyselect::where(is.character), 
                                .fns = stringi::stri_trans_nfkc))
}

read_npa_code_commons <- function(path) {
  readr::read_csv(path, 
                  col_names = c("コード", "区分", "備考"),
                  col_types = "ccc_________") |> 
    modify_npa_code_df()
}

read_npa_code_todouhukenkoudo <- function(path) {
  readr::read_csv(path, 
                  col_names = c("コード", "都道府県名"),
                  col_types = "cc________") |> 
    modify_npa_code_df()
}

read_npa_code_keisatusyotoukoudo <- function(path) {
  readr::read_csv(path, 
                  col_names = c("コード", 
                                "コード_警察署等",
                                "都道府県名", "警察署等名"),
                  col_types = "cccc_______") |> 
    modify_npa_code_df() |> 
    dplyr::rename(`コード_都道府県` = `コード`)  
}

read_npa_code_dourokanrisyakubun <- function(path) {
  readr::read_csv(path, 
                  col_names = c("コード", "区分"),
                  col_types = "cc________") |> 
    modify_npa_code_df()  
}

read_npa_code_tonnerubangou <- function(path) {
  readr::read_csv(path, 
                  col_names = c("コード",
                                paste("コード",
                                      c("路線", "トンネル番号"),
                                      sep = "_"),
                                "トンネル名"),
                  col_types = "cccc__") |> 
    modify_npa_code_df() |> 
    dplyr::rename(`コード_都道府県` = `コード`)  
}

detect_code_item <- function(path) {
  readr::read_lines(path, n_max = 1) |> 
    stringr::str_extract("(^項目名),(.+)", group = 2) |> 
    stringr::str_remove_all(",") |> 
    stringr::str_squish()
}

coord_to_dohun <- function(x, type) {
  rlang::arg_match(type,
                   c("longitude", "latitude"))
  switch (type,
          "latitude" = .latitude_to_dohun(x),
          "longitude" = .longitude_to_dohun(x)
  )
}

# getOption("digits")
options(digits = 10)
.latitude_to_dohun <- function(x) {
  x <- 
    as.integer(
      stringr::str_pad(
        as.integer(x), width = 9, pad = "0", side = "right")) / 1000
  stringr::str_c(
    "北緯",
    x |> 
      stringr::str_sub(1, 2),
    "度",
    x |> 
      stringr::str_sub(3, 4),
    "分",
    x |> 
       stringr::str_sub(5, 6) |> 
       as.numeric() +
       x |> 
       stringr::str_sub(7) |> 
       as.numeric(),
    "秒")
}
.longitude_to_dohun <- function(x) {
  x <- 
    as.integer(x) / 1000
  stringr::str_c(
    "東経",
    x |> 
      stringr::str_sub(1, 3),
    "度",
    x |> 
      stringr::str_sub(4, 5),
    "分",
    x |> 
      stringr::str_sub(6, 6),
    (x |> 
       stringr::str_sub(7, 7) |> 
       as.numeric() +
       x |> 
       stringr::str_sub(8) |> 
       as.numeric()),
    "秒")
}

modify_npa_honhyo <- function(df) {
  df |> 
    dplyr::mutate(datetime = lubridate::make_datetime(year = `発生日時_年`, 
                                                      month = `発生日時_月`,
                                                      day = `発生日時_日`,
                                                      hour = `発生日時_時`,
                                                      min = `発生日時_分`)) |>
    dplyr::mutate(latitude = `地点_緯度_北緯`,
                  longitude = `地点_経度_東経`) |> 
    dplyr::mutate(
      dplyr::across(.cols = c(latitude, longitude),
                    .fns = ~ dplyr::na_if(.x, 0))
    ) |> 
    dplyr::mutate(latitude = dplyr::if_else(is.na(latitude),
                                            NA_character_,
                                            coord_to_dohun(latitude, "latitude")),
                  longitude = dplyr::if_else(is.na(longitude),
                                             NA_character_,
                                             coord_to_dohun(longitude, "longitude"))) |> 
    dplyr::rowwise() |> 
    dplyr::mutate(latitude = ifelse(is.na(latitude), 
                                    NA_real_,
                                    kuniezu::parse_lat_dohunbyo(latitude)),
                  longitude = ifelse(is.na(longitude), 
                                     NA_real_,
                                     kuniezu::parse_lon_dohunbyo(longitude))) |> 
    dplyr::ungroup()
}
