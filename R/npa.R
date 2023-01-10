read_npa_honhyo <- function(path) {
  d <- 
    readr::read_csv(
      path,
      locale = readr::locale(encoding = "cp932"),
      col_types = "ddccdcccdccdddddddddccdcdcdcdccddcccccccccccccdddddddddddd")
  colnames(d) <- 
    c("資料区分", "都道府県コード", "警察署等コード", 
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
      "歩車道区分", "事故類型", "年齢_当事者A", "年齢_当事者B", 
      "当事者種別_当事者A", "当事者種別_当事者B", 
      "用途別_当事者A", "用途別_当事者B", "車両形状_当事者A", 
      "車両形状_当事者B", "速度規制_指定のみ_当事者A", 
      "速度規制_指定のみ_当事者B", "車両の衝突部位_当事者A", 
      "車両の衝突部位_当事者B", "車両の損壊程度_当事者A", 
      "車両の損壊程度_当事者B", "エアバッグの装備_当事者A", 
      "エアバッグの装備_当事者B", "サイドエアバッグの装備_当事者A", 
      "サイドエアバッグの装備_当事者B", "人身損傷程度_当事者A", 
      "人身損傷程度_当事者B", "地点_緯度_北緯", "地点_経度_東経", 
      "曜日_発生年月日", "祝日_発生年月日")
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
