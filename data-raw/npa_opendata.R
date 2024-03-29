#################################
# 【警察庁】交通事故統計データ
# https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html
# 本票: honhyo
# 補充票: hojuhyo
# 高速票: kosokuhyo
#################################
fs::dir_create(here::here("data/npa"), recurse = TRUE)
fs::dir_create(here::here("data-raw/npa",
                          c("teigisyo", "code_tbl")), recurse = TRUE)
npa_domain <- "https://www.npa.go.jp/"

if (length(fs::dir_ls(here::here("data-raw/npa"), recurse = TRUE, regexp = "(hoju|hon|kosoku)hyo_.+.csv$")) != 9L) {
  x <- 
    sprintf("%spublications/statistics/koutsuu/opendata/index_opendata.html", npa_domain) |> 
    rvest::read_html()
  
  df_npa_list <- 
    tibble::tibble(
      year = x |> 
        rvest::html_elements(css = "#contArea > main > article > section > section:nth-child(3) > p > a") |> 
        rvest::html_text() |> 
        stringi::stri_trans_nfkc() |> 
        stringr::str_remove("\\(.+\\)") |> 
        stringr::str_remove("年") |> 
        readr::parse_number(),
      url = x |> 
        rvest::html_elements(css = "#contArea > main > article > section > section:nth-child(3) > p > a") |> 
        rvest::html_attr(name = "href") |> 
        xml2::url_absolute(base = npa_domain)) |> 
    ensurer::ensure(nrow(.) == 3L)
  
  purrr::pwalk(
    df_npa_list,
    function(year, url) {
      fs::dir_create(here::here(glue::glue("data-raw/npa/{year}")))
      Sys.sleep(8)
      x <- 
        url |> 
        rvest::read_html() |> 
        rvest::html_elements(css = "#contArea > main > article > section > section > p > a") |> 
        rvest::html_attr(name = "href") |> 
        stringr::str_subset(".csv") |> 
        xml2::url_absolute(base = npa_domain)
      download.file(url = x,
                    destfile = here::here(glue::glue("data-raw/npa/{year}/{x}",
                                                     x = basename(x))))
    }
  )
}


# ファイル定義書 -----------------------------------------------------------------
if (length(fs::dir_ls(here::here("data-raw/npa/teigisyo"), regexp = ".csv$")) != 3L) {
  x <- 
    sprintf("%spublications/statistics/koutsuu/opendata/teigisyo/teigisyo.html", 
            npa_domain) |> 
    rvest::read_html()
  
  x |> 
    rvest::html_elements(css = "#contArea > main > article > section > section > p > a") |> 
    rvest::html_attr(name = "href") |> 
    xml2::url_absolute(base = npa_domain) |> 
    ensurer::ensure(length(.) == 3L) |> 
    purrr::walk(
      function(url) {
        Sys.sleep(8)
        download.file(url = url,
                      destfile = here::here(glue::glue("data-raw/npa/teigisyo/{x}",
                                                       x = basename(url))))
      }
    )
}


# 各種コード表 ------------------------------------------------------------------
if (length(fs::dir_ls(here::here("data-raw/npa/code_tbl"), regexp = ".csv$")) != 59L) {
  x <- 
    sprintf("%spublications/statistics/koutsuu/opendata/koudohyou/koudohyou.html", 
            npa_domain) |> 
    rvest::read_html()
  
  df_npa_code <- 
    tibble::tibble(
      url = x |> 
        rvest::html_elements(css = "#contArea > main > article > section > section > p > a") |> 
        rvest::html_attr(name = "href") |> 
        xml2::url_absolute(base = npa_domain),
      name = x |> 
        rvest::html_elements(css = "#contArea > main > article > section > section > p > a") |> 
        rvest::html_text()) |> 
    ensurer::ensure(nrow(.) == 59L)
  
  purrr::walk(
    df_npa_code$url,
    function(url) {
      Sys.sleep(8)
      download.file(url = url,
                    destfile = here::here(glue::glue("data-raw/npa/code_tbl/{x}",
                                                     x = basename(url))))
    }
  )
  df_npa_code |> 
    transmute(name,
              file = basename(url)) |> 
    readr::write_csv(here::here("data/npa/コード表一覧.csv"))
}


# parquet形式への出力 -----------------------------------------------------------
library(dplyr)
source(here::here("R/npa.R"))

code_files <- 
  fs::dir_ls(here::here("data-raw/npa/code_tbl/"), 
             recurse = TRUE, 
             regexp = ".csv$") |> 
  ensurer::ensure(length(.) == 59L) |> 
  naturalsort::naturalsort()

d_raw <- 
  fs::dir_ls(here::here("data-raw/npa"), 
           recurse = TRUE, 
           regexp = "honhyo_.+.csv$") |>
  ensurer::ensure(length(.) == 3L) |> 
  purrr::map(
    function(file) {
      read_npa_honhyo(file)    
    }
  ) |> 
  purrr::list_rbind()

glimpse(d_raw)

# ~5min
d <-
  d_raw |> 
  modify_npa_honhyo()

fs::dir_create(here::here("data/npa/honhyo"))
d |> 
  rename(pref_code = `都道府県コード`,
         year = 発生日時_年,
         month = 発生日時_月) |> 
  arrow::write_dataset(path = "data/npa/honhyo",
                       partitioning = c("pref_code", "year", "month"))
usethis::use_git_ignore("*.parquet")
