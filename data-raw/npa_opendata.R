#################################
# 【警察庁】交通事故統計データ
# https://www.npa.go.jp/publications/statistics/koutsuu/opendata/index_opendata.html
# 本票: honhyo
# 補充票: hojuhyo
# 高速票: kosokuhyo
#################################
fs::dir_create(here::here("data-raw/npa"), recurse = TRUE)
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
# https://www.npa.go.jp/publications/statistics/koutsuu/opendata/teigisyo/teigisyo.html
# 各種コード表 ------------------------------------------------------------------
# https://www.npa.go.jp/publications/statistics/koutsuu/opendata/koudohyou/koudohyou.html

