library(dplyr)
source(here::here("R/npa.R"))

code_files <- 
  fs::dir_ls(here::here("data-raw/npa/code_tbl/"), 
             recurse = TRUE, 
             regexp = ".csv$") |> 
  naturalsort::naturalsort()

# 本票 ----------------------------------------------------------------------
files <-
  fs::dir_ls(here::here("data-raw/npa"), 
             recurse = TRUE, 
             regexp = "honhyo_.+.csv$")
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

d_raw <- 
  files |> 
  purrr::map(
    function(file) {
      read_npa_honhyo(file)    
    }
  ) |> 
  purrr::list_rbind()

d <- 
  d_raw |> 
  modify_npa_honhyo()

# glimpse(d)

fs::dir_create(here::here("data/npa/honhyo"))
d |> 
  rename(pref_code = `都道府県コード`) |> 
  mutate(year = lubridate::year(datetime),
         month = lubridate::month(datetime)) |> 
  arrow::write_dataset(path = "data/npa/honhyo",
                partitioning = c("pref_code", "year", "month"))
usethis::use_git_ignore("*.parquet")

names(d_raw)

library(arrow)
d <- 
  arrow::open_dataset("data/npa/honhyo/",
                      schema = arrow::schema(
                        # 資料区分 = double(),
                        pref_code = utf8(),
                        datetime = arrow::timestamp(unit = "s"),
                        latitude = double(),
                        longitude = double())) |> 
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
