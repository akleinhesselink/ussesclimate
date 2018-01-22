library(tidyverse)

seasons <- read_csv('data-raw/season_table.csv')

devtools::use_data(seasons, overwrite = T)
