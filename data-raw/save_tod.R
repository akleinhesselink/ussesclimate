library(tidyverse)

tod <- read_csv('data-raw/tod_table.csv')

devtools::use_data(tod, overwrite = T)
