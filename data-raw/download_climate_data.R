rm(list = ls())
library(tidyverse)

# input --------------------------------------------------- #
dubois_exp_stn_url <- "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00102707.dly"
data_readme_url <- "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt"

# output -------------------------------------------------- #

write_lines( read_lines(dubois_exp_stn_url), path = 'data-raw/usses_climate.txt')
write_lines( read_lines(data_readme_url), path = 'data-raw/climate_readme.txt')
