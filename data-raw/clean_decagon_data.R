rm(list = ls())

library(tidyverse)
library(stringr)
library(zoo)

source( 'data-raw/import_and_format_decagon_data.R')
source( 'data-raw/correct_dates.R' )
source( 'data-raw/correct_values.R')
source( 'data-raw/merge_with_climate.R')

# input -------------------------------------------- #

q_info <- read.csv('data-raw/quad_info.csv')
port_depth <- read.csv('data-raw/sensor_positions.csv')
season <- read.csv('data-raw/season_table.csv')
tod <- read.csv('data-raw/tod_table.csv')
folders <- dir('data-raw/raw_soil_data', pattern = '20[0-9]{2}_[1-2]$', full.names = TRUE)
station_dat <- read.csv('data-raw/USSES_climate.csv')

# ---------------------------------------------------#
soil_moisture_data <- import_and_format(folders, q_info, port_depth)

check_dates(soil_moisture_data)
# Modify check dates.csv  by hand
check <- read_csv('data-raw/check_dates_modified.csv') # read in file modified by hand

soil_moisture_data %>% filter( reading == 76) %>% select( date, new_date, Time, plot )  %>% distinct()

soil_moisture_data <-
  correct_dates(soil_moisture_data, check, season, tod) %>%
  correct_values() %>%
  merge_with_climate(station_dat = station_dat)

# save processed and cleaned data -------------
devtools::use_data(soil_moisture_data, compress = 'gzip')


