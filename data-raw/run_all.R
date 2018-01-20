rm(list = ls())

library(devtools)
library(tidyverse)
library(stringr)
library(zoo)

# source all functions ------------------------------ #
source( 'data-raw/import_and_format.R')
source( 'data-raw/correct_dates.R' )
source( 'data-raw/correct_values.R')
source( 'data-raw/merge_with_climate.R')

# input -------------------------------------------- #

quadrats <- read.csv('data-raw/quad_info.csv')
port_info <- read.csv('data-raw/sensor_positions.csv')
seasons <- read.csv('data-raw/season_table.csv')
tod <- read.csv('data-raw/tod_table.csv')
folders <- dir('data-raw/raw_soil_data', pattern = '20[0-9]{2}_[1-2]$', full.names = TRUE)
weather <- read.csv('data-raw/USSES_climate.csv')

# ---------------------------------------------------#
daily_soil_moisture <- import_and_format(folders, quadrats, port_depth)

check_dates(daily_soil_moisture)
# Modify check dates.csv  by hand
check <- read_csv('data-raw/check_dates_modified.csv') # read in file modified by hand

daily_soil_moisture %>% filter( reading == 76) %>% select( date, new_date, Time, plot )  %>% distinct()

daily_soil_moisture <-
  daily_soil_moisture %>%
  correct_dates(check = check, season = seasons, tod = tod) %>%
  correct_values() %>%
  merge_with_climate(station_dat = weather)

# save processed and cleaned soil moisture data -------------
devtools::use_data(daily_soil_moisture, compress = 'gzip')

# save other files ------------------------------------------

devtools::use_data(check, seasons, tod, weather, port_info, quadrats)


