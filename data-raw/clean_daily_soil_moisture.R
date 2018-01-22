rm(list = ls())

clean_soil_moisture <- function(){

  library(devtools)
  library(tidyverse)
  library(stringr)
  library(zoo)

  if( file.exists('data/daily_soil_moisture.rda') ){
    print( 'file exists, not processing data')
  }else{

    # source all functions ------------------------------ #
    source( 'data-raw/import_and_format.R')
    source( 'data-raw/correct_dates.R' )
    source( 'data-raw/clean_values.R')
    source( 'data-raw/merge_with_climate.R')

    # input -------------------------------------------- #
    load('data/weather.rda')
    load('data/port_info.rda')
    load('data/quadrats.rda')

    folders <- dir('data-raw/raw_soil_data', pattern = '20[0-9]{2}_[1-2]$', full.names = TRUE)

    # process data ---------------------------------------------------#

    daily_soil_moisture <-
      import_and_format(folders, quadrats, port_info)

    check_dates(daily_soil_moisture) # output check dates file
    # Modify check dates.csv  by hand and then read from csv
    check <- read_csv('data-raw/check_dates_modified.csv') # read in file modified by hand

    daily_soil_moisture <-
      daily_soil_moisture %>%
      correct_dates(check = check) %>%
      clean_values()

    daily_soil_moisture <-
      daily_soil_moisture %>%
      merge_with_climate(station_dat = weather)

    devtools::use_data(daily_soil_moisture, compress = 'gzip', overwrite = T)
  }
}

clean_soil_moisture()
