rm(list = ls())
save_weather <- function( ){
  library(tidyverse)
  library(devtools)
  library(stringr)

  # get weather data

  if( file.exists('data/weather.rda') ) {
    print( 'file exists, not downloading new data')
  }else{
    dubois_exp_stn_url <- "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00102707.dly"
    data_readme_url <- "https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/readme.txt"

    # each line is broken up into these sets of characters
    # matrix gives start and finish character
    char_mat <- matrix(
      c(sort(c(1,12,16,18, seq(22, 269, c(8)), seq(27, 269, 8), seq(28, 269, 8), seq(29, 269, 8))),
        sort(c(11,15,17,21, seq(22 + 4, 269, c(8)), seq(27, 269, 8), seq(28, 269, 8), seq(29, 269, 8)))),
      ncol = 2)

    vnames <- paste(c('VALUE', 'MFLAG', 'QFLAG', 'SFLAG'), sort(rep(1:31, 4)), sep = '')
    col_names <- c('ID', 'YEAR', 'MONTH', 'ELEMENT', vnames)

    weather <- as.list( readLines( dubois_exp_stn_url) )
    weather <- lapply( weather, str_sub, char_mat)

    weather <- data.frame( do.call(rbind, weather) )
    names(weather) <- col_names

    weather <-
      weather %>%
      select( YEAR, MONTH, ELEMENT, starts_with('VALUE')) %>%
      gather(day, value, starts_with('VALUE')) %>%
      mutate( day = str_extract( day, '\\d+')) %>%
      mutate( value = as.numeric(value)) %>%
      mutate( value = ifelse( value == -9999, NA, value)) %>%
      mutate( value = ifelse(ELEMENT %in% c('TMIN', 'TMAX', 'PRCP'), value/10, value)) %>%
      mutate( YEAR = as.numeric(levels(YEAR)[YEAR]), MONTH = as.numeric(MONTH), day = as.numeric(day)) %>%
      arrange( ELEMENT, YEAR, MONTH, day)

    weather <-
      weather %>%
      filter( ELEMENT %in% c('PRCP', 'TMIN', 'TMAX'))

    weather <-
      weather %>%
      mutate( date = paste( YEAR, MONTH, day, sep = '-')) %>%
      mutate( date = as.Date( date, '%Y-%m-%d')) %>%
      filter( ! is.na(date)) %>%
      select( date, ELEMENT, value)

    # save output to data folder
    devtools::use_data(weather, overwrite = T)
  }
}

save_weather()
