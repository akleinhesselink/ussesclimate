library( tidyverse )
library( stringr )

clean_weather <- function( station_dat) {

  station_dat$date <-  as.POSIXct( strptime( station_dat$DATE, '%Y%m%d', tz = 'MST'))

  station_dat <-
    station_dat %>%
    select(date, TMIN, TMAX, PRCP)

  station_dat$PRCP[ station_dat$PRCP == -9999.0 ] <- NA
  station_dat$TMAX[ station_dat$TMAX == -9999.0 ] <- NA
  station_dat$TMIN[ station_dat$TMIN == -9999.0 ] <- NA

  return(station_dat)
}

weather <- read_csv('data-raw/USSES_climate.csv')
weather <- clean_weather(weather)

# save output to data folder
devtools::use_data(weather, overwrite = T)



