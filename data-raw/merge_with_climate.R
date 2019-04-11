require(tidyverse)
require(stringr)
require(zoo)


merge_with_climate <- function(mydat, station_dat){

  station_dat <-
    station_dat %>%
    spread( ELEMENT, value) %>%
    mutate( TMEAN = ( TMAX + TMIN ) / 2 ) %>%
    filter(date > '2011-01-01', date < '2017-01-01') %>%
    select(date, PRCP, TMEAN)

  station_dat$PRCP[ is.na(station_dat$PRCP) ] <- 0

  station_dat <-
    station_dat %>%
    mutate( rainfall = rollapply(PRCP, 2, sum, fill = 0, na.rm = TRUE, align = 'right') ) %>%
    mutate( rainfall = ifelse( rainfall > 0.0 & TMEAN > 3 & !is.na(rainfall), 'rainy', 'not rainy'))

  # create a factor listing each rainy period, including the day before the rain
  station_dat <-
    station_dat %>%
    arrange( desc(date) ) %>%
    mutate( prerain = lag( rainfall, 1) ) %>%
    mutate( prerain = ifelse( prerain == 'rainy' & rainfall == 'not rainy', TRUE, FALSE)) %>%
    arrange( date) %>%
    mutate( prcp_event = factor( cumsum ( prerain ) )) %>%
    group_by( prcp_event, prerain) %>%
    mutate( total_rain = cumsum(PRCP) )

  station_dat <-
    station_dat %>%
    ungroup() %>%
    mutate( simple_date = as.Date( date, tz = 'MST')) %>%
    mutate( year = strftime( simple_date, '%Y', tz = 'MST')) %>%
    group_by( year ) %>%
    arrange( year, simple_date ) %>%
    mutate( ann_cum_PRCP = cumsum(PRCP))

  # clean-up decagon data -------------------

  mydat$depth_label <- factor( mydat$depth , levels = c('air temperature', '5 cm deep', '25 cm deep') , order = TRUE )
  mydat$Treatment_label <- factor(mydat$Treatment, levels = c('Drought', 'Control', 'Irrigation'), order = TRUE)

  mydat <- mydat %>%
    mutate ( unique_position = paste0( plot, '.', position))

  mydat$datetime <- mydat$new_date

  station_dat$simple_date <- as.Date( station_dat$date, tz = 'MST')

  mydat <- mydat %>% left_join( station_dat, by = c('year','simple_date'))

  return(mydat)

}

load('data/daily_soil_moisture.rda')
load('data/weather.rda')

weather <-
  weather %>%
  spread( ELEMENT, value) %>%
  mutate( TMEAN = ( TMAX + TMIN ) / 2 ) %>%
  filter(date > '2011-01-01', date < '2017-01-01') %>%
  select(date, PRCP, TMEAN)

weather$PRCP[ is.na(weather$PRCP) ] <- 0

weather <-
  weather %>%
  mutate( rainfall = rollapply(PRCP, 2, sum, fill = 0, na.rm = TRUE, align = 'right') ) %>%
  mutate( rainfall = ifelse( rainfall > 0.0 & TMEAN > 3 & !is.na(rainfall), 'rainy', 'not rainy'))

# create a factor listing each rainy period, including the day before the rain

test <-
  weather %>%
  arrange( desc(date) ) %>%
  mutate( prerain = lag( rainfall, 1) ) %>%
  mutate( prerain = ifelse( prerain == 'rainy' & rainfall == 'not rainy', TRUE, FALSE)) %>%
  arrange( date)  %>%
  mutate( prcp_event =  cumsum(prerain))

test$`cumsum(prerain)`

mutate( prcp_event = factor( cumsum ( prerain ) )) %>%
  group_by( prcp_event, prerain) %>%
  mutate( total_rain = cumsum(PRCP) )

station_dat <-
  station_dat %>%
  ungroup() %>%
  mutate( simple_date = as.Date( date, tz = 'MST')) %>%
  mutate( year = strftime( simple_date, '%Y', tz = 'MST')) %>%
  group_by( year ) %>%
  arrange( year, simple_date ) %>%
  mutate( ann_cum_PRCP = cumsum(PRCP))

# clean-up decagon data -------------------

mydat$depth_label <- factor( mydat$depth , levels = c('air temperature', '5 cm deep', '25 cm deep') , order = TRUE )
mydat$Treatment_label <- factor(mydat$Treatment, levels = c('Drought', 'Control', 'Irrigation'), order = TRUE)

mydat <- mydat %>%
  mutate ( unique_position = paste0( plot, '.', position))

mydat$datetime <- mydat$new_date

station_dat$simple_date <- as.Date( station_dat$date, tz = 'MST')

mydat <- mydat %>% left_join( station_dat, by = c('year','simple_date'))

