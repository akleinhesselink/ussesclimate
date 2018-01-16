rm(list = ls())

library( ggplot2 )
library(tidyverse)
library(zoo)

fill_in_hours_skipped <- function( x ) {

  hs = 0

  for( i in 1:nrow(x)) {

    if (is.na( x$change[i] )) {

      x$hours_skipped[i] <- hs

    }else if(x$change[i] == 1 ){

      print(paste('old hs', hs ))

      hs <- x$hours_skipped[i] <- x$hours_skipped[i] + hs

      print(paste('new hs', hs))

    }else if(x$change[i] == 0 ){

      hs <- x$hours_skipped[i] <- 0 }
  }

  return( x )
}

# file input ---------------------------------------- #

season <- read.csv('data-raw/season_table.csv')
tod <- read.csv('data-raw/tod_table.csv')
check <- read.csv(file = 'data-raw/check_dates_modified.csv')  #  Read in the hand modified table
df <- readRDS('data-raw/decagon_data.rds')
# ----------------------------------------------------#

check$new_date <- as.POSIXct ( as.character( check$new_date ) , format = '%Y-%m-%d %H:%M:%S', tz = 'MST' )

df %>% filter( reading == 76) %>% select( date, new_date, Time, plot )  %>% distinct()

df <- left_join(df, check , by =c( 'f', 'new_date', 'reading' )) # join changes to main df

df <- df %>%
  ungroup () %>%
  group_by(f, plot, port, measure ) %>%
  arrange( reading ) %>%
  mutate( hours_skipped = ifelse( row_number() == 1 & is.na(change), 0, hours_skipped ))

out <- df %>%  do ( fill_in_hours_skipped(. ) ) # apply fill in hours function to all measurement groups

# actually make the date changes here ----------------------------------------------------------------------------------

out <- out %>%
  mutate( new_date = as.POSIXct(new_date - 60*60*hours_skipped, origin = '1970-01-01 00:00:00', tz = 'MST'))

# ----------------------------------------------------------------------------------------------------------------------
out <- out %>%
  mutate ( good_date = ifelse ( new_date >= date_started - 60*60*48 & new_date <= date_uploaded + 60*60*48 , 1, 0))

#out %>% ungroup() %>% distinct( f, new_date) %>% group_by(good_date) %>% summarise( n() )

#View( out %>% filter( good_date == 0 ) %>% group_by( f ) %>% distinct( f ) )

#out %>% filter( good_date == 0 ) %>% group_by(f ) %>% distinct(f) %>% select( date_started, new_date, date_uploaded) %>% mutate( new_date > date_started & new_date < date_uploaded  )

# check for readings from the same date, time and place # --------------------------------------------------------------

#out %>% group_by( plot, port, measure, new_date ) %>% mutate( n =  n() ) %>% filter( n > 1 )

# check earliest and latest dates -----------------------------------------------------------------

out %>% ungroup( ) %>% summarise ( max( new_date ), min( new_date ), which.min(new_date ), which.max(new_date ))

# ----------------------------------------------------------------------------

out <- out %>%
  ungroup() %>%
  mutate( simple_date = as.Date(new_date, tz = 'MST'),
          hour = strftime( new_date, '%H', tz = 'MST'),
          year = strftime( new_date, '%Y', tz = 'MST'),
          month = strftime( new_date, '%m', tz = 'MST'))

out$month <- as.numeric( out$month)
out$hour <- as.numeric( out$hour)

out <- merge( out, season, by = 'month')
out <- merge( out, tod, by = 'hour')

# output ----------------------------------------------------------------------------

saveRDS( out , 'data-raw/corrected_dates.rds')



