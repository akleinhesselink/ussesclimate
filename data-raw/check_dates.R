rm(list = ls())

library( ggplot2 )
library(tidyr)
library(dplyr)
library(zoo)

# output check dates as csv table.
# this table needs to be modified by hand to correct dates.

# input -------------------------------------------- #

df <- readRDS(file = 'data-raw/decagon_data.rds')

# ---------------------------------------------------#

df$date_started <- as.character ( levels( df$date_started )[df$date_started] )
df$date_started <- as.POSIXct( df$date_started, tz = 'MST' )
df$date_uploaded <- as.character(levels(df$date_uploaded)[ df$date_uploaded ])
df$date_uploaded <- as.POSIXct( df$date_uploaded, tz = 'MST')

reading_list <- df %>% ungroup () %>% select( f, plot, id , period, new_date, reading ) %>% mutate( f = factor(f)) %>% distinct()

table( reading_list$f, reading_list$period ) # one file per period

jumps <- reading_list %>%
  group_by(f ) %>%
  arrange( f , reading ) %>%
  mutate( time_numeric = as.numeric(new_date )) %>%
  mutate ( time_diff = c(NA, diff(time_numeric, 1 ))) %>%
  mutate( hours_skipped = time_diff/3600 - 2 ) %>%
  mutate( reading_diff = c(NA, diff(reading, 1))) %>%
  ungroup() %>%
  mutate( jump = ifelse( reading_diff == 1 & (hours_skipped != 0 ), 1, 0 )) %>%
  mutate( lead_jump = lead( jump, 1 ))

jumps %>% group_by ( f ) %>% summarise( n_jumps =  sum(jump, na.rm = T)) %>% filter ( n_jumps > 0  )

check <-
  jumps %>%
  select( f, new_date, reading, hours_skipped, reading_diff, jump ) %>%
  filter( jump > 0 , hours_skipped != 0 & reading_diff == 1 ) %>%
  filter( f != 'data/raw_soil_data/2015_2/EL5739 4Nov15-1838.txt') %>%
  filter( f != 'data/raw_soil_data/2015_2/EL5742 4Nov15-1820.txt') %>%
  filter( !( abs(hours_skipped) < 10000 & f == 'data/raw_soil_data/2015_2/EL5743 4Nov15-1828.txt')) %>%
  filter( f != 'data/raw_soil_data/2013_1/EM20070.txt') %>%
  filter( f != 'data/raw_soil_data/2013_1/EM20085.txt') %>%
  filter( !(f== 'data/raw_soil_data/2014_2/15_reordered.txt' & hours_skipped < 4 )) %>%
  arrange( new_date, f  )

# output -----------------------------------------------------------------------------------------

write.csv(check, 'data-raw/check_dates.csv', row.names = FALSE) # write list of changes

# determined for each jump whether it should be corrected or remain in place
# change = 1  indicates jumps that should be changed
# make changes on the csv file above

saveRDS(df, 'data-raw/decagon_data.rds')

