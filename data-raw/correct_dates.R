
check_dates <- function( temp_dat ) {
  reading_list <- temp_dat %>% ungroup () %>% select( f, plot, id , period, new_date, reading ) %>% mutate( f = factor(f)) %>% distinct()

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

  write_csv(check, 'data-raw/check_dates.csv') # write list of changes

  # determined for each jump whether it should be corrected or remain in place
  # change = 1  indicates jumps that should be changed
  # make changes on the csv file above
}



correct_dates <- function(temp_dat, check){

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

  check$new_date <- as.POSIXct ( as.character( check$new_date ) , format = '%Y-%m-%d %H:%M:%S', tz = 'MST' )

  temp_dat <- left_join(temp_dat, check , by =c( 'f', 'new_date', 'reading' )) # join changes to main df

  temp_dat <-
    temp_dat %>%
    ungroup() %>%
    group_by(f, plot, port, measure ) %>%
    arrange( reading ) %>%
    mutate( hours_skipped = ifelse( row_number() == 1 & is.na(change), 0, hours_skipped ))

  print("Correcting dates: ")
  temp_dat <- temp_dat %>%  do ( fill_in_hours_skipped(. ) ) # apply fill in hours function to all measurement groups

  # actually make the date changes here ----------------------------------------------------------------------------------

  temp_dat <-
    temp_dat %>%
    mutate( new_date = as.POSIXct(new_date - 60*60*hours_skipped, origin = '1970-01-01 00:00:00', tz = 'MST'))

  # ----------------------------------------------------------------------------------------------------------------------
  temp_dat <-
    temp_dat %>%
    mutate ( good_date = ifelse ( new_date >= date_started - 60*60*48 & new_date <= date_uploaded + 60*60*48 , 1, 0)) %>%
    filter( good_date == 1)

  #temp_dat %>% ungroup() %>% distinct( f, new_date) %>% group_by(good_date) %>% summarise( n() )

  #View( temp_dat %>% filter( good_date == 0 ) %>% group_by( f ) %>% distinct( f ) )

  #temp_dat %>% filter( good_date == 0 ) %>% group_by(f ) %>% distinct(f) %>% select( date_started, new_date, date_uploaded) %>% mutate( new_date > date_started & new_date < date_uploaded  )

  # check for readings from the same date, time and place # --------------------------------------------------------------

  #temp_dat %>% group_by( plot, port, measure, new_date ) %>% mutate( n =  n() ) %>% filter( n > 1 )

  # check earliest and latest dates -----------------------------------------------------------------

  temp_dat %>%
    ungroup( ) %>%
    summarise ( max( new_date ), min( new_date ), which.min(new_date ), which.max(new_date ))

  # ----------------------------------------------------------------------------

  temp_dat <- temp_dat %>%
    ungroup() %>%
    mutate( simple_date = as.Date(new_date, tz = 'MST'),
            hour = strftime( new_date, '%H', tz = 'MST'),
            year = strftime( new_date, '%Y', tz = 'MST'),
            month = strftime( new_date, '%m', tz = 'MST'))

  temp_dat$month <- as.numeric( temp_dat$month)
  temp_dat$hour <- as.numeric( temp_dat$hour)

  temp_dat <-
    temp_dat %>%
    select( - c(hours_skipped, reading_diff, jump, change ))

  return( temp_dat )

}

