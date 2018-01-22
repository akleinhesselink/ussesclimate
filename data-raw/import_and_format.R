require(tidyverse)
require(stringr)
require(zoo)

import_and_format <- function( folders, q_info, port_depth) {

  # This function reads in and exports the decagon soil moisture and temperature data
  # from the raw data files.

  # internal functions --------------------------------------------------------- #

  make_col_names <- function( x ) {

    port <- str_extract_all(x[1, ], pattern = '(Time)|(Port\\s[0-9])')
    probe_type <- str_extract( x[1, ] , pattern = '(ECT)|(EC\\-5)|(5TM)')
    measure <- str_extract( x [ 1, ] , pattern = '(VWC$)|((C$|C\\sTemp$))')

    new_names <- paste( port, measure , sep = '_')
    new_names <-  str_replace(string = new_names, pattern = '\\sTemp$', replacement = '')
    str_replace_all(string = new_names, pattern = c('_NA'), replacement = c(''))

  }


  rename_cols <- function(x ) {

    names(x) <- make_col_names( x )

    return(   x[-1, ] )
  }

  assign_NAs <- function( x ) {

    x [ x == '* * * '] <- NA

    return( x )
  }

  convert_time <- function(x) {

    return( strptime(x = x$Time, format = '%m/%d/%y %I:%M %p', tz = 'MST') )

  }


  make_date <- function(x) {

    x$date <- convert_time( x )

    x$date <- as.POSIXct(x$date, format = '%Y-%m-%d %H:%m:%s' )

    return(x)
  }

  make_readings <- function( x ) {

    m <- regexpr(row.names(x), pattern = '([0-9]+$)')

    x$reading <- as.numeric(regmatches(row.names(x), m))

    return( x )
  }


  gather_ports <- function ( x ) {
    x %>%
      gather( key = port, value = value ,  starts_with("Port") ) %>%
      separate(col = port, into = c('port', 'measure') , sep = '_')
  }

  get_files <- function( folder ){

    record_file <- dir(folder , pattern = 'logger_info.csv', full.names = TRUE)

    record <- read.csv(record_file)

    f <- dir(folder, pattern = '^E[ML][0-9]+.*txt$', full.names = TRUE)

    f_raw <- dir(folder, pattern = '^E[ML][0-9]+.*csv$', full.names = TRUE, recursive = TRUE)

    f2 <- dir(folder, pattern = '^[0-9]+(_[0-9]+_C)?.*txt$', full.names = TRUE)

    f2_raw <- dir(folder, pattern = '^[0-9]+(_[0-9]+_C)?.*csv$', full.names = TRUE, recursive = TRUE)

    f <- c(f, f_raw, f2_raw, f2)

    m_date <- file.mtime(f)

    attributes(m_date)$tzone <- 'MST'

    logger <- str_extract(basename(f), pattern = '(^E[ML][0-9]+)|(^[0-9]+(_[0-9]+_C)?)')

    file_df <-  data.frame( f, m_date, logger  )

    file_df$type <- str_extract( file_df$f, pattern = '.txt|.csv')

    file_df <- file_df %>%
      group_by( logger ) %>%
      mutate( modified_date = min(m_date )) %>%
      filter( type == '.txt') %>%
      select( - m_date )

    out <- lapply(as.character(file_df$f), read.table, sep = '\t', colClasses = 'character')

    names(out) <- file_df$logger

    out <- lapply(out, rename_cols)

    out <- lapply(out, assign_NAs)

    out <- lapply(out, make_date)

    out <- lapply(out, make_readings )

    out <- lapply( out, gather_ports )

    out <- do.call(rbind, out)

    out$id <- gsub( pattern = '\\.[0-9]+$', replacement = '', x = row.names(out))

    out <- merge(out, record, by.x = 'id', by.y = 'logger' )

    out <- merge(out, file_df, by.x = 'id', by.y = 'logger')

    out$value <- as.numeric(out$value)

    out <-  out %>%
      mutate( tail = ifelse ( is.na( tail ) , 2 , tail ), hours = ifelse(is.na(hours), 0, hours)) %>%
      filter( reading > tail ) %>%
      mutate( new_date = date - hours*60*60)

    return(out)
  }

  # end functions ------------------------------------------------------------- #
  temp_data <- lapply( folders,  get_files)

  temp_dat <- do.call( rbind, temp_data )  # bind the data lists from each folder

  temp_dat  <-
    temp_dat %>%
    group_by(plot , port , measure, reading , date, value) %>%
    arrange(period ) %>% filter( row_number() == 1  ) # when there are duplicate records get data from only the first file

  q_info$plot <- gsub( q_info$QuadName, pattern = 'X', replacement = '')

  temp_dat <- merge( temp_dat, q_info, by = 'plot')

  port_depth <- port_depth %>% gather( port, position, `Port 1`:`Port 5`)

  port_depth$depth <- str_extract( port_depth$position, pattern = '(air)|([0-9]+)')

  port_depth$port <- str_replace(port_depth$port, pattern = '\\.', replacement = ' ')

  temp_dat <- merge( temp_dat, port_depth, by = c('plot', 'period', 'port') )

  temp_dat$date_started <- as.character ( levels( temp_dat$date_started )[temp_dat$date_started] )
  temp_dat$date_started <- as.POSIXct( temp_dat$date_started, tz = 'MST' )
  temp_dat$date_uploaded <- as.character(levels(temp_dat$date_uploaded)[ temp_dat$date_uploaded ])
  temp_dat$date_uploaded <- as.POSIXct( temp_dat$date_uploaded, tz = 'MST')

  temp_dat <-
    temp_dat %>%
    select(plot, period, port, id, new_date, reading, measure, value,
           f, date_started, date_uploaded, modified_date,
           Treatment, PrecipGroup, quad, position, depth)

  return(temp_dat)
}

