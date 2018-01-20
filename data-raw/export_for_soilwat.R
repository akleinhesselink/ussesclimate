export_soil_moisture_for_soilwat <- function(soil, output_folder) {

  ###
  # Save the soil moisture data in multiple csv files
  # These can be used to calibrate a site specific soilwat model
  ###

  if(!dir.exists(output_folder)){ dir.create(output_folder)}

  soil_export <-
    soil %>%
    filter( measure == 'VWC',
            stat == 'raw',
            bad_values == 0,
            Treatment == 'Control') %>%
    mutate( depth = str_extract( depth, pattern = '[0-9]+' )) %>%
    dplyr::select(Treatment, plot, port, position, depth, new_date, v)

  port_labels <- data.frame( position = rev( unique( soil_export$position) ), SMS_label = paste0('VWC_L', 1:4), `depth (cm)` = c(5,5, 25,25 ) )

  temp <- soil_export %>% left_join(port_labels, by = 'position') %>% ungroup()

  temp <- temp %>% select( plot, Treatment, new_date, v, SMS_label )

  temp_avg <-
    temp %>%
    mutate( old_date = new_date) %>%
    mutate( date = as.Date( old_date, '%Y-%m-%d', tz = 'MST'), DOY  = strftime( old_date, '%j'), year = strftime( old_date , '%Y') ) %>%
    group_by( plot, date, Treatment, year, DOY, SMS_label) %>%
    summarise( VWC = mean(v, na.rm = TRUE), n = n()) %>%
    ungroup() %>%
    spread(  SMS_label, VWC) %>%
    rename( Date = date)

  all_dates <- data.frame( date = seq.POSIXt(strptime( '2012-01-01', '%Y-%m-%d', tz = 'MST'), strptime( '2016-12-31', '%Y-%m-%d', tz = 'MST'), by = 24*3600) )

  all_dates <- expand.grid( plot = unique( temp_avg$plot), Date = as.Date( all_dates$date, tz = 'MST'))

  temp_avg <- left_join(all_dates, temp_avg , by = c('plot', 'Date')) %>% mutate( doy = as.numeric( strftime( Date, '%j', 'MST') ))

  out_list <- temp_avg %>% dplyr::select(plot, Date, doy, starts_with( 'VWC'))

  out_list <- split( out_list, out_list$plot )

  # for( i in 1:length(out_list)) {
  #   print( ggplot ( out_list[[i]]  %>% gather( pos, v, VWC_L1:VWC_L4) %>% filter( v > -9999) , aes( x = Date, y = v, color = pos )) +
  #     geom_line() +
  #     ylim( -0.1, 0.50  ) +
  #     ggtitle(names(out_list)[i]) )
  # }

  for( i in 1:length(out_list) ) {
    fname <- paste0("USSES_", names(out_list)[i], '_SoilWater.csv')
    write_csv(out_list[[i]], file.path(output_folder, fname))
  }

  sms_labels <- data.frame(Label = c('USSES_11_12_C',
                                     'USSES_1_2_C',
                                     'USSES_15_16_C',
                                     'USSES_7_8_C'),
                           SCANInstallation_Number = 1 ,
                           SMS_Number = 2,
                           SMS1 = NA,
                           SMS2 = NA,
                           SMS3 = NA,
                           SMS4 = NA )

  write_csv( sms_labels, file.path(output_folder, 'FieldSensors_MappedTo_SoilWatLayers.csv'))
  write_csv( port_labels, file.path(output_folder, 'port_info.csv'))
}


export_weather_for_soilwat <- function(station_dat, output_folder) {

  ###
  # Save the local weather data in multiple csv files
  # These can be used to calibrate a site specific soilwat model
  ###

  # function def -----------------
  write_with_header <- function(x, file, header, f = write.table, ...){

    datafile <- file(file, open = 'wt')

    on.exit(close(datafile))

    if(!missing(header)) writeLines(header,con=datafile)

    f(x, datafile,...)
  }

  make_header <- function( prefix, df, station, year) {

    paste0( '#', prefix, station, ' year = ', year, '\n#', 'DOY', ' ', 'Tmax(C)', ' ', 'Tmin(C)', ' ', 'PPT(cm)')

  }

  # --------------------------------

  if(!dir.exists(output_folder)){ dir.create(output_folder)}

  station_dat$date <- as.POSIXct( strptime( station_dat$DATE, format = '%Y%m%d', tz = 'MST')    )

  station_dat <- station_dat %>% select( date, STATION, STATION_NAME, LATITUDE, LONGITUDE, ELEVATION, PRCP, SNWD, SNOW, TMAX, TMIN )

  station_dat <- station_dat %>% mutate( LONGITUDE = LONGITUDE[which.max(date)], LATITUDE = LATITUDE[ which.max(date )], ELEVATION = ELEVATION[ which.max(date )] )

  head(station_dat)

  station_dat$year <- strftime(station_dat$date, '%Y')
  station_dat$DOY <- as.numeric( strftime( station_dat$date, '%j'))
  station_dat$PPT <- station_dat$PRCP/10

  station_dat %>% group_by(year)  %>% filter( year > 1980 , year < 2011, PPT >= 0 ) %>% summarise(AP = sum(PPT, na.rm = T)) %>% summarise( MAP = mean(AP))

  df <- expand.grid( DOY = 1:365 , year = c(min(station_dat$year):max(station_dat$year)))

  station_dat <- merge( df,station_dat, by = c('year', 'DOY'), all.x = T, all.y = T)

  year_list <- split( station_dat[ , c('DOY', 'TMAX', 'TMIN', 'PPT') ], station_dat$year)

  for ( i in 1:length( year_list) ) {

    temp_df <- year_list[[i]]
    temp_year <- names(year_list)[[i]]

    temp_fname <- file.path( output_folder, paste0( 'weath.', temp_year) )
    temp_header <- make_header(prefix = 'weather for site ', df = temp_df, station = 'US Sheep Experiment Station', year = temp_year )

    write_with_header( x = temp_df, file = temp_fname, header = temp_header, f = write.table, sep = '\t', col.names = FALSE, row.names = FALSE)
  }
}
