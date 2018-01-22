rm(list = ls())

library(tidyverse)
library(devtools)

quadrats <- read_csv('data-raw/quadrats.csv')
seasons <- read_csv('data-raw/seasons.csv')
tod <- read_csv('data-raw/tod.csv')
port_info <- read_csv('data-raw/port_info.csv')

devtools::use_data(quadrats, seasons, tod, port_info, overwrite = T)

source('data-raw/save_weather.R')
source('data-raw/clean_daily_soil_moisture.R')

