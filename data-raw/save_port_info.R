library(tidyverse)

# save sensor_positions aka port info
port_info <- read_csv('data-raw/sensor_positions.csv')

devtools::use_data(port_info, overwrite = T)
