library(tidyverse)

# save quadrats
quadrats <- read_csv('data-raw/quad_info.csv')

devtools::use_data(quadrats, overwrite = T)
