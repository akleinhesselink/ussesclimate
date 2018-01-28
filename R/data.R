#' Daily weather data from the US Sheep Experiment Station
#'
#' A dataset containing the date, the daily max, min and mean
#' mean temperatures, and the total rainfall and snowfall for
#' that date. Data recorded at the US Sheep Experiment Station
#' weather station GHCND:USC00102707
#'
#'
#' @format A data frame with 101783 rows and 3 variables:
#' \describe{
#'   \item{date}{date in \%Y-\%m-\%d format}
#'   \item{ELEMENT}{weather variable: PRCP, TMAX, TMIN}
#'   \item{value}{value of weather variable, PRCP mm, TMAX (0.1 C), TMIN (0.1 C)}
#' }
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00102707.dly}
"weather"


#' Daily soil moisture data from the US Sheep Experiment Station
#'
#' A dataset containing the average soil moisture in 12 study plots
#' at the US Sheep Experiment Station.  Each plot received either
#' ambient precipitation, a 50% increase in precipitation, or a 50%
#' decrease in precipitation. Average soil moisture is calculated
#'
#'
#' @format A data frame with 958918 rows and 35 variables:
#' \describe{
#'   \item{date}{plot unique plot}
#'   \item{period}{monitoring period}
#'   \item{port}{decagon datalogger port number}
#'   \item{id}{ decagon datalogger id}
#'   \item{new_date} datetime in \%Y-\%m-\%d \%H:\%M:\%s format
#'   \item{reading} value
#'   \item{measure} variable type, C (temperature in degree C), volumetric water content (VWC)
#'   \item{f}{original filename}
#'   \item{date_started}{start date for period}
#'   \item{date_uploaded}{upload date for period }
#'   \item{modified_date}{ date modified}
#'   \item{Treatment}{ Treatment level irrigation, drought, control}
#'   \item{PrecipGroup}{ plot group, each plot group contains drought, irrigation and ambient plots}
#'   \item{quad}{unique quadrat id}
#'   \item{position}{ position of decagon probe}
#'   \item{depth} { soil depth of decagon probe}
#'   \item{good_date}{ factor indicating whether date is good or bad}
#'   \item{ simple_date}{date}
#'   \item{hour}{hour}
#'   \item{year}{year}
#' }
#' @source \url{https://www1.ncdc.noaa.gov/pub/data/ghcn/daily/all/USC00102707.dly}
"weather"


