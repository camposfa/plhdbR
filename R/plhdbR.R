#' plhdbR aims to facillitate the analysis of data in the PLHDB.
#'
#' It will have three main sets of utilities:
#'
#' \itemize{
#' \item Functions for reading and error-checking the life history and fertility data.
#' \item Functions for calculating vital rates.
#' \item Functions for loading and analyzing climate data.
#' }
#'
#' Additional functionality may be added in the future.
#'
#'
#' @docType package
#' @name plhdbR
#' @import stringr
#' @import tidyr
#' @import dplyr
#' @importFrom plyr mapvalues
#' @importFrom plyr revalue
#' @importFrom lubridate year
#' @importFrom lubridate ymd
#' @importFrom lubridate ymd_hms
#' @importFrom lubridate parse_date_time
#' @importFrom lubridate parse_date_time2
#' @importFrom lubridate new_interval
#' @importFrom lubridate %within%
NULL