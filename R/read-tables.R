#' Read biography table from plhdb.org csv export.
#'
#' Returns a tbl_df.
#'
#' @param f Name of the csv file containing biography data. Do not edit the file manually after downloading.
#' @export
#' @examples
#' # Assuming your file is called "biography_2015_03_17.csv"
#' lh <- read_bio_table("data/biography_2015_03_17.csv")
read_bio_table <- function(f){

  # file_ext function from tools package
  file_ext <- function (x)
  {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
  }

  if(file_ext(f) != "csv"){
    message("Unrecognized file. Create a query at plhdb.org (e.g., Study Id != '10'), use the download button, and use that csv file here.")
    return(NULL)
  }

  lh_names <- read.delim(f, sep = ",", skip = 5, na.strings = "",
                         encoding = "latin1", nrows = 1, header = TRUE)
  lh <- dplyr::tbl_df(read.delim(f, sep = ",", skip = 8, header = FALSE,
                                 encoding = "latin1", na.strings = ""))
  names(lh) <- names(lh_names)

  # Convert date fields to date/time understood by R
  lh <- lh %>%
    dplyr::mutate(Birth.Date = lubridate::parse_date_time(Birth.Date, "%d-%b-%y"),
                  Min.Birth.Date = lubridate::parse_date_time(Min.Birth.Date, "%d-%b-%y"),
                  Max.Birth.Date = lubridate::parse_date_time(Max.Birth.Date, "%d-%b-%y"),
                  Entry.Date = lubridate::parse_date_time(Entry.Date, "%d-%b-%y"),
                  Depart.Date = lubridate::parse_date_time(Depart.Date, "%d-%b-%y"))

  # Change
  lh$Study.Id <- factor(as.character(lh$Study.Id))
  lh$Study.Id <- plyr::revalue(lh$Study.Id,
                               c("1" = "rppn-fma", "2" = "amboseli",
                                 "3" = "kakamega", "4" = "gombe",
                                 "5" = "karisoke", "6" = "beza",
                                 "7" = "ssr"))

  # RPPN data has animal with code "NA", which messes with R
  lh$Animal.Id <- factor(lh$Animal.Id, exclude = NULL)

  return(lh)
}

#' Read fertility table from plhdb.org csv export.
#'
#' Returns a tbl_df.
#'
#' @param f Name of the csv file containing fertility data. Do not edit the file manually after downloading.
#' @export
#' @examples
#' # Assuming your file is called "fertility_2015_03_17.csv"
#' lh <- read_fert_table("fertility_2015_03_17.csv")
read_fert_table <- function(f){

  # file_ext function from tools package
  file_ext <- function (x)
  {
    pos <- regexpr("\\.([[:alnum:]]+)$", x)
    ifelse(pos > -1L, substring(x, pos + 1L), "")
  }

  if(file_ext(f) != "csv"){
    message("Unrecognized file. Create a query at plhdb.org (e.g., Study Id != '10'), use the download button, and use that csv file here.")
    return(NULL)
  }

  fert_names <- read.delim(f, sep = ",", skip = 5,
                           encoding = "latin1", nrows = 1, header = TRUE,
                           na.strings = "")
  fert <- dplyr::tbl_df(read.delim(f, sep = ",", skip = 8, header = FALSE,
                                   encoding = "latin1", na.strings = ""))
  names(fert) <- names(fert_names)

  fert <- fert %>%
    dplyr::mutate(Start.Date = lubridate::parse_date_time(Start.Date, "%d-%b-%y"),
                  Stop.Date = lubridate::parse_date_time(Stop.Date, "%d-%b-%y"))

  fert$Study.Id <- factor(as.character(fert$Study.Id))
  fert$Study.Id <- plyr::revalue(fert$Study.Id,
                                 c("1" = "rppn-fma", "2" = "amboseli",
                                   "3" = "kakamega", "4" = "gombe",
                                   "5" = "karisoke", "6" = "beza",
                                   "7" = "ssr"))

  # RPPN data has animal with code "NA", which messes with R
  fert$Animal.Id <- factor(fert$Animal.Id, exclude = NULL)

  return(fert)
}