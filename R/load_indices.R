#' Obtain data on large-scale climate oscillation indices.
#'
#' Returns a named list of dplyr::tbl_df objects.
#'
#' @param index One or more climate indices.
#' Supported indices include.
#' \itemize{
#' \item Dipole Mode index ("dmi")
#' \item Multivariate ENSO Index ("mei")
#' \item Oceanic Niño Index ("oni")
#' \item Southern Oscillation Index ("soi")
#' \item Pacific Decadal Oscillation ("pdo")
#' \item Atlantic Multidecadal Oscillation ("amo")
#' \item North Atlantic Oscillation ("nao")
#' }
#' @export
#' @examples
#' indices <- load_climate_index(c("mei", "oni"))
load_climate_index <- function(index){

#   test_packages <- (requireNamespace("dplyr", quietly = TRUE) & requireNamespace("stringr", quietly = TRUE) &
#                       requireNamespace("tidyr", quietly = TRUE) & requireNamespace("lubridate", quietly = TRUE) &
#                       requireNamespace("plyr", quietly = TRUE))
#
#   if(!test_packages){
#     message("Please load the required packages first with load_plhdb_packages()")
#     return(NULL)
#   }

  `%ni%` = Negate(`%in%`)

  res <- list()

  if("dmi" %in% index){
    message("Reading DMI data from http://www.jamstec.go.jp/frcgc/research/d1/iod/DATA/dmi.monthly.txt")
    dmi <- dplyr::tbl_df(read.table("http://www.jamstec.go.jp/frcgc/research/d1/iod/DATA/dmi.monthly.txt",
                                    header = TRUE))
    names(dmi) <- c("date_of", "west", "east", "value")

    if(nrow(dmi) > 0){
      dmi <- dmi %>%
        dplyr::mutate(date_of = lubridate::parse_date_time2(as.character(date_of), "Y:m:d:H"),
                      index = "dmi") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["dmi"]] <- dmi
    }
    else{
      message("Error reading file.")
    }
  }

  if("mei" %in% index){

    message("Reading MEI data from http://www.esrl.noaa.gov/psd/enso/mei/table.html")
    # Starts at 1950
    # Read in current year - 1950 + 1 lines
    mei <- dplyr::tbl_df(read.table("http://www.esrl.noaa.gov/psd/enso/mei/table.html",
                                    skip = 12, nrows = lubridate::year(Sys.Date()) - 1950 + 1,
                                    header = TRUE, fill = TRUE))

    if(nrow(mei) > 0){
      mei <- tidyr::gather(mei, bimonth, mei, -YEAR)

      mei <- mei %>%
        dplyr::rename(year_of = YEAR, value = mei) %>%
        dplyr::mutate(date_of = substr(mei$bimonth, start = 4, stop = 6),
                      date_of = lubridate::ymd(paste(year_of, date_of, "01", sep = "-")),
                      index = "mei") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["mei"]] <- mei
    }
    else{
      message("Error reading file")
    }
  }

  if("oni" %in% index){

    message("Reading ONI data from http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt")
    oni <- dplyr::tbl_df(read.table("http://www.cpc.ncep.noaa.gov/data/indices/oni.ascii.txt", header = TRUE))

    if(nrow(oni) > 0){
      oni$SEAS <- plyr::mapvalues(oni$SEAS,
                            from = c("AMJ", "ASO", "DJF", "FMA",
                                     "JAS", "JFM", "JJA", "MAM",
                                     "MJJ", "NDJ", "OND", "SON"),
                            to = c("5", "9", "1", "3",
                                   "8", "2", "7", "4",
                                   "6", "12", "11", "10"))

      oni <- oni %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(YR, SEAS, "16", sep = "-")),
                      value = ANOM,
                      index = "oni") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["oni"]] <- oni
    }
  }

  if("soi" %in% index){

    message("Reading SOI data from ftp://ftp.bom.gov.au/anon/home/ncc/www/sco/soi/soiplaintext.html")
    # Starts in 1876
    soi <- dplyr::tbl_df(read.delim("ftp://ftp.bom.gov.au/anon/home/ncc/www/sco/soi/soiplaintext.html",
                                    skip = 12, fill = TRUE,
                                    nrows = lubridate::year(Sys.Date()) - 1876 + 1))

    if(nrow(soi) > 0){
      soi <- dplyr::select(soi, -X)
      soi <- suppressWarnings(tidyr::gather(soi, month_of, soi, -Year))

      soi <- soi %>%
        dplyr::filter(soi != "*") %>%
        dplyr::mutate(date_of = lubridate::ymd(paste(Year,
                                                     as.numeric(month_of),
                                                     "16", sep = "-")),
                      value = as.numeric(soi),
                      index = "soi") %>%
        dplyr::filter(!is.na(value)) %>%
        dplyr::arrange(date_of) %>%
        dplyr::select(date_of, value, index)

      res[["soi"]] <- soi
    }
  }

  if("pdo" %in% index){

    message("Reading PDO data from http://jisao.washington.edu/pdo/PDO.latest")

    # Starts 1900
    pdo <- dplyr::tbl_df(read.table("http://jisao.washington.edu/pdo/PDO.latest",
                             skip = 29, header = TRUE, fill = TRUE,
                             nrows = lubridate::year(Sys.Date()) - 1900 + 1))

    pdo$YEAR <- stringr::str_replace_all(pdo$YEAR, stringr::fixed("*"), "")
    pdo <- tidyr::gather(pdo, month_of, value, -YEAR)
    pdo <- pdo %>%
      dplyr::mutate(date_of = lubridate::ymd(paste(YEAR,
                                                   as.numeric(month_of),
                                                   "16", sep = "-")),
                    value = as.numeric(value),
                    index = "pdo") %>%
      dplyr::filter(!is.na(value)) %>%
      dplyr::arrange(date_of) %>%
      dplyr::select(date_of, value, index)

    res[["pdo"]] <- pdo
  }

 if("amo" %in% index){

   message("Reading AMO data from http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data")

   # Starts 1948
   amo <- dplyr::tbl_df(read.table("http://www.esrl.noaa.gov/psd/data/correlation/amon.us.data",
                                   skip = 1, fill = TRUE, nrows = lubridate::year(Sys.Date()) - 1948 + 1))

   names(amo)[2:13] <- month.abb
   amo <- tidyr::gather(amo, month_of, value, -V1)
   amo <- amo %>%
     dplyr::mutate(date_of = lubridate::ymd(paste(V1,
                                                  as.numeric(month_of),
                                                  "16", sep = "-")),
                   value = as.numeric(value),
                   index = "amo") %>%
     dplyr::filter(value != -99.990) %>%
     dplyr::arrange(date_of) %>%
     dplyr::select(date_of, value, index)

   res[["amo"]] <- amo
 }

 if("nao" %in% index){

   message("Reading NAO data from http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table")

   nao <- dplyr::tbl_df(read.table("http://www.cpc.ncep.noaa.gov/products/precip/CWlink/pna/norm.nao.monthly.b5001.current.ascii.table",
                                   fill = TRUE))

   names(nao)[2:13] <- month.abb
   nao <- tidyr::gather(nao, month_of, value, -V1)
   nao <- nao %>%
     dplyr::mutate(date_of = lubridate::ymd(paste(V1,
                                                  as.numeric(month_of),
                                                  "16", sep = "-")),
                   value = as.numeric(value),
                   index = "nao") %>%
     dplyr::filter(!is.na(value)) %>%
     dplyr::arrange(date_of) %>%
     dplyr::select(date_of, value, index)

   res[["nao"]] <- nao
 }

 if(length(res) < length(index)){
   known_indices <- c("dmi", "mei", "oni", "soi", "pdo", "amo", "nao")
   errors <- index[which(index %ni% known_indices)]
   message(paste("Unknown indices:" , paste(errors, collapse=", "), sep = " "))
 }

 if(length(res) == 0){
   res = NULL
 }

  return(res)

}