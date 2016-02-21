source("http://bioconductor.org/biocLite.R")
biocLite("rhdf5")

library(rhdf5)

# FTP
# Download the files within a directory.
if (interactive() && url.exists('ftp://gpm1.gesdisc.eosdis.nasa.gov')) {

  url <-'ftp://gpm1.gesdisc.eosdis.nasa.gov/data/s4pa/GPM_L3/GPM_3IMERGM.03/'
  dirnames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)

  dirnames <- paste(url, strsplit(dirnames, "\r*\n")[[1]], sep = "")
  con <- getCurlHandle(ftp.use.epsv = FALSE)
  dirnames <- dirnames[!str_detect(dirnames, ".xml")]

  gpm <- list()

  for (i in 1:length(dirnames)){

    url <- paste(dirnames[i], "/", sep = "")
    filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
    filenames <- strsplit(filenames, "\r*\n")[[1]]
    filenames <- filenames[!str_detect(filenames, ".xml")]

    for (j in 1:length(filenames)){
      download.file(paste(dirnames[i], filenames[j], sep = "/"),
                    destfile = paste("data/grids/GPM/", filenames[j], sep = ""))
    }

  }
}

# Downloaded from ftp://gpm1.gesdisc.eosdis.nasa.gov/data/s4pa/GPM_L3/GPM_3IMERGM.03/

# GPM monthly precipitation rate (mm/hr) (2014 to present)
# 0.1 degree latitude x 0.1 degree longitude global grid
f <- paste("data/grids/GPM", list.files("data/grids/GPM"), sep = "/")

gpm <- list()

for (j in 1:length(f)){

  t <- h5read(f[j], name = "Grid")

  longitude <- t$lon
  latitude <- t$lat

  inds_lon <- (1:dim(longitude))
  inds_lat <- (1:dim(latitude))

  start_date <- parse_date_time(substr(f[j], 36, 43), "%Y%m%d")
  gpm_duration <- as.numeric((floor_date(start_date, "month") + months(1)) - start_date)

  site_coords <- read.csv("data/site_coords.csv")
  names(site_coords)[2] <- "long_name"
  site_list <- c("rppn-fma", "amboseli", "kakamega", "gombe", "karisoke", "beza", "ssr")
  site_coords$site <- site_list
  site_coords$site <- factor(site_coords$site, levels = site_list)
  site_coords <- mutate(site_coords, lat_ind = 0, lon_ind = 0)

  for(i in 1:nrow(site_coords)){
    site_coords[i, ]$lat_ind <- which.min(abs(site_coords[i, ]$Lat - latitude))
    site_coords[i, ]$lon_ind <- which.min(abs(site_coords[i, ]$Lon - longitude))
  }

  gpm_f <- list()

  for(i in 1:nrow(site_coords)){
    gpm_f[[i]] <- tbl_df(data.frame(year_of = year(start_date),
                                    month_of = month(start_date),
                                    days = gpm_duration,
                                    precip = t$precipitation[site_coords[i, ]$lat_ind,
                                                             site_coords[i, ]$lon_ind],
                                    site = site_coords[i, ]$site))
  }

  gpm[[j]] <- bind_rows(gpm_f)

}

gpm <- bind_rows(gpm)

gpm <- gpm %>%
  mutate(rain_total_mm = precip * days * 24)


rm(list = c("t", "gpm", "longitude", "latitude", "start_date", "gpm_duration"))