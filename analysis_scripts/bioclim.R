get_qua_set <- function(df, target_year){
  r <- filter(df, year_of == target_year |
                (year_of == target_year - 1 & month_of %in% c("Nov", "Dec")))
  return(r)
}

get_bioclim_annual <- function(df, clim_extremes = NULL){

  bca <- list()
  c <- 1

  for (i in min(df$year_of + 1):max(df$year_of)) {

    t <- get_qua_set(df, i)

    tmin_i <- t(as.matrix(t$tmin_monthly))
    tmax_i <- t(as.matrix(t$tmax_monthly))
    prec_i <- t(as.matrix(t$rain_monthly_mm))
    tavg_i <- t(as.matrix(t$tavg_monthly))

    clim <- mutate_each(clim_extremes[df$site[1], ], funs(as.numeric))

    b <- suppressMessages(bioclim_annual(tmin_i, tmax_i, prec_i, tavg_i,
                                         t.as.int = FALSE, clim = clim))

    b <- data.frame(rbind(b), row.names = NULL)
    b$year_of <- i

    bca[[c]] <- b

    c <- c + 1
  }

  bca <- bind_rows(bca)
  bca <- select(bca, year_of, 1:19)

  return(bca)

}

get_indclim <- function(df, clim_extremes = NULL){

  ica <- list()
  c <- 1

  for (i in min(df$year_of + 1):max(df$year_of)) {

    t <- get_qua_set(df, i)

    osc_i <- t(as.matrix(t$value))

    clim <- mutate_each(clim_extremes, funs(as.numeric))

    b <- suppressMessages(indclim_annual(osc_i, t.as.int = FALSE, clim = clim))

    b <- data.frame(rbind(b), row.names = NULL)
    b$year_of <- i
    b$site <- levels(climates$site)

    ica[[c]] <- b

    c <- c + 1
  }

  ica <- bind_rows(ica)
  ica <- select(ica, site, year_of, 1:9)

  return(ica)

}

get_speiclim <- function(df, clim_extremes = NULL){

  sca <- list()
  c <- 1

  for (i in min(df$year_of + 1):max(df$year_of)) {

    t <- get_qua_set(df, i)

    osc_i <- t(as.matrix(t$value))

    clim <- mutate_each(clim_extremes[df$site[1], ], funs(as.numeric))

    b <- suppressMessages(indclim_annual(osc_i, t.as.int = FALSE, clim = clim))

    b <- data.frame(rbind(b), row.names = NULL)
    b$year_of <- i

    sca[[c]] <- b

    c <- c + 1
  }

  sca <- bind_rows(sca)
  sca <- select(sca, year_of, 1:9)

  return(sca)

}

# The function below was written by Jeremy VanDerWal jjvanderwal@gmail.com
# In defunct "climates" package
# I just replaced annoying "print" command with "message", which can be suppressed
bioclim <- function(tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL,
          vois = 1:19, cov = FALSE, t.as.int = TRUE, period = "month",
          annual = FALSE) {
  tmin.vois = c(2, 3, 5, 6)
  tmax.vois = c(2, 3, 5, 7)
  prec.vois = c(8, 9, 12:19)
  tmean.vois = c(1, 4:6, 8:11, 18:19)
  tsize = NULL
  m.per.indx = function(x) {
    c(x, (x:(x + 1)) %% 12 + 1)
  }
  w.per.indx = function(x) {
    c(x, (x:(x + 11)) %% 52 + 1)
  }
  error.check = function(datum, datum.name, dsize = tsize) {
    if (is.null(datum))
      stop(paste(datum.name, "is needed for the variables selected"))
    else if (is.data.frame(datum) | is.matrix(datum)) {
      if (!(dim(datum)[2] %in% c(12, 52)))
        stop(paste(datum.name, "must have 12 or 52 columns -- one for each month or week."))
      dsize = c(dsize, dim(datum)[1])
    }
    else stop(paste(datum.name, "must be a data.frame or matrix"))
    return(dsize)
  }
  if (any(vois %in% tmin.vois))
    tsize = error.check(tmin, "tmin")
  if (any(vois %in% tmax.vois))
    tsize = error.check(tmax, "tmax")
  if (any(vois %in% prec.vois))
    tsize = error.check(prec, "prec")
  if (any(vois %in% tmean.vois)) {
    if (is.null(tmean)) {
      tmean = (tmax + tmin)/2
      message("Calculated tmean as (tmax+tmin)/2")
    }
    else tsize = error.check(tmean, "tmean")
  }
  if (!all(tsize == mean(tsize)))
    stop("all input data must be of the same length")
  tsize = mean(tsize)
  out = matrix(NA, nrow = tsize, ncol = 19)
  if (any(vois %in% c(1, 4)))
    out[, 1] = rowMeans(tmean, na.rm = T)
  if (any(vois %in% c(2, 3)))
    out[, 2] = rowMeans(tmax - tmin, na.rm = T)
  if (any(vois == 4)) {
    if (cov) {
      out[, 4] = (apply(tmean, 1, function(x) {
        return(sd(x, na.rm = T))
      })/(out[, 1] + 273.15)) * 100
    }
    else {
      out[, 4] = apply(tmean, 1, function(x) {
        return(sd(x, na.rm = T))
      })
    }
  }
  if (any(vois %in% c(5, 3, 7))) {
    out[, 5] = apply(tmax, 1, function(x) {
      return(max(x, na.rm = T))
    })
  }
  if (any(vois %in% c(6, 3, 7))) {
    out[, 6] = apply(tmin, 1, function(x) {
      return(min(x, na.rm = T))
    })
  }
  if (any(vois %in% c(3, 7)))
    out[, 7] = out[, 5] - out[, 6]
  if (any(vois == 3))
    out[, 3] = out[, 2]/out[, 7]
  if (any(vois == 12))
    out[, 12] = rowSums(prec, na.rm = T)
  if (any(vois == 13)) {
    out[, 13] = apply(prec, 1, function(x) {
      return(max(x, na.rm = T))
    })
  }
  if (any(vois == 14)) {
    out[, 14] = apply(prec, 1, function(x) {
      return(min(x, na.rm = T))
    })
  }
  if (any(vois == 15)) {
    out[, 15] = apply(prec, 1, function(x) {
      return(sd(x, na.rm = T))
    })/rowMeans(prec, na.rm = T)
  }
  if (any(vois %in% c(8:11, 16:19))) {
    if (period == "month") {
      tt1 = matrix(NA, nrow = tsize, ncol = 12)
      tt2 = matrix(NA, nrow = tsize, ncol = 12)
      for (ii in 1:12) {
        tt1[, ii] = rowMeans(tmean[, m.per.indx(ii)],
                             na.rm = T)
        tt2[, ii] = rowSums(prec[, m.per.indx(ii)], na.rm = T)
      }
    }
    else {
      tt1 = matrix(NA, nrow = tsize, ncol = 52)
      tt2 = matrix(NA, nrow = tsize, ncol = 52)
      for (ii in 1:52) {
        tt1[, ii] = rowMeans(tmean[, w.per.indx(ii)],
                             na.rm = T)
        tt2[, ii] = rowSums(prec[, w.per.indx(ii)], na.rm = T)
      }
    }
  }
  if (any(vois %in% c(10:11, 18:19))) {
    if (any(vois %in% c(10, 18))) {
      out[, 10] = apply(tt1, 1, function(x) {
        return(max(x, na.rm = T))
      })
    }
    if (any(vois %in% c(11, 19))) {
      out[, 11] = apply(tt1, 1, function(x) {
        return(min(x, na.rm = T))
      })
    }
    if (any(vois %in% 18:19)) {
      if (any(vois == 18)) {
        tt = NULL
        for (ii in 1:tsize) {
          tt = which(tt1[ii, ] == out[ii, 10])
          if (length(tt) > 1)
            tt = tt[1]
          out[ii, 18] = tt2[ii, tt]
        }
      }
      if (any(vois == 19)) {
        tt = NULL
        for (ii in 1:tsize) {
          tt = which(tt1[ii, ] == out[ii, 11])
          if (length(tt) > 1)
            tt = tt[1]
          out[ii, 19] = tt2[ii, tt]
        }
      }
    }
  }
  if (any(vois %in% c(8:9, 16:17))) {
    if (any(vois %in% c(8, 16))) {
      out[, 16] = apply(tt2, 1, function(x) {
        return(max(x, na.rm = T))
      })
    }
    if (any(vois %in% c(9, 17))) {
      out[, 17] = apply(tt2, 1, function(x) {
        return(min(x, na.rm = T))
      })
    }
    if (any(vois %in% 8:9)) {
      if (any(vois == 8)) {
        tt = NULL
        for (ii in 1:tsize) {
          tt = which(tt2[ii, ] == out[ii, 16])
          if (length(tt) > 1)
            tt = tt[1]
          out[ii, 8] = tt1[ii, tt]
        }
      }
      if (any(vois == 9)) {
        tt = NULL
        for (ii in 1:tsize) {
          tt = which(tt2[ii, ] == out[ii, 17])
          if (length(tt) > 1)
            tt = tt[1]
          out[ii, 9] = tt1[ii, tt]
        }
      }
    }
  }
  colnames(out) = paste("bioclim", 1:19, sep = "_")
  return(out[, vois])
}


# The functions below were modified from code written by Jeremy VanDerWal
# jjvanderwal@gmail.com
# In defunct "climates" package
#
# The original function takes a series of climate measurements
# and calculates the 19 Bioclimatic variables based for each location
# over the entire time period
#
# The modified "bioclim_annual" function calculates the 19 variables
# seperately for each year represented in the data
#
# It takes as an argument a data.frame produced by mon_extremes:
# The normally coldest, warmest, wettest, and driest months and quarters based
# on long-term data
bioclim_annual <- function(tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL,
                     vois = 1:19, cov = FALSE, t.as.int = TRUE, period = "month",
                     clim = NULL)
{
  tmin.vois = c(2, 3, 5, 6)
  tmax.vois = c(2, 3, 5, 7)
  prec.vois = c(8, 9, 12:19)
  tmean.vois = c(1, 4:6, 8:11, 18:19)
  tsize = NULL
  m.per.indx = function(x) {
    mm <- c(x, (x:(x + 1)) %% 12 + 1) + 2
    if (x == 11)
      mm <- c(1, 2, 3)
    if (x == 12)
      mm <- c(2, 3, 4)
    return(mm)
  }

  error.check = function(datum, datum.name, dsize = tsize) {
    if (is.null(datum))
      stop(paste(datum.name, "is needed for the variables selected"))
    else if (is.data.frame(datum) | is.matrix(datum)) {
      if (!(dim(datum)[2] == 14))
        stop(paste(datum.name, "must have 14 columns -- one for each month + Nov and Dec of previous year."))
      dsize = c(dsize, dim(datum)[1])
    }
    else stop(paste(datum.name, "must be a data.frame or matrix"))
    return(dsize)
  }
  if (any(vois %in% tmin.vois))
    tsize = error.check(tmin, "tmin")
  if (any(vois %in% tmax.vois))
    tsize = error.check(tmax, "tmax")
  if (any(vois %in% prec.vois))
    tsize = error.check(prec, "prec")
  if (any(vois %in% tmean.vois)) {
    if (is.null(tmean)) {
      tmean = (tmax + tmin)/2
      message("Calculated tmean as (tmax+tmin)/2")
    }
    else tsize = error.check(tmean, "tmean")
  }
  if (!all(tsize == mean(tsize)))
    stop("all input data must be of the same length")
  tsize = mean(tsize)
  out = matrix(NA, nrow = tsize, ncol = 19)
  if (any(vois %in% c(1, 4)))
    out[, 1] = mean(tmean[, 3:14], na.rm = T)
  if (any(vois %in% c(2, 3)))
    out[, 2] = mean(tmax[, 3:14] - tmin[, 3:14], na.rm = T)
  if (any(vois == 4)) {
    if (cov) {
      out[, 4] = sd(tmean[, 3:14], na.rm = T) / (out[, 1] + 273.15) * 100
    }
    else {
      out[, 4] = sd(tmean[, 3:14], na.rm = T)
    }
  }
  if (any(vois %in% c(5, 3, 7))) {
    out[, 5] = tmax[, 3:14][as.numeric(clim["warmest_month"])]
  }
  if (any(vois %in% c(6, 3, 7))) {
    out[, 6] = tmin[, 3:14][as.numeric(clim["coldest_month"])]
  }
  if (any(vois %in% c(3, 7)))
    out[, 7] = out[, 5] - out[, 6]
  if (any(vois == 3))
    out[, 3] = out[, 2]/out[, 7]
  if (any(vois == 12))
    out[, 12] = sum(prec[, 3:14], na.rm = T)
  if (any(vois == 13)) {
    out[, 13] = prec[, 3:14][as.numeric(clim["wettest_month"])]
  }
  if (any(vois == 14)) {
    out[, 14] = prec[, 3:14][as.numeric(clim["driest_month"])]
  }
  if (any(vois == 15)) {
    out[, 15] = sd(prec[, 3:14], na.rm = T) / mean(prec[, 3:14], na.rm = T)
  }
  if (any(vois %in% c(8:11, 16:19))) {
    tt1 = matrix(NA, nrow = tsize, ncol = 12)
    tt2 = matrix(NA, nrow = tsize, ncol = 12)
    for (ii in 1:12) {
      tt1[, ii] = mean(tmean[, m.per.indx(ii)], na.rm = T)
      tt2[, ii] = sum(prec[, m.per.indx(ii)], na.rm = T)
    }
  }
  if (any(vois %in% c(10:11, 18:19))) {
    if (any(vois %in% c(10, 18))) {
      out[, 10] = tt1[, as.numeric(clim["warmest_quarter"])]
    }
    if (any(vois %in% c(11, 19))) {
      out[, 11] = tt1[, as.numeric(clim["coldest_quarter"])]
    }
    if (any(vois %in% 18:19)) {
      if (any(vois == 18)) {
        out[, 18] = tt2[, as.numeric(clim["warmest_quarter"])]
      }
      if (any(vois == 19)) {
        out[, 19] = tt2[, as.numeric(clim["coldest_quarter"])]
      }
    }
  }
  if (any(vois %in% c(8:9, 16:17))) {
    if (any(vois %in% c(8, 16))) {
      out[, 16] = tt2[, as.numeric(clim["wettest_quarter"])]
    }
    if (any(vois %in% c(9, 17))) {
      out[, 17] = tt2[, as.numeric(clim["driest_quarter"])]
    }
    if (any(vois %in% 8:9)) {
      if (any(vois == 8)) {
        out[, 8] = tt1[, as.numeric(clim["wettest_quarter"])]
      }
      if (any(vois == 9)) {
        out[, 9] = tt1[, as.numeric(clim["driest_quarter"])]
      }
    }
  }
  colnames(out) = paste("bioclim", 1:19, sep = "_")
  return(out[, vois])
}


indclim_annual <- function(osc = NULL, vois = 1:9, t.as.int = TRUE, clim = NULL)
{
  osc.vois = c(1:9)
  tsize = NULL
  m.per.indx = function(x) {
    mm <- c(x, (x:(x + 1)) %% 12 + 1) + 2
    if (x == 11)
      mm <- c(1, 2, 3)
    if (x == 12)
      mm <- c(2, 3, 4)
    return(mm)
  }

  error.check = function(datum, datum.name, dsize = tsize) {
    if (is.null(datum))
      stop(paste(datum.name, "is needed for the variables selected"))
    else if (is.data.frame(datum) | is.matrix(datum)) {
      if (!(dim(datum)[2] == 14))
        stop(paste(datum.name, "must have 14 columns -- one for each month + Nov and Dec of previous year."))
      dsize = c(dsize, dim(datum)[1])
    }
    else stop(paste(datum.name, "must be a data.frame or matrix"))
    return(dsize)
  }

  if (any(vois %in% osc.vois))
    tsize = error.check(osc, "osc")

  if (!all(tsize == mean(tsize)))
    stop("all input data must be of the same length")

  hsize = nrow(clim)

  out = matrix(NA, nrow = hsize, ncol = 9)

  # 1. Annual mean value
  if (any(vois == 1))
    out[, 1] = rowMeans(t(osc[, 3:14]), na.rm = T)

  # 2. Seasonality
  if (any(vois == 2)) {
    out[, 2] = apply(t(osc[, 3:14]), 1, function(x) {
      return(sd(x, na.rm = T))
    })
  }

  # 3. Max value
  if (any(vois %in% c(3, 5))) {
    out[, 3] = apply(t(osc[, 3:14]), 1, function(x) {
      return(max(x, na.rm = T))
    })
  }

  # 4. Min value
  if (any(vois %in% c(4, 5))) {
    out[, 4] = apply(t(osc[, 3:14]), 1, function(x) {
      return(min(x, na.rm = T))
    })
  }

  # 5. Annual range
  if (any(vois == 5))
    out[, 5] = out[, 3] - out[, 4]

  if (any(vois %in% c(6:9))) {
    tt3 = matrix(NA, nrow = tsize, ncol = 12)
    for (ii in 1:12) {
      tt3[, ii] = mean(osc[, m.per.indx(ii)], na.rm = T)
    }
  }

  if (any(vois %in% c(6:7))) {
    # 6. Mean value during warmest quarter
    if (any(vois == 6)) {
      out[, 6] = tt3[1, clim[, "warmest_quarter"]]
    }
    # 7. Mean value during coldest quarter
    if (any(vois == 7)) {
      out[, 7] = tt3[1, clim[, "coldest_quarter"]]
    }
  }

  if (any(vois %in% c(8:9))) {
    # 8. Mean value during wettest quarter
    if (any(vois == 8)) {
      out[, 8] = tt3[1, clim[, "wettest_quarter"]]
    }
    # 9. Mean value during driest quarter
    if (any(vois == 9)) {
      out[, 9] = tt3[1, clim[, "driest_quarter"]]
    }
  }

  colnames(out) = paste("indclim", 1:9, sep = "_")
  return(out[, vois])
}

# indclim <- function(tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL,
#                      osc = NULL, vois = 1:9, t.as.int = TRUE,
#                      period = "month")
# {
#   # osc.vois = c(1:9)
#   prec.vois = c(8, 9)
#   tmean.vois = c(6, 7)
#   tsize = NULL
#   m.per.indx = function(x) {
#     c(x, (x:(x + 1)) %% 12 + 1)
#   }
#   w.per.indx = function(x) {
#     c(x, (x:(x + 11)) %% 52 + 1)
#   }
#
#   error.check = function(datum, datum.name, dsize = tsize) {
#     if (is.null(datum))
#       stop(paste(datum.name, "is needed for the variables selected"))
#     else if (is.data.frame(datum) | is.matrix(datum)) {
#       if (!(dim(datum)[2] %in% c(12, 52)))
#         stop(paste(datum.name, "must have 12 or 52 columns -- one for each month or week."))
#       dsize = c(dsize, dim(datum)[1])
#     }
#     else stop(paste(datum.name, "must be a data.frame or matrix"))
#     return(dsize)
#   }
#
# #   if (any(vois %in% osc.vois))
# #     tsize = error.check(osc, "osc")
#   if (any(vois %in% prec.vois))
#     tsize = error.check(prec, "prec")
#   if (any(vois %in% tmean.vois)) {
#     if (is.null(tmean)) {
#       tmean = (tmax + tmin)/2
#       message("Calculated tmean as (tmax+tmin)/2")
#     }
#     else tsize = error.check(tmean, "tmean")
#   }
#   if (!all(tsize == mean(tsize)))
#     stop("all input data must be of the same length")
#
#   tsize = mean(tsize)
#   out = matrix(NA, nrow = tsize, ncol = 9)
#
#   # 1. Annual mean value
#   if (any(vois == 1))
#     out[, 1] = rowMeans(osc, na.rm = T)
#
#   # 2. Seasonality
#   if (any(vois == 2)) {
#     out[, 2] = apply(osc, 1, function(x) {
#       return(sd(x, na.rm = T))
#     })
#   }
#
#   # 3. Max value
#   if (any(vois %in% c(3, 5))) {
#     out[, 3] = apply(osc, 1, function(x) {
#       return(max(x, na.rm = T))
#     })
#   }
#
#   # 4. Min value
#   if (any(vois %in% c(4, 5))) {
#     out[, 4] = apply(osc, 1, function(x) {
#       return(min(x, na.rm = T))
#     })
#   }
#
#   # 5. Annual range
#   if (any(vois == 5))
#     out[, 5] = out[, 3] - out[, 4]
#
#   if (any(vois %in% c(6:9))) {
#     if (period == "month") {
#       tt1 = matrix(NA, nrow = tsize, ncol = 12)
#       tt2 = matrix(NA, nrow = tsize, ncol = 12)
#       tt3 = matrix(NA, nrow = tsize, ncol = 12)
#       for (ii in 1:12) {
#         tt1[, ii] = rowMeans(tmean[, m.per.indx(ii)], na.rm = T)
#         tt2[, ii] = rowSums(prec[, m.per.indx(ii)], na.rm = T)
#         tt3[, ii] = rowMeans(osc[, m.per.indx(ii)], na.rm = T)
#       }
#     }
#     else {
#       tt1 = matrix(NA, nrow = tsize, ncol = 52)
#       tt2 = matrix(NA, nrow = tsize, ncol = 52)
#       tt3 = matrix(NA, nrow = tsize, ncol = 52)
#       for (ii in 1:52) {
#         tt1[, ii] = rowMeans(tmean[, w.per.indx(ii)], na.rm = T)
#         tt2[, ii] = rowSums(prec[, w.per.indx(ii)], na.rm = T)
#         tt3[, ii] = rowMeans(osc[, w.per.indx(ii)], na.rm = T)
#       }
#     }
#   }
#
#   if (any(vois %in% c(6:7))) {
#     # 6. Mean value during warmest quarter
#     if (any(vois == 6)) {
#       warmest = apply(tt1, 1, function(x) {
#         return(max(x, na.rm = T))
#       })
#       tt = NULL
#       for (ii in 1:tsize) {
#         tt = which(tt1[ii, ] == warmest[ii])
#         if (length(tt) > 1)
#           tt = tt[1]
#         out[ii, 6] = tt3[ii, tt]
#       }
#     }
#     # 7. Mean value during coldest quarter
#     if (any(vois == 7)) {
#       coldest = apply(tt1, 1, function(x) {
#         return(min(x, na.rm = T))
#       })
#       tt = NULL
#       for (ii in 1:tsize) {
#         tt = which(tt1[ii, ] == coldest[ii])
#         if (length(tt) > 1)
#           tt = tt[1]
#         out[ii, 7] = tt3[ii, tt]
#       }
#     }
#   }
#
#   if (any(vois %in% c(8:9))) {
#     # 8. Mean value during wettest quarter
#     if (any(vois == 8)) {
#       wettest = apply(tt2, 1, function(x) {
#         return(max(x, na.rm = T))
#       })
#       tt = NULL
#       for (ii in 1:tsize) {
#         tt = which(tt2[ii, ] == wettest[ii])
#         if (length(tt) > 1)
#           tt = tt[1]
#         out[ii, 8] = tt3[ii, tt]
#       }
#     }
#     # 9. Mean value during driest quarter
#     if (any(vois == 9)) {
#       driest = apply(tt2, 1, function(x) {
#         return(min(x, na.rm = T))
#       })
#       tt = NULL
#       for (ii in 1:tsize) {
#         tt = which(tt2[ii, ] == driest[ii])
#         if (length(tt) > 1)
#           tt = tt[1]
#         out[ii, 9] = tt3[ii, tt]
#       }
#     }
#   }
#
#   colnames(out) = paste("indclim", 1:9, sep = "_")
#   return(out[, vois])
# }