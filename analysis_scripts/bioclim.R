# Code written by Jeremy VanDerWal jjvanderwal@gmail.com
# In defunct "climates" package
# I replaced annoying "print" command with "message", which can be suppressed

bioclim <- function (tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL,
          vois = 1:19, cov = FALSE, t.as.int = TRUE, period = "month")
{
  tmin.vois = c(2, 3, 5, 6)
  tmax.vois = c(2, 3, 5, 7)
  prec.vois = c(8, 9, 12:19)
  tmean.vois = c(1, 4:6, 8:11, 18:19)
  tsize = NULL
  m.per.indx = function(x) {
    c(x, (x:(x + 1))%%12 + 1)
  }
  w.per.indx = function(x) {
    c(x, (x:(x + 11))%%52 + 1)
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


indclim <- function (tmin = NULL, tmax = NULL, prec = NULL, tmean = NULL,
                     ind = NULL, vois = 1:9, cov = FALSE, t.as.int = TRUE,
                     period = "month")
{
  tmin.vois = c(2, 3, 5, 6)
  tmax.vois = c(2, 3, 5, 7)
  prec.vois = c(8, 9, 12:19)
  tmean.vois = c(1, 4:6, 8:11, 18:19)
  tsize = NULL
  m.per.indx = function(x) {
    c(x, (x:(x + 1))%%12 + 1)
  }
  w.per.indx = function(x) {
    c(x, (x:(x + 11))%%52 + 1)
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
  colnames(out) = paste("indclim", 1:9, sep = "_")
  return(out[, vois])
}