autowin_fc <- function (reference, xvar, cdate, bdate, baseline, range, stat,
          func, type, refday, cmissing = FALSE, cinterval = "day",
          upper = NA, lower = NA, binary = FALSE, centre = list(NULL,
                                                                "both"), cohort = NULL, spatial = NULL, cutoff.day = NULL,
          cutoff.month = NULL, furthest = NULL, closest = NULL, thresh = NULL,
          WindowOpen = NULL, WindowClose = NULL)
{
  if (is.null(cohort) == TRUE) {
    cohort = lubridate::year(as.Date(bdate, format = "%d/%m/%Y"))
  }
  if(is.null(WindowOpen)) {
    WindowOpen <- reference$Dataset$WindowOpen[1]
  }
  if(is.null(WindowClose)) {
    WindowClose <- reference$Dataset$WindowClose[1]
  }
  reference <- reference$BestModelData$climate
  if (is.null(thresh) == FALSE) {
    stop("Parameter 'thresh' is now redundant. Please use parameter 'binary' instead.")
  }
  if (type == "variable" || type == "fixed") {
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  if (is.null(cutoff.day) == FALSE & is.null(cutoff.month) ==
      FALSE) {
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  if (is.null(furthest) == FALSE & is.null(closest) == FALSE) {
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  xvar = xvar[[1]]
  print("Initialising, please wait...")
  if (stat == "slope" & func == "log" || stat == "slope" &
      func == "inv") {
    stop("stat = slope cannot be used with func = log or inv as negative values may be present.")
  }
  if (cinterval == "day") {
    if ((min(as.Date(bdate, format = "%d/%m/%Y")) - range[1]) <
        min(as.Date(cdate, format = "%d/%m/%Y"))) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  if (cinterval == "week") {
    if ((min(as.Date(bdate, format = "%d/%m/%Y")) - lubridate::weeks(range[1])) <
        min(as.Date(cdate, format = "%d/%m/%Y"))) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  if (cinterval == "month") {
    if ((min(as.Date(bdate, format = "%d/%m/%Y")) - months(range[1])) <
        min(as.Date(cdate, format = "%d/%m/%Y"))) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  duration <- (range[1] - range[2]) + 1
  maxmodno <- (duration * (duration + 1))/2
  cont <- convertdate(bdate = bdate, cdate = cdate, xvar = xvar,
                      cinterval = cinterval, type = type, refday = refday,
                      cohort = cohort, spatial = spatial, upper = NA, lower = NA)
  modno <- 1
  modlist <- list()
  cmatrix <- matrix(ncol = (duration), nrow = length(bdate))
  climate1 <- matrix(ncol = 1, nrow = length(bdate), 1)
  if (is.null(spatial) == FALSE) {
    if (is.na(upper) == FALSE && is.na(lower) == TRUE) {
      if (thresh == TRUE) {
        cont$xvar$Clim <- ifelse(cont$xvar$Clim > upper,
                                 1, 0)
      }
      else {
        cont$xvar$Clim <- ifelse(cont$xvar$Clim > upper,
                                 cont$xvar$Clim, 0)
      }
    }
    if (is.na(lower) == FALSE && is.na(upper) == TRUE) {
      if (thresh == TRUE) {
        cont$xvar$Clim <- ifelse(cont$xvar$Clim < lower,
                                 1, 0)
      }
      else {
        cont$xvar$Clim <- ifelse(cont$xvar$Clim < lower,
                                 cont$xvar$Clim, 0)
      }
    }
    if (is.na(lower) == FALSE && is.na(upper) == FALSE) {
      if (thresh == TRUE) {
        cont$xvar$Clim <- ifelse(cont$xvar$Clim > lower &
                                   cont$xvar$Clim < upper, 1, 0)
      }
      else {
        cont$xvar$Clim <- ifelse(cont$xvar$Clim > lower &
                                   cont$xvar$Clim < upper, cont$xvar$Clim - lower,
                                 0)
      }
    }
  }
  else {
    if (is.na(upper) == FALSE && is.na(lower) == TRUE) {
      if (thresh == TRUE) {
        cont$xvar <- ifelse(cont$xvar > upper, 1, 0)
      }
      else {
        cont$xvar <- ifelse(cont$xvar > upper, cont$xvar,
                            0)
      }
    }
    if (is.na(lower) == FALSE && is.na(upper) == TRUE) {
      if (thresh == TRUE) {
        cont$xvar <- ifelse(cont$xvar < lower, 1, 0)
      }
      else {
        cont$xvar <- ifelse(cont$xvar < lower, cont$xvar,
                            0)
      }
    }
    if (is.na(lower) == FALSE && is.na(upper) == FALSE) {
      if (thresh == TRUE) {
        cont$xvar <- ifelse(cont$xvar > lower & cont$xvar <
                              upper, 1, 0)
      }
      else {
        cont$xvar <- ifelse(cont$xvar > lower & cont$xvar <
                              upper, cont$xvar - lower, 0)
      }
    }
  }
  if (is.null(spatial) == FALSE) {
    for (i in 1:length(bdate)) {
      cmatrix[i, ] <- cont$xvar[which(cont$cintno$spatial %in%
                                        cont$bintno$spatial[i] & cont$cintno$Date %in%
                                        (cont$bintno$Date[i] - c(range[2]:range[1]))),
                                1]
    }
  }
  else {
    for (i in 1:length(bdate)) {
      cmatrix[i, ] <- cont$xvar[which(cont$cintno %in%
                                        (cont$bintno[i] - c(range[2]:range[1])))]
    }
  }
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  if (cmissing == FALSE && length(which(is.na(cmatrix))) >
      0) {
    if (is.null(spatial) == FALSE) {
      if (cinterval == "day") {
        .GlobalEnv$missing <- as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)],
                                      origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                        1)
      }
      if (cinterval == "month") {
        .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)],
                                                              origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                1)), "Year:", year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)],
                                                                                           origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                                             1))))
      }
      if (cinterval == "week") {
        .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)],
                                                             origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                               1)), "Year:", year(as.Date(cont$cintno$Date[is.na(cont$xvar$Clim)],
                                                                                          origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                                            1))))
      }
      stop(c("Climate data should not contain NA values: ",
             length(.GlobalEnv$missing), " NA value(s) found. Please add missing climate data or set cmissing=TRUE.\n           See object missing for all missing climate data"))
    }
    else {
      if (cinterval == "day") {
        .GlobalEnv$missing <- as.Date(cont$cintno[is.na(cont$xvar)],
                                      origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                        1)
      }
      if (cinterval == "month") {
        .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cont$cintno[is.na(cont$xvar)],
                                                              origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                1)), "Year:", year(as.Date(cont$cintno[is.na(cont$xvar)],
                                                                                           origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                                             1))))
      }
      if (cinterval == "week") {
        .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cont$cintno[is.na(cont$xvar)],
                                                             origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                               1)), "Year:", year(as.Date(cont$cintno[is.na(cont$xvar)],
                                                                                          origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                                            1))))
      }
      stop(c("Climate data should not contain NA values: ",
             length(.GlobalEnv$missing), " NA value(s) found. Please add missing climate data or set cmissing=TRUE.\n           See object missing for all missing climate data"))
    }
  }
  if (cmissing != FALSE && length(which(is.na(cmatrix))) >
      0) {
    print("Missing climate data detected. Please wait while appropriate data is calculated to replace NAs.")
    if (cmissing == "method1") {
      for (i in which(is.na(cmatrix))) {
        cmatrix[i] <- mean(c(cmatrix[i - (1:2)], cmatrix[i +
                                                           (1:2)]), na.rm = T)
        if (is.na(cmatrix[i])) {
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
        }
      }
    }
    else if (cmissing == "method2") {
      cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                              Year = lubridate::year(as.Date(cdate, format = "%d/%m/%Y")),
                              Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                              Day = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
      if (cinterval == "week") {
        for (j in 1:nrow(cdate_new)) {
          cdate_new$Week[j] <- ceiling((as.numeric(cdate_new$Date[j]) -
                                          min(as.numeric(subset(cdate_new, cdate_new$Year ==
                                                                  cdate_new$Year[j])$Date)) + 1)/7)
        }
      }
      for (i in which(is.na(cmatrix))) {
        col <- floor(i/nrow(cmatrix))
        if (is.null(spatial)) {
          brecord <- cont$bintno[i - col * nrow(cmatrix)] -
            (range[2] + col) - 1
        }
        else {
          brecord <- cont$bintno$Date[i - col * nrow(cmatrix)] -
            (range[2] + col) - 1
        }
        min_date <- min(as.Date(cdate, format = "%d/%m/%Y"))
        if (cinterval == "day") {
          missing_rec <- as.Date(brecord, format = "%d/%m/%Y",
                                 origin = min_date)
          cmatrix[i] <- mean(xvar[which(cdate_new$Month ==
                                          lubridate::month(missing_rec) & cdate_new$Day ==
                                          lubridate::day(missing_rec))], na.rm = T)
        }
        else if (cinterval == "week") {
          missing_week <- ceiling(((as.numeric((as.Date(bdate[i -
                                                                col * nrow(cmatrix)], format = "%d/%m/%Y"))) -
                                      (col * 7)) - as.numeric(as.Date(paste("01/01/",
                                                                            lubridate::year(as.Date(bdate[i - col * nrow(cmatrix)],
                                                                                                    format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) +
                                     1)/7)
          cmatrix[i] <- mean(xvar[which(cdate_new$Week ==
                                          missing_week)], na.rm = T)
        }
        else if (cinterval == "month") {
          missing_month <- (lubridate::month(min(as.Date(cdate,
                                                         format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) -
                                                                                     1)) - (floor((lubridate::month(min(as.Date(cdate,
                                                                                                                                format = "%d/%m/%Y"))) + (which(is.na(cont$xvar)) -
                                                                                                                                                            1))/12) * 12)
          cmatrix[i] <- mean(xvar[which(cdate_new$Month ==
                                          missing_month)], na.rm = T)
        }
        if (is.na(cmatrix[i])) {
          stop("There is no data available for certain climate records across all years. Consider using method1 or manually replacing NAs.")
        }
      }
    }
    else {
      stop("cmissing should be FALSE, 'method1' or 'method2'")
    }
  }
  modeldat <- model.frame(baseline)
  modeldat$yvar <- modeldat[, 1]
  modeldat$climate <- seq(1, nrow(modeldat), 1)
  if (is.null(weights(baseline)) == FALSE) {
    if (class(baseline)[1] == "glm" & sum(weights(baseline)) ==
        nrow(model.frame(baseline)) || attr(class(baseline),
                                            "package") == "lme4" & sum(weights(baseline)) ==
        nrow(model.frame(baseline))) {
    }
    else {
      modeldat$modweights <- weights(baseline)
      baseline <- update(baseline, . ~ ., weights = modeldat$modweights,
                         data = modeldat)
    }
  }
  if (func == "lin") {
    modeloutput <- update(baseline, . ~ . + climate, data = modeldat)
  }
  else if (func == "quad") {
    modeloutput <- update(baseline, . ~ . + climate + I(climate^2),
                          data = modeldat)
  }
  else if (func == "cub") {
    modeloutput <- update(baseline, . ~ . + climate + I(climate^2) +
                            I(climate^3), data = modeldat)
  }
  else if (func == "log") {
    modeloutput <- update(baseline, . ~ . + log(climate),
                          data = modeldat)
  }
  else if (func == "inv") {
    modeloutput <- update(baseline, . ~ . + I(climate^-1),
                          data = modeldat)
  }
  else {
    print("Define func")
  }
  pb <- txtProgressBar(min = 0, max = maxmodno, style = 3,
                       char = "|")
  for (m in range[2]:range[1]) {
    for (n in 1:duration) {
      if ((m - n) >= (range[2] - 1)) {
        if (stat != "slope" || n > 1) {
          windowopen <- m - range[2] + 1
          windowclose <- windowopen - n + 1
          if (stat == "slope") {
            time <- seq(1, n, 1)
            climate1 <- apply(cmatrix[, windowclose:windowopen],
                              1, FUN = function(x) coef(lm(x ~ time))[2])
          }
          else {
            if (n == 1) {
              climate1 <- cmatrix[, windowclose:windowopen]
            }
            else {
              climate1 <- apply(cmatrix[, windowclose:windowopen],
                                1, FUN = stat)
            }
          }
          modeloutput <- cor(climate1, reference)
          modlist$cor[[modno]] <- modeloutput
          modlist$WindowOpen[[modno]] <- m
          modlist$WindowClose[[modno]] <- m - n + 1
          modno <- modno + 1
        }
      }
    }
    setTxtProgressBar(pb, modno - 1)
  }
  modlist$Furthest <- range[1]
  modlist$Closest <- range[2]
  modlist$Statistics <- stat
  modlist$Functions <- type
  modlist$BestWindowOpen <- WindowOpen
  modlist$BestWindowClose <- WindowClose
  if (type == TRUE) {
    modlist$Reference.day <- refday[1]
    modlist$Reference.month <- refday[2]
  }
  local <- as.data.frame(modlist)
  return(local)
}

convertdate_fc <- function(bdate, cdate, xvar, xvar2 = NULL, cinterval, type,
                        refday, cross = FALSE, cohort, spatial, stat, upper, lower){


  if (cinterval != "day" && cinterval != "week" && cinterval != "month"){
    stop("cinterval should be either day, week or month")
  }

  bdate  <- as.Date(bdate, format = "%d/%m/%Y") # Convert the date variables into the R date format
  if(is.null(spatial) == FALSE) {
    SUB.DATE <- list()
    NUM <- 1
    for(i in levels(as.factor(spatial[[2]]))){
      SUB <- cdate[which(spatial[[2]] == i)]
      SUB.DATE[[NUM]] <- data.frame(Date = seq(min(as.Date(SUB, format = "%d/%m/%Y")), max(as.Date(SUB, format = "%d/%m/%Y")), "days"),
                                    spatial = i)
      if (length(SUB.DATE[[NUM]]$Date) != length(unique(SUB.DATE[[NUM]]$Date))){
        stop ("There are duplicate dayrecords in climate data")
      }
      NUM <- NUM + 1
    }
    spatialcdate <- plyr::rbind.fill(SUB.DATE)
    cdate2       <- spatialcdate$Date
    cintno       <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno   <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
  } else {
    cdate2     <- seq(min(as.Date(cdate, format = "%d/%m/%Y")), max(as.Date(cdate, format = "%d/%m/%Y")), "days")
    cintno     <- as.numeric(cdate2) - min(as.numeric(cdate2)) + 1   # atrribute daynumbers for both datafiles with first date in CLimateData set to cintno 1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) + 1
    if (length(cintno) != length(unique(cintno))){
      stop ("There are duplicate dayrecords in climate data")
    }
  }
  cdate  <- as.Date(cdate, format = "%d/%m/%Y")

  if(is.null(spatial) == FALSE){
    for(i in levels(as.factor(spatial[[2]]))){
      SUB <- cdate[which(spatial[[2]] == i)]
      SUB_biol <- bdate[which(spatial[[1]] == i)]
      if (min(SUB) > min(SUB_biol)){
        stop(paste("Climate data does not cover all years of biological data at site ", i ,". Earliest climate data is ", min(cdate), " Earliest biological data is ", min(bdate), ". Please increase range of climate data", sep = ""))
      }
      if (max(SUB) < max(SUB_biol)){
        stop(paste("Climate data does not cover all years of biological data at site ", i ,". Latest climate data is ", max(cdate), " Latest biological data is ", min(bdate), ". Please increase range of climate data", sep = ""))
      }
    }
  } else if (min(cdate) > min(bdate)){
    stop(paste("Climate data does not cover all years of biological data. Earliest climate data is ", min(cdate), ". Earliest biological data is ", min(bdate), sep = ""))
  }

  if (is.null(xvar2) == FALSE){
    if(is.null(spatial) == FALSE){
      xvar2      <- data.frame(Clim = xvar2, spatial = spatial[[2]])
      cdatetemp  <- data.frame(Date = cdate, spatial = spatial[[2]])
      split.list <- list()
      NUM <- 1
      for(i in levels(xvar2$spatial)){
        SUB <- subset(xvar2, spatial == i)
        SUBcdate  <- subset(cdatetemp, spatial == i)
        SUBcdate2 <- subset(spatialcdate, spatial == i)
        rownames(SUB) <- seq(1, nrow(SUB), 1)
        rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
        NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
        Newspatial <- rep(i, times = length(NewClim))
        split.list[[NUM]] <- data.frame(NewClim, Newspatial)
        NUM <- NUM + 1
      }
      xvar2    <- (plyr::rbind.fill(split.list))$NewClim
      climspatial <- (plyr::rbind.fill(split.list))$Newspatial
    } else {
      xvar2    <- xvar2[match(cdate2, cdate)]
    }
  }

  if(is.null(spatial) == FALSE){
    xvar       <- data.frame(Clim = xvar, spatial = spatial[[2]])
    cdate      <- data.frame(Date = cdate, spatial = spatial[[2]])
    split.list <- list()
    NUM <- 1
    for(i in levels(xvar$spatial)){
      SUB <- subset(xvar, spatial == i)
      SUBcdate  <- subset(cdate, spatial == i)
      SUBcdate2 <- subset(spatialcdate, spatial == i)
      rownames(SUB) <- seq(1, nrow(SUB), 1)
      rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
      NewClim    <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
      Newspatial <- rep(i, times = length(NewClim))
      split.list[[NUM]] <- data.frame(NewClim, Newspatial)
      NUM <- NUM + 1
    }
    xvar    <- (plyr::rbind.fill(split.list))$NewClim
    climspatial <- (plyr::rbind.fill(split.list))$Newspatial
  } else {
    xvar    <- xvar[match(cdate2, cdate)]
  }

  if (cross == FALSE){
    if (cinterval == "day"){
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
          }
        } else {
          bintno            <- as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "week"){

      if(is.na(upper) == FALSE && is.na(lower) == TRUE){

        xvar <- ifelse(xvar > upper, 1, 0)

      } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar < lower, 1, 0)

      } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)

      }

      cintno      <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to cintno 1
      realbintno  <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial)
        newclim2    <- reshape::cast(newclim, id = c("cintno", "spatial"))
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else {
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar)
        newclim2    <- reshape::cast(newclim, id = "cintno")
        newclim3    <- reshape::cast(newclim2, cintno ~ variable, fun.aggregate = stat, na.rm = T)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
      }

      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7)
          }
        } else {
          bintno <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7)
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "month"){

      if(is.na(upper) == FALSE && is.na(lower) == TRUE){

        xvar <- ifelse(xvar > upper, 1, 0)

      } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar < lower, 1, 0)

      } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)

      }

      cmonth     <- lubridate::month(cdate2)
      cyear      <- year(cdate2) - min(year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - min(year(cdate2)))
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "spatial" = climspatial)
        newclim2    <- reshape::cast(newclim, id = c("cintno", "spatial"))
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        newclim3    <- newclim3[order(newclim3$spatial, newclim3$cintno), ]
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar)
        newclim2   <- reshape::cast(newclim, id = "cintno")
        newclim3   <- reshape::cast(newclim2, cintno ~ variable, fun.aggregate = stat, na.rm = T)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
      }
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
          }
        } else {
          bintno <- refday[2] + 12 * (year(bdate) - min(year(cdate2)))
        }
      } else {
        bintno <- realbintno
      }
    }
  } else {
    if (cinterval == "day"){
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
          }
        } else {
          bintno <- as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "week"){

      if(is.na(upper) == FALSE && is.na(lower) == TRUE){

        xvar <- ifelse(xvar > upper, 1, 0)

      } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar < lower, 1, 0)

      } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)

      }

      cintno     <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) + 1) / 7)   # atrribute weeknumbers for both datafiles with first week in CLimateData set to cintno 1
      realbintno <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) + 1) / 7)
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial)
        newclim2    <- reshape::cast(newclim, id = c("cintno", "spatial"))
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- reshape::cast(newclim, id = "cintno")
        newclim3   <- reshape::cast(newclim2, cintno ~ variable, mean, na.rm = T)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
        xvar2      <- newclim3$xvar2
      }
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], min(lubridate::year(sub$bdate)), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7)
          }
        } else {
          bintno <- ceiling((as.numeric(as.Date(paste(refday[1], refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) + 1)/7)
        }
      } else {
        bintno <- realbintno
      }
    } else if (cinterval == "month"){

      if(is.na(upper) == FALSE && is.na(lower) == TRUE){

        xvar <- ifelse(xvar > upper, 1, 0)

      } else if(is.na(upper) == TRUE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar < lower, 1, 0)

      } else if(is.na(upper) == FALSE && is.na(lower) == FALSE){

        xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)

      }

      cmonth     <- lubridate::month(cdate2)
      cyear      <- year(cdate2) - min(year(cdate2))
      cintno     <- cmonth + 12 * cyear
      realbintno <- lubridate::month(bdate) + 12 * (year(bdate) - min(year(cdate2)))
      if(is.null(spatial) == FALSE){
        newclim     <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2, "spatial" = climspatial)
        newclim2    <- reshape::cast(newclim, id = c("cintno", "spatial"))
        newclim3    <- reshape::cast(newclim2, cintno + spatial ~ variable, mean, na.rm = T)
        cintno      <- newclim3$cintno
        xvar        <- newclim3$xvar
        xvar2       <- newclim3$xvar2
        climspatial <- newclim3$spatial
      } else {
        newclim    <- data.frame("cintno" = cintno, "xvar" = xvar, "xvar2" = xvar2)
        newclim2   <- reshape::cast(newclim, id = "cintno")
        newclim3   <- reshape::cast(newclim2, cintno ~ variable, mean, na.rm = T)
        cintno     <- newclim3$cintno
        xvar       <- newclim3$xvar
        xvar2      <- newclim3$xvar2
      }
      if (type == "absolute"){
        if(is.null(cohort) == FALSE){
          newdat   <- cbind(as.data.frame(bdate), as.data.frame(cohort))
          datenum  <- 1
          bintno   <- seq(1, length(bdate), 1)
          for(i in levels(as.factor(cohort))){
            sub                               <- subset(newdat, cohort == i)
            bintno[as.numeric(rownames(sub))] <- refday[2] + 12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
          }
        } else {
          bintno            <- refday[2] + 12 * (year(bdate) - min(year(cdate2)))
        }
      } else {
        bintno <- realbintno
      }
    }
  }
  if(is.null(spatial) == FALSE){
    if(is.null(xvar2) == FALSE){
      return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                  bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                  xvar = data.frame(Clim = xvar, spatial = climspatial),
                  xvar2 = data.frame(Clim = xvar2, spatial = climspatial)))
    } else {
      return(list(cintno = data.frame(Date = cintno, spatial = climspatial),
                  bintno = data.frame(Date = bintno, spatial = spatial[[1]]),
                  xvar = data.frame(Clim = xvar, spatial = climspatial)))
    }
  } else {
    if(is.null(xvar2) == FALSE){
      return(list(cintno = cintno, bintno = bintno, xvar = xvar, xvar2 = xvar2))
    } else {
      return(list(cintno = cintno, bintno = bintno, xvar = xvar))
    }
  }
}

singlewin_fc <- function (xvar, cdate, bdate, baseline, range, stat, func, type,
                          refday, cmissing = FALSE, cinterval = "day", cohort = NULL,
                          spatial = NULL, upper = NA, lower = NA, binary = FALSE,
                          centre = list(NULL, "both"), cutoff.day = NULL, cutoff.month = NULL, furthest = NULL,
                          closest = NULL, thresh = NULL)
{
  if (is.null(thresh) == FALSE) {
    stop("Parameter 'thresh' is now redundant. Please use parameter 'binary' instead.")
  }
  if (type == "variable" || type == "fixed") {
    stop("Parameter 'type' now uses levels 'relative' and 'absolute' rather than 'variable' and 'fixed'.")
  }
  if (is.null(furthest) == FALSE & is.null(closest) == FALSE) {
    stop("furthest and closest are now redundant. Please use parameter 'range' instead.")
  }
  if (is.null(cutoff.day) == FALSE & is.null(cutoff.month) ==
      FALSE) {
    stop("cutoff.day and cutoff.month are now redundant. Please use parameter 'refday' instead.")
  }
  xvar = xvar[[1]]
  if (stat == "slope" & func == "log" || stat == "slope" &
      func == "inv") {
    stop("stat = slope cannot be used with func = LOG or I as negative values may be present")
  }
  duration <- (range[1] - range[2]) + 1
  bdate <- as.Date(bdate, format = "%d/%m/%Y")
  if (is.null(spatial) == FALSE) {
    SUB.DATE <- list()
    NUM <- 1
    for (i in levels(as.factor(spatial[[2]]))) {
      SUB <- cdate[which(spatial[[2]] == i)]
      SUB.DATE[[NUM]] <- data.frame(Date = seq(min(as.Date(SUB,
                                                           format = "%d/%m/%Y")), max(as.Date(SUB, format = "%d/%m/%Y")),
                                               "days"), spatial = i)
      if (length(SUB.DATE[[NUM]]$Date) != length(unique(SUB.DATE[[NUM]]$Date))) {
        stop("There are duplicate dayrecords in climate data")
      }
      NUM <- NUM + 1
    }
    spatialcdate <- plyr::rbind.fill(SUB.DATE)
    cdate2 <- spatialcdate$Date
    cintno <- as.numeric(cdate2) - min(as.numeric(cdate2)) +
      1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) +
      1
  }
  else {
    cdate2 <- seq(min(as.Date(cdate, format = "%d/%m/%Y")),
                  max(as.Date(cdate, format = "%d/%m/%Y")), "days")
    cintno <- as.numeric(cdate2) - min(as.numeric(cdate2)) +
      1
    realbintno <- as.numeric(bdate) - min(as.numeric(cdate2)) +
      1
    if (length(cintno) != length(unique(cintno))) {
      stop("There are duplicate dayrecords in climate data")
    }
  }
  cdate <- as.Date(cdate, format = "%d/%m/%Y")
  if (is.null(spatial) == FALSE) {
    for (i in levels(as.factor(spatial[[2]]))) {
      SUB <- cdate[which(spatial[[2]] == i)]
      if (min(SUB) > min(bdate) | max(SUB) < max(bdate)) {
        stop("Climate data does not cover all years of biological data. Please increase range of climate data")
      }
    }
  }
  else if (min(cdate) > min(bdate) | max(cdate) < max(bdate)) {
    stop("Climate data does not cover all years of biological data. Please increase range of climate data")
  }
  if (is.null(spatial) == FALSE) {
    xvar <- data.frame(Clim = xvar, spatial = spatial[[2]])
    cdate <- data.frame(Date = cdate, spatial = spatial[[2]])
    split.list <- list()
    NUM <- 1
    for (i in levels(xvar$spatial)) {
      SUB <- subset(xvar, spatial == i)
      SUBcdate <- subset(cdate, spatial == i)
      SUBcdate2 <- subset(spatialcdate, spatial == i)
      rownames(SUB) <- seq(1, nrow(SUB), 1)
      rownames(SUBcdate) <- seq(1, nrow(SUBcdate), 1)
      NewClim <- SUB$Clim[match(SUBcdate2$Date, SUBcdate$Date)]
      Newspatial <- rep(i, times = length(NewClim))
      split.list[[NUM]] <- data.frame(NewClim, Newspatial)
      NUM <- NUM + 1
    }
    xvar <- (plyr::rbind.fill(split.list))$NewClim
    climspatial <- (plyr::rbind.fill(split.list))$Newspatial
  }
  else {
    xvar <- xvar[match(cdate2, cdate)]
  }
  if (cinterval != "day" && cinterval != "week" && cinterval !=
      "month") {
    stop("cinterval should be either day, week or month")
  }
  if (cinterval == "day") {
    if (type == "absolute") {
      if (is.null(cohort) == FALSE) {
        newdat <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum <- 1
        bintno <- seq(1, length(bdate), 1)
        for (i in levels(as.factor(cohort))) {
          sub <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- as.numeric(as.Date(paste(refday[1],
                                                                        refday[2], min(lubridate::year(sub$bdate)),
                                                                        sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) +
            1
        }
      }
      else {
        bintno <- as.numeric(as.Date(paste(refday[1],
                                           refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) -
          min(as.numeric(cdate2)) + 1
      }
    }
    else {
      bintno <- realbintno
    }
  }
  else if (cinterval == "week") {
    cintno <- ceiling((as.numeric(cdate2) - min(as.numeric(cdate2)) +
                         1)/7)
    realbintno <- ceiling((as.numeric(bdate) - min(as.numeric(cdate2)) +
                             1)/7)
    if (is.null(spatial) == FALSE) {
      newclim <- data.frame(cintno = cintno, xvar = xvar,
                            spatial = climspatial)
      newclim2 <- melt(newclim, id = c("cintno", "spatial"))
      newclim3 <- cast(newclim2, cintno + spatial ~ variable,
                       mean)
      newclim3 <- newclim3[order(newclim3$spatial, newclim3$cintno),
                           ]
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
      climspatial <- newclim3$spatial
    }
    else {
      newclim <- data.frame(cintno = cintno, xvar = xvar)
      newclim2 <- melt(newclim, id = "cintno")
      newclim3 <- cast(newclim2, cintno ~ variable, mean)
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
    }
    if (type == "absolute") {
      if (is.null(cohort) == FALSE) {
        newdat <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum <- 1
        bintno <- seq(1, length(bdate), 1)
        for (i in levels(as.factor(cohort))) {
          sub <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- ceiling((as.numeric(as.Date(paste(refday[1],
                                                                                 refday[2], min(lubridate::year(sub$bdate)),
                                                                                 sep = "-"), format = "%d-%m-%Y")) - min(as.numeric(cdate2)) +
                                                          1)/7)
        }
      }
      else {
        bintno <- ceiling((as.numeric(as.Date(paste(refday[1],
                                                    refday[2], year(bdate), sep = "-"), format = "%d-%m-%Y")) -
                             min(as.numeric(cdate2)) + 1)/7)
      }
    }
    else {
      bintno <- realbintno
    }
  }
  else if (cinterval == "month") {
    cmonth <- lubridate::month(cdate2)
    cyear <- year(cdate2) - min(year(cdate2))
    cintno <- cmonth + 12 * cyear
    realbintno <- lubridate::month(bdate) + 12 * (year(bdate) -
                                                    min(year(cdate2)))
    if (is.null(spatial) == FALSE) {
      newclim <- data.frame(cintno = cintno, xvar = xvar,
                            spatial = climspatial)
      newclim2 <- melt(newclim, id = c("cintno", "spatial"))
      newclim3 <- cast(newclim2, cintno + spatial ~ variable,
                       mean)
      newclim3 <- newclim3[order(newclim3$spatial, newclim3$cintno),
                           ]
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
      climspatial <- newclim3$spatial
    }
    else {
      newclim <- data.frame(cintno = cintno, xvar = xvar)
      newclim2 <- melt(newclim, id = "cintno")
      newclim3 <- cast(newclim2, cintno ~ variable, mean)
      cintno <- newclim3$cintno
      xvar <- newclim3$xvar
    }
    if (type == "absolute") {
      if (is.null(cohort) == FALSE) {
        newdat <- cbind(as.data.frame(bdate), as.data.frame(cohort))
        datenum <- 1
        bintno <- seq(1, length(bdate), 1)
        for (i in levels(as.factor(cohort))) {
          sub <- subset(newdat, cohort == i)
          bintno[as.numeric(rownames(sub))] <- refday[2] +
            12 * (min(lubridate::year(sub$bdate)) - min(lubridate::year(cdate2)))
        }
      }
      else {
        bintno <- refday[2] + 12 * (year(bdate) - min(year(cdate2)))
      }
    }
    else {
      bintno <- realbintno
    }
  }
  if (cinterval == "day") {
    if ((min(bintno) - range[1]) < min(cintno)) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  if (cinterval == "week") {
    if ((min(bintno) - range[1] * 7) < min(cintno)) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  if (cinterval == "month") {
    if ((as.numeric(min(as.Date(bdate, format = "%d/%m/%Y")) -
                    months(range[1])) - (as.numeric(min(as.Date(cdate,
                                                                format = "%d/%m/%Y"))))) <= 0) {
      stop("You do not have enough climate data to search that far back. Please adjust the value of range or add additional climate data.")
    }
  }
  if (max(bintno) > max(cintno)) {
    if (type == "absolute") {
      stop("You need more recent biological data. This error may be caused by your choice of refday")
    }
    else {
      stop("You need more recent biological data")
    }
  }
  baseline <- update(baseline, . ~ .)
  nullmodel <- AICc(baseline)
  modlist <- list()
  cmatrix <- matrix(ncol = (duration), nrow = length(bdate))
  modeldat <- model.frame(baseline)
  modeldat$yvar <- modeldat[, 1]
  if (is.null(centre[[1]]) == FALSE) {
    func = "centre"
  }
  if (length(modeldat$yvar) != length(bdate)) {
    stop("NA values present in biological response. Please remove NA values")
  }
  if (is.na(upper) == FALSE && is.na(lower) == TRUE) {
    if (binary == TRUE) {
      xvar <- ifelse(xvar > upper, 1, 0)
    }
    else {
      xvar <- ifelse(xvar > upper, xvar, 0)
    }
  }
  if (is.na(lower) == FALSE && is.na(upper) == TRUE) {
    if (binary == TRUE) {
      xvar <- ifelse(xvar < lower, 1, 0)
    }
    else {
      xvar <- ifelse(xvar < lower, xvar, 0)
    }
  }
  if (is.na(lower) == FALSE && is.na(upper) == FALSE) {
    if (binary == TRUE) {
      xvar <- ifelse(xvar > lower & xvar < upper, 1, 0)
    }
    else {
      xvar <- ifelse(xvar > lower & xvar < upper, xvar -
                       lower, 0)
    }
  }
  if (is.null(spatial) == FALSE) {
    cintno = data.frame(Date = cintno, spatial = climspatial)
    bintno = data.frame(Date = bintno, spatial = spatial[[1]])
    xvar = data.frame(Clim = xvar, spatial = climspatial)
    for (i in 1:length(bdate)) {
      cmatrix[i, ] <- xvar[which(cintno$spatial %in% bintno$spatial[i] &
                                   cintno$Date %in% (bintno$Date[i] - c(range[2]:range[1]))),
                           1]
    }
  }
  else {
    for (i in 1:length(bdate)) {
      cmatrix[i, ] <- xvar[which(cintno %in% (bintno[i] -
                                                c(range[2]:range[1])))]
    }
  }
  cmatrix <- as.matrix(cmatrix[, c(ncol(cmatrix):1)])
  if (cmissing == FALSE && length(which(is.na(cmatrix))) >
      0) {
    if (cinterval == "day") {
      .GlobalEnv$missing <- as.Date(cintno[is.na(xvar)],
                                    origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                      1)
    }
    if (cinterval == "month") {
      .GlobalEnv$missing <- c(paste("Month:", month(as.Date(cintno[is.na(xvar)],
                                                            origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                              1)), "Year:", year(as.Date(cintno[is.na(xvar)],
                                                                                         origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                                           1))))
    }
    if (cinterval == "week") {
      .GlobalEnv$missing <- c(paste("Week:", month(as.Date(cintno[is.na(xvar)],
                                                           origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                             1)), "Year:", year(as.Date(cintno[is.na(xvar)],
                                                                                        origin = min(as.Date(cdate, format = "%d/%m/%Y")) -
                                                                                          1))))
    }
    stop(c("Climate data should not contain NA values: ",
           length(.GlobalEnv$missing), " NA value(s) found. Please add missing climate data or set cmissing = TRUE.\n           See object missing for all missing climate data"))
  }
  if (cmissing != FALSE && length(which(is.na(cmatrix))) >
      0) {
    print("Missing climate data detected. Please wait while appropriate data is calculated to replace NAs.")
    if (cmissing == "method1") {
      for (i in which(is.na(cmatrix))) {
        cmatrix[i] <- mean(c(cmatrix[i - (1:2)], cmatrix[i +
                                                           (1:2)]), na.rm = T)
        if (is.na(cmatrix[i])) {
          stop("Too many consecutive NAs present in the data. Consider using method2 or manually replacing NAs.")
        }
      }
    }
    else if (cmissing == "method2") {
      cdate_new <- data.frame(Date = as.Date(cdate, format = "%d/%m/%Y"),
                              Year = lubridate::year(as.Date(cdate, format = "%d/%m/%Y")),
                              Month = lubridate::month(as.Date(cdate, format = "%d/%m/%Y")),
                              Day = lubridate::day(as.Date(cdate, format = "%d/%m/%Y")))
      if (cinterval == "week") {
        for (j in 1:nrow(cdate_new)) {
          cdate_new$Week[j] <- ceiling((as.numeric(cdate_new$Date[j]) -
                                          min(as.numeric(subset(cdate_new, cdate_new$Year ==
                                                                  cdate_new$Year[j])$Date)) + 1)/7)
        }
      }
      for (i in which(is.na(cmatrix))) {
        col <- floor(i/nrow(cmatrix))
        if (is.null(spatial)) {
          brecord <- bintno[i - col * nrow(cmatrix)] -
            (range[2] + col) - 1
        }
        else {
          brecord <- bintno$Date[i - col * nrow(cmatrix)] -
            (range[2] + col) - 1
        }
        min_date <- min(as.Date(cdate, format = "%d/%m/%Y"))
        if (cinterval == "day") {
          missing_rec <- as.Date(brecord, format = "%d/%m/%Y",
                                 origin = min_date)
          cmatrix[i] <- mean(xvar[which(cdate_new$Month ==
                                          lubridate::month(missing_rec) & cdate_new$Day ==
                                          lubridate::day(missing_rec))], na.rm = T)
        }
        else if (cinterval == "week") {
          missing_week <- ceiling(((as.numeric((as.Date(bdate[i -
                                                                col * nrow(cmatrix)], format = "%d/%m/%Y"))) -
                                      (col * 7)) - as.numeric(as.Date(paste("01/01/",
                                                                            lubridate::year(as.Date(bdate[i - col * nrow(cmatrix)],
                                                                                                    format = "%d/%m/%Y")), sep = ""), format = "%d/%m/%Y")) +
                                     1)/7)
          cmatrix[i] <- mean(xvar[which(cdate_new$Week ==
                                          missing_week)], na.rm = T)
        }
        else if (cinterval == "month") {
          missing_month <- (lubridate::month(min(as.Date(cdate,
                                                         format = "%d/%m/%Y"))) + (which(is.na(xvar)) -
                                                                                     1)) - (floor((lubridate::month(min(as.Date(cdate,
                                                                                                                                format = "%d/%m/%Y"))) + (which(is.na(xvar)) -
                                                                                                                                                            1))/12) * 12)
          cmatrix[i] <- mean(xvar[which(cdate_new$Month ==
                                          missing_month)], na.rm = T)
        }
        if (is.na(cmatrix[i])) {
          stop("There is no data available for certain climate records across all years. Consider using method1 or manually replacing NAs.")
        }
      }
    }
    else {
      stop("cmissing should be FALSE, 'method1' or 'method2'")
    }
  }
  modeldat <- model.frame(baseline)
  modeldat$yvar <- modeldat[, 1]
  modeldat$climate <- matrix(ncol = 1, nrow = nrow(modeldat),
                             seq(from = 1, to = nrow(modeldat), by = 1))
  if (is.null(weights(baseline)) == FALSE) {
    if (class(baseline)[1] == "glm" & sum(weights(baseline)) ==
        nrow(model.frame(baseline)) || attr(class(baseline),
                                            "package") == "lme4" & sum(weights(baseline)) ==
        nrow(model.frame(baseline))) {
    }
    else {
      modeldat$modweights <- weights(baseline)
      baseline <- update(baseline, . ~ ., weights = modeldat$modweights,
                         data = modeldat)
    }
  }
  if (func == "lin") {
    modeloutput <- update(baseline, yvar ~ . + climate, data = modeldat)
  }
  else if (func == "quad") {
    modeloutput <- update(baseline, yvar ~ . + climate +
                            I(climate^2), data = modeldat)
  }
  else if (func == "cub") {
    modeloutput <- update(baseline, yvar ~ . + climate +
                            I(climate^2) + I(climate^3), data = modeldat)
  }
  else if (func == "log") {
    modeloutput <- update(baseline, yvar ~ . + log(climate),
                          data = modeldat)
  }
  else if (func == "inv") {
    modeloutput <- update(baseline, yvar ~ . + I(climate^-1),
                          data = modeldat)
  }
  else if (func == "centre") {
    if (centre[[2]] == "both") {
      modeldat$wgdev <- matrix(ncol = 1, nrow = nrow(cmatrix),
                               seq(from = 1, to = nrow(cmatrix), by = 1))
      modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(cmatrix),
                                seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~ . + wgdev +
                              wgmean, data = modeldat)
    }
    if (centre[[2]] == "mean") {
      modeldat$wgmean <- matrix(ncol = 1, nrow = nrow(cmatrix),
                                seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~ . + wgmean,
                            data = modeldat)
    }
    if (centre[[2]] == "dev") {
      modeldat$wgdev <- matrix(ncol = 1, nrow = nrow(cmatrix),
                               seq(from = 1, to = nrow(cmatrix), by = 1))
      modeloutput <- update(baseline, yvar ~ . + wgdev,
                            data = modeldat)
    }
  }
  else {
    print("Define func")
  }
  m <- range[2]
  n <- duration
  if (stat == "slope") {
    time <- seq(1, n, 1)
    modeldat$climate <- apply(cmatrix, 1, FUN = function(x) coef(lm(x ~
                                                                      time))[2])
  }
  else {
    ifelse(n == 1, modeldat$climate <- cmatrix, modeldat$climate <- apply(cmatrix,
                                                                          1, FUN = stat))
  }
  if (is.null(centre[[1]]) == FALSE) {
    if (centre[[2]] == "both") {
      modeldat$wgdev <- wgdev(modeldat$climate, centre[[1]])
      modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
      LocalBestModel <- update(modeloutput, . ~ ., data = modeldat)
    }
    if (centre[[2]] == "mean") {
      modeldat$wgmean <- wgmean(modeldat$climate, centre[[1]])
      LocalBestModel <- update(modeloutput, . ~ ., data = modeldat)
    }
    if (centre[[2]] == "dev") {
      modeldat$wgdev <- wgdev(modeldat$climate, centre[[1]])
      LocalBestModel <- update(modeloutput, . ~ . + wgdev,
                               data = modeldat)
    }
  }
  else {
    LocalBestModel <- update(modeloutput, . ~ ., data = modeldat)
  }
  LocalData <- model.frame(LocalBestModel)
  return(list(BestModel = LocalBestModel, BestModelData = LocalData,
              Dataset = data.frame(ModelAICc = AICc(LocalBestModel),
                                   deltaAICc = AICc(LocalBestModel) - nullmodel, WindowOpen = range[1],
                                   WindowClose = range[2])))
}