seatrackGLS <- function(luxfile, save_sun_plots.dir, save_filter_plots.dir,
                        speed, boundary.box, coast_to_land, coast_to_sea, loess_filter_k, months_breeding, man_equinox_periods,
                        aut_equinox_periods, noonfilter, daylengthfilter, midnightsun_removal, add_summer_pos, add_winter_pos, split_years, year_tracked,
                        sun_angle_start, sun_angle_end, light_threshold, logger_id, logger_model,
                        ring_number, species, colony, col_lon, col_lat, date_deployed, date_retrieved, age_deployed) {
  oldw <- getOption("warn")
  options(warn = -1)

  if (!exists("save_filter_plots.dir")) {
    save_filter_plots.dir <- NA
  }
  if (!exists("save_sun_plots.dir")) {
    save_sun_plots.dir <- NA
  }

  show_sun_plot <- TRUE
  if (is.na(save_sun_plots.dir)) {
    show_sun_plot <- FALSE
  }
  # if(exists("save_sun_plots.dir")){if(!is.na(save_sun_plots.dir)){show_sun_plot<-TRUE}}
  show_filter_plots <- TRUE
  if (is.na(save_filter_plots.dir)) {
    show_filter_plots <- FALSE
  }


  # if(exists("save_filter_plots.dir")){if(!is.na(save_filter_plots.dir)){show_filter_plots<-TRUE}}
  # if(!is.null(save_filter_plots.dir)){if(!is.na(save_filter_plots.dir)){show_filter_plots<-TRUE}}

  Sys.setenv(TZ = "GMT")
  logger_yeartracked <- paste(logger_id, logger_model, year_tracked, sep = "_")

  if (any(grepl(".lux", luxfile))) {
    lu <- read.table(luxfile, sep = "\t", header = F, fill = T, skip = 20)
  } else {
    lu <- read.table(luxfile, sep = ",", skip = 1, header = F, fill = T)
  }
  if (any(grepl(".lux", luxfile))) {
    lu$dtime <- datetime_conversion(lu[, 1])
  } else {
    lu$dtime <- datetime_conversion(lu[, 2])
  }
  if (any(grepl(".lux", luxfile))) {
    lu$lux <- lu[, 2]
  } else {
    lu$lux <- lu[, 4]
  }
  lu$lux <- as.numeric(gsub("\\,", ".", lu$lux))
  lu$date <- as.Date(lu$dtime)
  lu$date <- date_conversion(lu$date)
  lu$time <- strptime(paste("01.01.2000", substr(lu$dtime, 12, 19), sep = " "), "%d.%m.%Y %H:%M:%S")

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # limit data to year_tracked ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # limit first to date deployed/retrieved
  lu <- lu[lu$date < date_conversion(date_retrieved), ]
  lu <- lu[lu$date > date_conversion(date_deployed), ]

  # then limit data to a year_tracked
  firstdate_light <- date_conversion(date_deployed)
  if (substr(year_tracked, 1, 4) != year(date_conversion(date_deployed))) {
    firstdate_light <- paste(substr(year_tracked, 1, 4), "-", substr(split_years[2], 1, 2), "-", substr(split_years[2], 4, 5), sep = "")
  }

  lastdate_light <- date_conversion(date_retrieved)
  if (paste("20", substr(year_tracked, 6, 7), sep = "") != year(date_conversion(date_retrieved))) {
    lastdate_light <- paste("20", substr(year_tracked, 6, 7), "-", substr(split_years[1], 1, 2), "-", substr(split_years[1], 4, 5), sep = "")
  }

  lu <- lu[lu$date <= lastdate_light, ]
  lu <- lu[lu$date >= firstdate_light, ]
  lu <- lu[!is.na(lu$V1), ]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # run automated transition ----
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # define recording interval:
  li <- median(difftime(lu$dtime[2:nrow(lu)], lu$dtime[1:(nrow(lu) - 1)], units = "mins"))

  if (any(lu$V1 %in% c("ok", "OK", "Ok"))) {
    lu <- lu[lu$V1 %in% c("ok", "OK", "Ok"), ]
  }
  trn2 <- NULL
  # turn light curves into twilight data
  tryCatch(
    {
      trn2 <- twilightCalc_bugfree(lu$dtime, lu$lux, ask = F, preSelection = TRUE, allTwilights = FALSE, LightThreshold = light_threshold, maxLight = li)
    },
    error = function(e) {}
  )

  # among small migrate tech. loggers (c65, f100) some throw an error when running twilightCalc, the code below fixes that:
  if (is.null(trn2) & any(grepl(".lux", luxfile)) & logger_model %in% c("f100", "c65")) {
    lu$lux <- log(lu$lux)
  }
  if (is.null(trn2) & any(grepl(".lux", luxfile)) & logger_model %in% c("f100", "c65")) {
    trn2 <- twilightCalc_bugfree(lu$dtime, lu$lux, ask = F, preSelection = TRUE, allTwilights = FALSE, LightThreshold = light_threshold, maxLight = li)
  }


  # type=1 and type=2 not always correctly assigned, this fixes most of these cases:
  for (i in 1:(nrow(trn2) - 2)) {
    if (sum(trn2$type[i:(i + 2)]) == 3) {
      trn2$type[i + 1] <- 2
    }
    if (sum(trn2$type[i:(i + 2)]) == 6) {
      trn2$type[i + 1] <- 1
    }
  }

  # plot
  if (show_filter_plots == TRUE) {
    with_preslect <- trn2
    no_preselect <- twilightCalc_bugfree(lu$dtime, lu$lux, ask = F, preSelection = FALSE, allTwilights = FALSE, LightThreshold = light_threshold, maxLight = li)
    with_preslect$hours <- as.numeric(format(with_preslect[, 1], "%H")) + as.numeric(format(with_preslect[, 1], "%M")) / 60
    no_preselect$hours <- as.numeric(format(no_preselect[, 1], "%H")) + as.numeric(format(no_preselect[, 1], "%M")) / 60

    par(mfrow = c(1, 1))

    if (!is.null(save_filter_plots.dir)) {
      png(filename = paste(save_filter_plots.dir, logger_yeartracked, "_step2_twilightCalc", ".png", sep = ""), height = 8, width = 10, units = "cm", res = 500)
      plot(no_preselect$tFirst, no_preselect$hours, col = "firebrick", ylim = c(0, 26), ann = FALSE, pch = 19, yaxt = "none", xaxt = "none", cex = 0.3)
      mtext(side = 1, text = "Month", line = 1, cex = 0.7)
      mtext(side = 2, text = "Time of day (GMT)", line = 1, cex = 0.7)
      mtext(side = 3, text = logger_yeartracked, line = 0.5, cex = 0.7)
      points(with_preslect$tFirst[with_preslect$type == 2], with_preslect$hours[with_preslect$type == 2], col = "cornflowerblue", pch = 19, cex = 0.3)
      points(with_preslect$tFirst[with_preslect$type == 1], with_preslect$hours[with_preslect$type == 1], col = "lightblue", pch = 19, cex = 0.3)
      daterange <- c(as.POSIXlt(min(with_preslect$tFirst)), as.POSIXlt(max(with_preslect$tFirst)))
      axis.POSIXct(1, at = seq(daterange[1], daterange[2], by = "month"), format = "%b", cex.axis = 0.6, tck = -0.02, mgp = c(3, 0, 0))
      axis(side = 2, at = c(1:24), labels = c(1:24), tck = -0.02, cex.axis = 0.6, las = 2, mgp = c(3, 0.3, 0))
      legend("top", legend = c("sunrise", "sunset", "ignored"), horiz = TRUE, col = c("lightblue", "cornflowerblue", "firebrick"), pch = 19, cex = 0.3)
      dev.off()
    }
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # advance sunset (shorten twilightCalc's advance by 1 minute,
  #          to follow procedures from transEdit and IntiProc)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  trn2$tFirst[trn2$type == 2] <- trn2$tFirst[trn2$type == 2] + 60
  trn2$tSecond[trn2$type == 1] <- trn2$tSecond[trn2$type == 1] + 60

  trn2 <- trn2[, c(1:3)]

  # keep track of n edited twilights/filtered positions
  filtering <- as.data.frame(logger_yeartracked)
  colnames(filtering) <- c("logger_yeartracked")
  filtering$nrow_twilightCalc <- nrow(trn2)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # remove twilights of same type, less than 22 hours apart within the same day
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  before_tw_remov <- trn2 # for later plotting

  months_extensive_legtucking <- NA # to be removed - must first remove the option from the relevant function

  par(mfrow = c(1, 1))
  if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
    png(filename = paste(save_filter_plots.dir, logger_yeartracked, "_step3_RemovedExtraTwilights", ".png", sep = ""), height = 8, width = 10, units = "cm", res = 500)
  }

  trn2 <- twilight_cleanup(df = trn2, breedingloc_lon = col_lon, breedingloc_lat = col_lat, months_breeding = months_breeding, species = species, sun_angle_start = sun_angle_start, sun_angle_end = sun_angle_end, show_plot = show_filter_plots)
  if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
    dev.off()
  }

  # keep track of n edited twilights/filtered positions
  filtering$removed_twilight_cleanup <- (filtering$nrow_twilightCalc - nrow(trn2))

  after_tw_remov <- trn2 # for later plotting


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # detect outliers and move them back to mean of their neighbors
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  before_changes <- trn2 # for later plotting
  # this filter require sun angles in order to get location estimates
  temp <- trn2
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(-3.5)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(-3.5)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- as.numeric(sun_angle_start)
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- as.numeric(sun_angle_start)
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst)))
  }

  par(mfrow = c(1, 1))
  if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
    png(filename = paste(save_filter_plots.dir, logger_yeartracked, "_step4_MoveSkewedTwilightstoMean", ".png", sep = ""), height = 8, width = 10, units = "cm", res = 500)
  }


  tab2 <- move_twilights(df = trn2, speed = speed, sun = temp$sun, show_plot = show_filter_plots)

  if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
    dev.off()
  }


  # keep track of n edited twilights/filtered positions
  filtering$moved_twilights <- nrow(tab2[!(trn2$tFirst %in% tab2$tFirst), ])
  after_changes <- tab2 # for later plotting

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # daylengthfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  before_daylengthfilter <- tab2 # for later plotting

  temp <- tab2

  if (is.null(noonfilter)) {
    daylengthfilter <- TRUE
  }
  if (daylengthfilter == TRUE) {
    par(mfrow = c(1, 1))
    if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
      png(filename = paste(save_filter_plots.dir, logger_yeartracked, "_step5_daylengthfilter", ".png", sep = ""), height = 8, width = 10, units = "cm", res = 500)
    }

    tab2 <- daylengthfilter(df = tab2, show_plot = show_filter_plots)

    if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
      dev.off()
    }
  }

  # keep track of n removed twilights/filtered positions
  filtering$daylengthfilter <- (nrow(before_daylengthfilter) - nrow(tab2))
  after_daylengthfilter <- tab2 # for later plotting

  temp <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # noonfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  before_noonfilter <- tab2 # for later plotting

  if (is.null(noonfilter)) {
    noonfilter <- TRUE
  }
  if (noonfilter == TRUE) {
    par(mfrow = c(1, 1))
    if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
      png(filename = paste(save_filter_plots.dir, logger_yeartracked, "_step6_noonfilter", ".png", sep = ""), height = 8, width = 10, units = "cm", res = 500)
    }

    tab2 <- noonfilter(df = tab2, show_plot = show_filter_plots)

    if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
      dev.off()
    }
  }

  # keep track of n removed twilights/filtered positions
  filtering$noonfilter <- (nrow(before_noonfilter) - nrow(tab2))
  after_before_noonfilter <- tab2 # for later plotting

  temp <- NULL

  twilights_with_no_positional_filtering <- tab2
  twilights_with_no_positional_filtering$logger_yeartracked <- logger_yeartracked
  twilights_with_no_positional_filtering$ring_number <- ring_number
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CALCULATE POSITIONS---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- tab2
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(-3.5)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(-3.5)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- as.numeric(sun_angle_start)
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- as.numeric(sun_angle_start)
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst)))
  }

  latlon <- GeoLight::coord(tab2$tFirst, tab2$tSecond, tab2$type, degElevation = temp$sun, note = F)
  postab1 <- cbind(tab2, latlon)

  temp <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Temp smoothing x 2---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  temp <- postab1
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(-3.5)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(-3.5)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- as.numeric(sun_angle_start)
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- as.numeric(sun_angle_start)
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst)))
  }

  posdata <- double_smoothing(df = postab1, sun = temp$sun)
  temp <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Equinox-filter ---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  posdata$eqfilter <- 1

  first_aut_eq_filter <- paste(unique(year(posdata$tFirst))[1], "-", man_equinox_periods[3], sep = "")
  last_aut_eq_filter <- paste(unique(year(posdata$tFirst))[1], "-", man_equinox_periods[4], sep = "")
  first_spring_eq_filter <- paste(unique(year(posdata$tFirst))[1] + 1, "-", man_equinox_periods[1], sep = "")
  last_spring_eq_filter <- paste(unique(year(posdata$tFirst))[1] + 1, "-", man_equinox_periods[2], sep = "")

  posdata$eqfilter[posdata$date >= first_aut_eq_filter & posdata$date <= last_aut_eq_filter] <- 0
  posdata$eqfilter[posdata$date >= first_spring_eq_filter & posdata$date <= last_spring_eq_filter] <- 0


  # an alternative way (not yet published)

  temp <- posdata
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(-3.5)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(-3.5)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- as.numeric(sun_angle_start)
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- as.numeric(sun_angle_start)
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst)))
  }

  if (aut_equinox_periods == TRUE) {
    posdata$eqfilter <- assign_equinox_periods(lats = posdata$lat_smooth2, dates = posdata$tFirst, breedingloc_lat = col_lat, sun = temp$sun)
  }
  temp <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Speed filtering --------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # does not affect latitudes within the equinox period
  before_speed <- as.data.frame(posdata$lat[posdata$eqfilter == 1]) # for later plotting
  colnames(before_speed) <- c("lat") # for later plotting
  before_speed$lon <- posdata$lon[posdata$eqfilter == 1] # for later plotting
  utvalg <- posdata

  # distance between pos1 and pos2, etc
  utvalg$distance <- NA
  no_na <- utvalg[!is.na(utvalg$lat_smooth2), ]
  no_na$distance[1:(length(no_na$lat_smooth2) - 1)] <- acos(sin(no_na$lat_smooth2[1:(length(no_na$lat_smooth2) - 1)] / 180 * pi) * sin(no_na$lat_smooth2[2:length(no_na$lat_smooth2)] / 180 * pi) + cos(no_na$lat_smooth2[1:(length(no_na$lat_smooth2) - 1)] / 180 * pi) * cos(no_na$lat_smooth2[2:length(no_na$lat_smooth2)] / 180 * pi) * cos(no_na$lon_smooth2[1:(length(no_na$lat_smooth2) - 1)] / 180 * pi - no_na$lon_smooth2[2:length(no_na$lat_smooth2)] / 180 * pi)) * 6371
  no_na$distance[length(no_na$lat_smooth2)] <- no_na$distance[length(no_na$lat_smooth2) - 1]
  no_na$time_diff <- NA
  no_na$time_diff[1:(length(no_na$lat_smooth2) - 1)] <- abs(difftime(no_na$tFirst[1:(length(no_na$lat_smooth2) - 1)], no_na$tFirst[2:length(no_na$lat_smooth2)], units = "hours"))
  no_na$time_diff[length(no_na$lat_smooth2)] <- no_na$time_diff[length(no_na$lat_smooth2) - 1]
  travel_distance <- no_na$distance / no_na$time_diff

  utvalg$distance[!is.na(utvalg$lat_smooth2)] <- travel_distance

  test <- utvalg
  test$time_diff <- NA
  test$speed <- NA
  test$time_diff[!is.na(test$lat_smooth2)] <- no_na$time_diff
  test$speed[!is.na(test$lat_smooth2)] <- speed
  test$speed[test$time_diff > 14 & test$time_diff < 18 & !is.na(test$lat_smooth2)] <- test$speed[test$time_diff > 14 & test$time_diff < 18 & !is.na(test$lat_smooth2)] * 0.95
  test$speed[test$time_diff > 18 & test$time_diff < 22 & !is.na(test$lat_smooth2)] <- test$speed[test$time_diff > 18 & test$time_diff < 22 & !is.na(test$lat_smooth2)] * 0.8
  test$speed[test$time_diff > 22 & test$time_diff < 30 & !is.na(test$lat_smooth2)] <- test$speed[test$time_diff > 22 & test$time_diff < 30 & !is.na(test$lat_smooth2)] * 0.75
  test$speed[test$time_diff > 30 & test$time_diff < 40 & !is.na(test$lat_smooth2)] <- test$speed[test$time_diff > 30 & test$time_diff < 40 & !is.na(test$lat_smooth2)] * 0.6
  test$speed[test$time_diff > 40 & !is.na(test$lat_smooth2)] <- test$speed[test$time_diff > 40 & !is.na(test$lat_smooth2)] * 0.5

  utvalg$keep <- TRUE
  utvalg$speed <- NA
  utvalg$speed <- speed
  utvalg$speed[!is.na(utvalg$lat_smooth2)] <- test$speed[!is.na(test$lat_smooth2)]
  utvalg$keep[utvalg$eqfilter == 1 & utvalg$distance > utvalg$speed] <- FALSE
  utvalg$speed <- NULL
  utvalg <- utvalg[utvalg$keep == TRUE, ]
  utvalg$keep <- NULL

  test <- NULL

  # extra filtering of longitudes during equinox:

  i.get.outliers <- function(residuals, k = 3) {
    x <- residuals
    # x is a vector of residuals
    # k is a measure of how many interquartile ranges to take before saying that point is an outlier
    # it looks like 3 is a good preset for k
    QR <- quantile(x, probs = c(0.25, 0.75))
    IQR <- QR[2] - QR[1]
    Lower.band <- QR[1] - (k * IQR)
    Upper.Band <- QR[2] + (k * IQR)
    delete <- which(x < Lower.band | x > Upper.Band)
    return(as.vector(delete))
  }

  test <- utvalg


  test$delta_lon_forward <- NA
  test$delta_lon_forward[1:(length(test$lon_smooth2) - 1)] <- (test$lon_smooth2[1:(length(test$lon_smooth2) - 1)] - test$lon_smooth2[2:length(test$lon_smooth2)])
  test$delta_lon_forward[length(test$lat_smooth2)] <- test$delta_lon_forward[length(test$lon_smooth2) - 1]

  test$delta_lon_backward <- NA
  test$delta_lon_backward[2:length(test$lon)] <- (test$lon[2:length(test$lon)] - test$lon[1:(length(test$lon) - 1)])
  test$delta_lon_backward[1] <- test$delta_lon_backward[2]

  test$delta_lon <- abs(test$delta_lon_backward + test$delta_lon_forward) / 2

  del.lon <- i.get.outliers(as.vector(residuals(loess(test$delta_lon ~ as.numeric(test$tFirst), span = 0.1))), k = 3)

  test$filter <- 1
  test$filter[del.lon] <- 0
  test$filter[test$eqfilter == 1] <- 1
  test$filter[test$lon_smooth2 > 170] <- 1
  test$filter[test$lon_smooth2 < -170] <- 1
  utvalg <- utvalg[test$filter == 1, ]





  after_speed <- as.data.frame(utvalg$lat[utvalg$eqfilter == 1]) # for later plotting
  colnames(after_speed) <- c("lat") # for later plotting
  after_speed$lon <- utvalg$lon[utvalg$eqfilter == 1] # for later plotting


  postab1 <- utvalg
  postab1$distance <- NULL

  # keep track of n edited twilights/filtered positions
  filtering$removed_speed <- nrow(posdata) - nrow(postab1)


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # BOUNDARY BOX---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # latitudes only filtered outside the equinox
  before_b_box <- as.data.frame(postab1$lat[postab1$eqfilter == 1]) # for later plotting
  colnames(before_b_box) <- c("lat") # for later plotting
  before_b_box$lon <- postab1$lon[postab1$eqfilter == 1] # for later plotting


  # adapt coordinates to Pacific birds
  if (boundary.box[2] > 180 | boundary.box[1] > 180) postab1$lon[!is.na(postab1$lon) & postab1$lon < 0] <- postab1$lon[!is.na(postab1$lon) & postab1$lon < 0] + 360

  postab1$lon_filter <- 1
  postab1$lat_filter <- 1

  postab1$lon_filter[postab1$lon > boundary.box[2]] <- 0
  postab1$lon_filter[postab1$lon < boundary.box[1]] <- 0
  postab1$lat_filter[postab1$lat < boundary.box[3]] <- 0
  postab1$lat_filter[postab1$lat > boundary.box[4]] <- 0

  postab2 <- postab1

  for (i in 1:(length(postab1$tFirst))) {
    if (postab1$lon_filter[i] == "0") postab2$tFirst[i] <- NA # BOUNDARY FOR LON (all data)
    if (postab1$lat_filter[i] == "0" & postab1$eqfilter[i] == "1") postab2$tFirst[i] <- NA # BOUNDARY FOR LAT (non-equinox data)
  }
  postab3 <- postab2[!is.na(postab2$tFirst), ]
  postab3 <- postab3[complete.cases(postab3), ]

  # keep track of n edited twilights/filtered positions
  filtering$removed_boundbox <- nrow(postab1) - nrow(postab3)

  tab2 <- NULL
  postab1 <- NULL
  postab1 <- postab3
  postab1$lon_filter <- NULL
  postab1$lat_filter <- NULL

  if (boundary.box[2] > 180 | boundary.box[1] > 180) postab1$lon[!is.na(postab1$lon) & postab1$lon > 180] <- postab1$lon[!is.na(postab1$lon) & postab1$lon > 180] - 360

  after_b_box <- as.data.frame(postab1$lat[postab1$eqfilter == 1]) # for later plotting
  colnames(after_b_box) <- c("lat") # for later plotting
  after_b_box$lon <- postab1$lon[postab1$eqfilter == 1] # for later plotting


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ARGOSFILTER---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  before_angle <- as.data.frame(postab1$lat[postab1$eqfilter == 1]) # for later plotting
  colnames(before_angle) <- c("lat") # for later plotting
  before_angle$lon <- postab1$lon[postab1$eqfilter == 1] # for later plotting

  tryCatch(
    {
      tab3 <- postab1
      tab3$dtime <- as.POSIXct(postab1$tFirst)
      # insert colony as the four first and four last coordinates (removed afterwards)
      tab3 <- rbind(tab3[1, ], tab3)
      tab3$dtime[1] <- tab3$dtime[1] - 43200
      tab3$lon_smooth2[1] <- col_lon
      tab3$lat_smooth2[1] <- col_lat
      if (tab3$type[2] %in% 1) tab3$type[1] <- 2
      if (tab3$type[2] %in% 2) tab3$type[1] <- 1
      tab3 <- rbind(tab3[1, ], tab3)
      tab3$dtime[1] <- tab3$dtime[1] - 43200
      tab3$lon_smooth2[1] <- col_lon
      tab3$lat_smooth2[1] <- col_lat
      if (tab3$type[2] %in% 1) tab3$type[1] <- 2
      if (tab3$type[2] %in% 2) tab3$type[1] <- 1
      tab3 <- rbind(tab3[1, ], tab3)
      tab3$dtime[1] <- tab3$dtime[1] - 43200
      tab3$lon_smooth2[1] <- col_lon
      tab3$lat_smooth2[1] <- col_lat
      if (tab3$type[2] %in% 1) tab3$type[1] <- 2
      if (tab3$type[2] %in% 2) tab3$type[1] <- 1
      tab3 <- rbind(tab3[1, ], tab3)
      tab3$dtime[1] <- tab3$dtime[1] - 43200
      tab3$lon_smooth2[1] <- col_lon
      tab3$lat_smooth2[1] <- col_lat
      if (tab3$type[2] %in% 1) tab3$type[1] <- 2
      if (tab3$type[2] %in% 2) tab3$type[1] <- 1
      tab3 <- rbind(tab3, tab3[nrow(tab3), ])
      tab3$dtime[nrow(tab3)] <- tab3$dtime[nrow(tab3)] + 43200
      tab3$lon_smooth2[nrow(tab3)] <- col_lon
      tab3$lat_smooth2[nrow(tab3)] <- col_lat
      if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
      if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
      tab3 <- rbind(tab3, tab3[nrow(tab3), ])
      tab3$dtime[nrow(tab3)] <- tab3$dtime[nrow(tab3)] + 43200
      tab3$lon_smooth2[nrow(tab3)] <- col_lon
      tab3$lat_smooth2[nrow(tab3)] <- col_lat
      if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
      if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
      tab3 <- rbind(tab3, tab3[nrow(tab3), ])
      tab3$dtime[nrow(tab3)] <- tab3$dtime[nrow(tab3)] + 43200
      tab3$lon_smooth2[nrow(tab3)] <- col_lon
      tab3$lat_smooth2[nrow(tab3)] <- col_lat
      if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
      if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
      tab3 <- rbind(tab3, tab3[nrow(tab3), ])
      tab3$dtime[nrow(tab3)] <- tab3$dtime[nrow(tab3)] + 43200
      tab3$lon_smooth2[nrow(tab3)] <- col_lon
      tab3$lat_smooth2[nrow(tab3)] <- col_lat
      if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
      if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1

      tab3$lc <- 1

      distlim1 <- (((speed * 2) * 12) / 2) * 1000
      distlim2 <- (((speed * 2) * 12) * 1000)

      tab3.1 <- tab3[tab3$type %in% 1, ]
      tab3.2 <- tab3[tab3$type %in% 2, ]
      tab3.1$argosfilter1 <- sdafilter(tab3.1$lat_smooth2, tab3.1$lon_smooth2, tab3.1$dtime, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
      tab3.2$argosfilter1 <- sdafilter(tab3.2$lat_smooth2, tab3.2$lon_smooth2, tab3.2$dtime, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
      tab3 <- rbind(tab3.1, tab3.2)
      tab3 <- tab3[order(tab3$dtime), ]

      distlim1 <- ((speed * 12) / 2) * 1000
      distlim2 <- ((speed * 12) * 1000)

      tab3$argosfilter2 <- sdafilter(tab3$lat_smooth2, tab3$lon_smooth2, tab3$dtime, tab3$lc, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))

      # remove first colony line
      tab3 <- tab3[5:(nrow(tab3) - 4), ]

      tab4 <- NULL
      tab4 <- tab3[tab3$argosfilter1 %in% c("not", "end_location") & tab3$eqfilter == 1, ]
      tab4 <- tab4[tab4$argosfilter2 %in% c("not", "end_location") & tab4$eqfilter == 1, ]
      tab5 <- tab3[tab3$eqfilter == 0, ]

      tab6 <- rbind(tab4, tab5)
      newSorted <- tab6[order(tab6$tFirst), ]

      postab1 <- NULL
      postab1 <- newSorted[c("tFirst", "tSecond", "type", "diff.h", "eqfilter", "lon", "lat")]

      filtering$removed_argos <- nrow(tab3) - nrow(postab1)
    },
    error = function(e) {}
  )

  # keep track of n edited twilights/filtered positions
  if (!(length(filtering$removed_argos) > 0)) filtering$removed_argos <- 0

  after_angle <- as.data.frame(postab1$lat[postab1$eqfilter == 1]) # for later plotting
  colnames(after_angle) <- c("lat") # for later plotting
  after_angle$lon <- postab1$lon[postab1$eqfilter == 1] # for later plotting


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOESS FILTER---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  postab1_loess <- postab1
  before_loess <- as.data.frame(postab1$lat[postab1$eqfilter == 1]) # for later plotting
  colnames(before_loess) <- c("lat") # for later plotting
  before_loess$lon <- postab1$lon[postab1$eqfilter == 1] # for later plotting

  if (difftime(postab1_loess$tFirst[length(postab1_loess$tFirst)], postab1_loess$tFirst[1], units = "days") > 45) {
    loess_filter <- loessFilter(postab1_loess$tFirst, postab1_loess$tSecond, postab1_loess$type, k = loess_filter_k, plot = F)
    postab1_loess_2 <- postab1_loess[loess_filter, ]
  }

  if (difftime(postab1_loess$tFirst[length(postab1_loess$tFirst)], postab1_loess$tFirst[1], units = "days") < 46) {
    postab1_loess_2 <- postab1_loess
  }

  # keep track of n edited twilights/filtered positions
  filtering$removed_loess <- nrow(postab1) - nrow(postab1_loess_2)

  postab1 <- NULL
  postab1 <- postab1_loess_2[c(1:7)]
  postab1$eqfilter <- postab1_loess_2$eqfilter

  after_loess <- as.data.frame(postab1$lat[postab1$eqfilter == 1]) # for later plotting
  colnames(after_loess) <- c("lat") # for later plotting
  after_loess$lon <- postab1$lon[postab1$eqfilter == 1] # for later plotting


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Exclude midnight sun period at colony-----------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # logic: i+3<=25h = START, i-3 <=25h = STOP
  aut_midnightsun_removal_loop <- midnightsun_removal

  # if(col_lat<60)aut_midnightsun_removal_loop<-FALSE
  if (aut_midnightsun_removal_loop == TRUE) {
    postab1 <- aut_midnight_sun_removal(df = postab1)
  }

  # keep track of n edited twilights/filtered positions
  one <- filtering$nrow_twilightCalc - filtering$removed_twilight_cleanup
  two <- one - filtering$removed_speed
  three <- two - filtering$removed_boundbox
  four <- three - filtering$removed_argos
  five <- four - filtering$removed_loess
  filtering$removed_midnight_sun <- five - nrow(postab1)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # END OF FILTERING---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Smoothing x 2---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- postab1
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(-3.5)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(-3.5)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- as.numeric(sun_angle_start)
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- as.numeric(sun_angle_start)
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst)))
  }

  posdata <- double_smoothing(df = postab1, sun = temp$sun)
  temp <- NULL


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add information to the final data table ----------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  posdata$diff.h <- NULL
  posdata$lon_unsmooth <- posdata$lon
  posdata$lat_unsmooth <- posdata$lat
  posdata$lon <- NULL
  posdata$lat <- NULL
  posdata$logger_id <- logger_id
  posdata$logger_model <- logger_model
  posdata$ring_number <- ring_number
  posdata$year_deployed <- year(date_conversion(date_deployed))
  posdata$year_retrieved <- year(date_conversion(date_retrieved))
  posdata$year_tracked <- year_tracked
  posdata$species <- species
  posdata$colony <- colony
  posdata$col_lon <- col_lon
  posdata$col_lat <- col_lat
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(-3.5)
  }
  if (is.na(sun_angle_start)) {
    posdata$sun <- as.numeric(-3.5)
  }
  if (!is.na(sun_angle_start)) {
    posdata$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    posdata$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(posdata$tFirst))
  }
  posdata$script_version <- "seatrackGLSv3"
  posdata$light_threshold <- light_threshold
  posdata$conf <- NULL
  posdata$lon_smooth1 <- NULL
  posdata$lat_smooth1 <- NULL
  posdata$lon <- posdata$lon_smooth2
  posdata$lat <- posdata$lat_smooth2
  posdata$lon_smooth2 <- NULL
  posdata$lat_smooth2 <- NULL


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Save plots of filtering---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
    tiff(filename = paste(save_filter_plots.dir, logger_yeartracked, "speed_bbox_angleanddist_loess_end", ".tiff", sep = ""), height = 22, width = 20, units = "cm", compression = "lzw", res = 600)
  }

  # plot maps:
  if (show_filter_plots == TRUE) {
    par(mfrow = c(4, 2))
    par(mar = c(2, 3, 2, 2) + 0.1)
    par(oma = c(2, 2, 2, 2))

    # center map on the Pacific or the Atlantic?
    map_ver <- "world"
    adjust_lon <- 0
    if ((nrow(posdata[lubridate::month(posdata$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4) & abs(posdata$lon) > 90, ]) / nrow(posdata[lubridate::month(posdata$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4), ])) > 0.5) adjust_lon <- 360
    if (adjust_lon == 360) map_ver <- "world2"

    ###### DEFINE MAP LIMITS (for plots of filtering):
    xlim <- c(as.data.frame(boundary.box)[1, ], as.data.frame(boundary.box)[2, ])
    ylim <- c(as.data.frame(boundary.box)[3, ], as.data.frame(boundary.box)[4, ])

    if (adjust_lon == 360 & as.data.frame(boundary.box)[2, ] <= 180) {
      xlim <- xlim + 180
    }
    ##################

    # before and after twilight removal

    if (is.null(sun_angle_start)) {
      sun_angle_start <- as.numeric(-3.5)
    }
    if (is.na(sun_angle_start)) {
      sun <- as.numeric(-3.5)
    }
    if (!is.na(sun_angle_start)) {
      sun <- as.numeric(sun_angle_start)
    }
    if (is.null(sun_angle_end)) {
      sun_angle_end <- as.numeric(sun_angle_start)
    }
    if (!is.na(sun_angle_end)) {
      sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(before_tw_remov$tFirst)))
    }

    latlon <- GeoLight::coord(before_tw_remov$tFirst, before_tw_remov$tSecond, before_tw_remov$type, degElevation = sun, note = F)
    basis <- cbind(before_tw_remov, latlon)

    if (is.null(sun_angle_start)) {
      sun_angle_start <- as.numeric(-3.5)
    }
    if (is.na(sun_angle_start)) {
      sun <- as.numeric(-3.5)
    }
    if (!is.na(sun_angle_start)) {
      sun <- as.numeric(sun_angle_start)
    }
    if (is.null(sun_angle_end)) {
      sun_angle_end <- as.numeric(sun_angle_start)
    }
    if (!is.na(sun_angle_end)) {
      sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(after_tw_remov$tFirst)))
    }

    latlon <- GeoLight::coord(after_tw_remov$tFirst, after_tw_remov$tSecond, after_tw_remov$type, degElevation = sun, note = F)
    after <- cbind(after_tw_remov, latlon)

    start_aut_eq <- min(posdata$tFirst[month(posdata$tFirst) %in% c(8, 9, 10, 11) & posdata$eqfilter == 0])
    end_aut_eq <- max(posdata$tFirst[month(posdata$tFirst) %in% c(8, 9, 10, 11) & posdata$eqfilter == 0])
    start_spr_eq <- min(posdata$tFirst[month(posdata$tFirst) %in% c(2, 3, 4) & posdata$eqfilter == 0])
    end_spr_eq <- max(posdata$tFirst[month(posdata$tFirst) %in% c(2, 3, 4) & posdata$eqfilter == 0])

    basis$eqfilter <- 1
    basis$eqfilter[basis$tFirst >= start_aut_eq & basis$tFirst <= end_aut_eq] <- 0
    basis$eqfilter[basis$tFirst >= start_spr_eq & basis$tFirst <= end_spr_eq] <- 0

    after$eqfilter <- 1
    after$eqfilter[after$tFirst >= start_aut_eq & after$tFirst <= end_aut_eq] <- 0
    after$eqfilter[after$tFirst >= start_spr_eq & after$tFirst <= end_spr_eq] <- 0


    tab2$tFirst <- as.POSIXct(as.character(tab2$tFirst), "UTC")
    tab2$tSecond <- as.POSIXct(as.character(tab2$tSecond), "UTC")

    before <- basis[!(basis$tFirst %in% after$tFirst), ]

    basis$lon[basis$lon < 0] <- basis$lon[basis$lon < 0] + adjust_lon
    after$lon[after$lon < 0] <- after$lon[after$lon < 0] + adjust_lon
    before$lon[before$lon < 0] <- before$lon[before$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    maps::map.axes()
    lines(cbind(basis$lon[basis$eqfilter == 1], basis$lat[basis$eqfilter == 1]), col = "grey", lwd = 0.5)
    points(cbind(basis$lon[basis$eqfilter == 1], basis$lat[basis$eqfilter == 1]), cex = 1, pch = 20, col = "darkgrey", lwd = 1)
    points(cbind(after$lon[after$eqfilter == 1], after$lat[after$eqfilter == 1]), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(before$lon[before$eqfilter == 1], before$lat[before$eqfilter == 1]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    mtext("Twilight	removal", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos removed: ", filtering$removed_twilight_cleanup, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)


    # before and after twilight edit
    if (is.null(sun_angle_start)) {
      sun_angle_start <- as.numeric(-3.5)
    }
    if (is.na(sun_angle_start)) {
      sun <- as.numeric(-3.5)
    }
    if (!is.na(sun_angle_start)) {
      sun <- as.numeric(sun_angle_start)
    }
    if (is.null(sun_angle_end)) {
      sun_angle_end <- as.numeric(sun_angle_start)
    }
    if (!is.na(sun_angle_end)) {
      sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(before_changes$tFirst)))
    }
    latlon <- GeoLight::coord(before_changes$tFirst, before_changes$tSecond, before_changes$type, degElevation = sun, note = F)
    basis <- cbind(before_changes, latlon)

    if (is.null(sun_angle_start)) {
      sun_angle_start <- as.numeric(-3.5)
    }
    if (is.na(sun_angle_start)) {
      sun <- as.numeric(-3.5)
    }
    if (!is.na(sun_angle_start)) {
      sun <- as.numeric(sun_angle_start)
    }
    if (is.null(sun_angle_end)) {
      sun_angle_end <- as.numeric(sun_angle_start)
    }
    if (!is.na(sun_angle_end)) {
      sun <- as.numeric(seq(sun_angle_start, sun_angle_end, length.out = length(after_changes$tFirst)))
    }
    latlon <- GeoLight::coord(after_changes$tFirst, after_changes$tSecond, after_changes$type, degElevation = sun, note = F)
    after <- cbind(after_changes, latlon)

    start_aut_eq <- min(posdata$tFirst[month(posdata$tFirst) %in% c(8, 9, 10, 11) & posdata$eqfilter == 0])
    end_aut_eq <- max(posdata$tFirst[month(posdata$tFirst) %in% c(8, 9, 10, 11) & posdata$eqfilter == 0])
    start_spr_eq <- min(posdata$tFirst[month(posdata$tFirst) %in% c(2, 3, 4) & posdata$eqfilter == 0])
    end_spr_eq <- max(posdata$tFirst[month(posdata$tFirst) %in% c(2, 3, 4) & posdata$eqfilter == 0])

    basis$eqfilter <- 1
    basis$eqfilter[basis$tFirst >= start_aut_eq & basis$tFirst <= end_aut_eq] <- 0
    basis$eqfilter[basis$tFirst >= start_spr_eq & basis$tFirst <= end_spr_eq] <- 0

    after$eqfilter <- 1
    after$eqfilter[after$tFirst >= start_aut_eq & after$tFirst <= end_aut_eq] <- 0
    after$eqfilter[after$tFirst >= start_spr_eq & after$tFirst <= end_spr_eq] <- 0

    before <- basis[!(basis$tFirst %in% after$tFirst), ]
    after <- after[!(after$tFirst %in% basis$tFirst), ]

    basis$lon[basis$lon < 0] <- basis$lon[basis$lon < 0] + adjust_lon
    after$lon[after$lon < 0] <- after$lon[after$lon < 0] + adjust_lon
    before$lon[before$lon < 0] <- before$lon[before$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    maps::map.axes()
    lines(cbind(basis$lon[basis$eqfilter == 1], basis$lat[basis$eqfilter == 1]), col = "grey", lwd = 0.5)
    points(cbind(basis$lon[basis$eqfilter == 1], basis$lat[basis$eqfilter == 1]), cex = 1, pch = 20, col = "darkgrey", lwd = 1)
    points(cbind(after$lon[after$eqfilter == 1], after$lat[after$eqfilter == 1]), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(before$lon[before$eqfilter == 1], before$lat[before$eqfilter == 1]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    mtext("Edit twilights	", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos changed: ", filtering$moved_twilights, ", n pos removed: ", filtering$daylengthfilter + filtering$noonfilter, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)


    # before and after speed filter
    before_speed$lon[before_speed$lon < 0] <- before_speed$lon[before_speed$lon < 0] + adjust_lon
    after_speed$lon[after_speed$lon < 0] <- after_speed$lon[after_speed$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    map.axes()
    lines(cbind(before_speed$lon, before_speed$lat), col = "grey", lwd = 0.5)
    points(cbind(after_speed$lon, after_speed$lat), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(before_speed$lon[!(before_speed$lon %in% after_speed$lon)], before_speed$lat[!(before_speed$lat %in% after_speed$lat)]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    mtext("Speed filter", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos removed: ", filtering$removed_speed, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)

    # before and after b_box filter
    before_b_box$lon[before_b_box$lon < 0] <- before_b_box$lon[before_b_box$lon < 0] + adjust_lon
    after_b_box$lon[after_b_box$lon < 0] <- after_b_box$lon[after_b_box$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    map.axes()
    lines(cbind(before_b_box$lon, before_b_box$lat), col = "grey", lwd = 0.5)
    points(cbind(after_b_box$lon, after_b_box$lat), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(before_b_box$lon[!(before_b_box$lon %in% after_b_box$lon)], before_b_box$lat[!(before_b_box$lat %in% after_b_box$lat)]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    lines(rbind(c(xlim[1], ylim[1]), c(xlim[2], ylim[1]), c(xlim[2], ylim[2]), c(xlim[1], ylim[2]), c(xlim[1], ylim[1])), col = "orange")
    mtext("Distribution filter", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos removed: ", filtering$removed_boundbox, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)


    # before and after angle filter
    before_angle$lon[before_angle$lon < 0] <- before_angle$lon[before_angle$lon < 0] + adjust_lon
    after_angle$lon[after_angle$lon < 0] <- after_angle$lon[after_angle$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    map.axes()
    lines(cbind(before_angle$lon, before_angle$lat), col = "grey", lwd = 0.5)
    points(cbind(after_angle$lon, after_angle$lat), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(before_angle$lon[!(before_angle$lon %in% after_angle$lon)], before_angle$lat[!(before_angle$lat %in% after_angle$lat)]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    mtext("Angle and distance filter", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos removed: ", filtering$removed_argos, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)

    # before and after loess filter
    before_loess$lon[before_loess$lon < 0] <- before_loess$lon[before_loess$lon < 0] + adjust_lon
    after_loess$lon[after_loess$lon < 0] <- after_loess$lon[after_loess$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    map.axes()
    lines(cbind(before_loess$lon, before_loess$lat), col = "grey", lwd = 0.5)
    points(cbind(after_loess$lon, after_loess$lat), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(before_loess$lon[!(before_loess$lon %in% after_loess$lon)], before_loess$lat[!(before_loess$lat %in% after_loess$lat)]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    mtext("Loess filter", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos removed: ", filtering$removed_loess, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)


    # after midnight sun removal
    postab1$lon[postab1$lon < 0] <- postab1$lon[postab1$lon < 0] + adjust_lon
    after_loess$lon[after_loess$lon < 0] <- after_loess$lon[after_loess$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    map.axes()
    lines(cbind(postab1$lon[postab1$eqfilter == 1], postab1$lat[postab1$eqfilter == 1]), col = "grey", lwd = 0.5)
    points(cbind(postab1$lon[postab1$eqfilter == 1], postab1$lat[postab1$eqfilter == 1]), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    points(cbind(after_loess$lon[!(after_loess$lon %in% postab1$lon)], after_loess$lat[!(after_loess$lat %in% postab1$lat)]), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    mtext("Midnight sun filter", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos removed: ", filtering$removed_midnight_sun, sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)

    # after smoothing
    pos_plot <- posdata
    pos_plot$lon_unsmooth[pos_plot$lon_unsmooth < 0] <- pos_plot$lon_unsmooth[pos_plot$lon_unsmooth < 0] + adjust_lon
    pos_plot$lon[pos_plot$lon < 0] <- pos_plot$lon[pos_plot$lon < 0] + adjust_lon

    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")
    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    map.axes()
    lines(cbind(pos_plot$lon_unsmooth[pos_plot$eqfilter == 1], pos_plot$lat_unsmooth[pos_plot$eqfilter == 1]), col = "black", lwd = 1)
    lines(cbind(pos_plot$lon[pos_plot$eqfilter == 1], pos_plot$lat[pos_plot$eqfilter == 1]), cex = 1.3, pch = 20, col = "firebrick", lwd = 1)
    mtext("Smoothed positions", side = 3, line = 1.3, cex = 0.7)
    mtext(paste("n pos: ", nrow(posdata), sep = ""), side = 3, line = 0.3, cex = 0.6, padj = 0)
  }
  if (show_filter_plots == TRUE & !is.null(save_filter_plots.dir)) {
    dev.off()
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calibration,  lat vs time plots---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~


  if (show_sun_plot %in% TRUE) {
    sun.angles <- c(-1.5, -2, -2.5, -3, -3.5, -4, -4.5, -5, -5.5, -6, -6.5)

    collat <- col_lat

    # center map to the Pacific or the Atlantic?
    map_ver <- "world"
    adjust_lon <- 0
    if ((nrow(posdata[month(posdata$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4) & abs(posdata$lon) > 90, ]) / nrow(posdata[month(posdata$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4), ])) > 0.5) adjust_lon <- 360
    if (adjust_lon == 360) map_ver <- "world2"
    pos_plot <- posdata

    pos_plot$lon[pos_plot$lon < 0] <- pos_plot$lon[pos_plot$lon < 0] + adjust_lon
    pos_plot$col_lon[pos_plot$col_lon < 0] <- pos_plot$col_lon[pos_plot$col_lon < 0] + adjust_lon

    if (show_sun_plot == TRUE & exists("save_sun_plots.dir")) {
      tiff(
        filename = paste(save_sun_plots.dir, logger_yeartracked, "lat vs time", ".tiff", sep = ""), height = 20, width = 18, units = "cm",
        compression = "lzw", res = 600
      )
    }

    latmax <- max(pos_plot$lat[pos_plot$eqfilter == 1]) + 5
    latmin <- min(pos_plot$lat[pos_plot$eqfilter == 1]) - 5
    if (latmin %in% Inf) latmin <- boundary.box[3]
    if (latmax %in% -Inf) latmax <- boundary.box[4]

    lonmax <- max(pos_plot$lon) + 5
    lonmin <- min(pos_plot$lon) - 5
    if (lonmin %in% Inf) lonmin <- boundary.box[1] + (adjust_lon / 2)
    if (lonmax %in% -Inf) lonmax <- boundary.box[2] + (adjust_lon / 2)


    par(mfrow = c(4, 3))
    par(mar = c(1.5, 2, 1.5, 1.5) + 0.1)
    i <- 1
    for (i in 1:(length(sun.angles))) {
      latlon.sun <- GeoLight::coord(pos_plot$tFirst, pos_plot$tSecond, pos_plot$type, degElevation = sun.angles[i], note = F)
      latlon.sun <- data.frame(cbind(pos_plot[, 1:3], latlon.sun))

      latlon.sun$eqfilter <- NA
      latlon.sun$eqfilter[latlon.sun$tFirst %in% pos_plot$tFirst] <- pos_plot$eqfilter[latlon.sun$tFirst %in% pos_plot$tFirst]

      plot(latlon.sun$tFirst, latlon.sun[, 5], col = "white", ylim = c(latmin, latmax), ylab = "lat", xlab = NULL, main = paste("sun", sun.angles[i], sep = ""))
      abline(v = latlon.sun$tFirst[latlon.sun$eqfilter == 0], col = "light grey")
      # lines(latlon.sun$tFirst,latlon.sun[,5])
      latlon.sun$lat_roll4 <- NA
      latlon.sun$lat_roll4[4:(nrow(latlon.sun) - 4)] <- zoo::rollmean(latlon.sun[, 5], 8)
      lines(latlon.sun$tFirst, latlon.sun$lat_roll4, lwd = 1.2)
      abline(h = collat, lty = 2)
    }

    plot(0, 0, ylim = c(25, 85), xlim = c(2, 10), axes = FALSE)
    text(6, 70, logger_id, cex = 1.5)
    text(6, 55, paste(logger_model, year_tracked, sep = " "), cex = 1.25)
    text(6, 45, colony, cex = 1.25)
    text(6, 35, species, cex = 1.25)
    if (show_sun_plot == TRUE & exists("save_sun_plots.dir")) {
      dev.off()
    }


    if (show_sun_plot == TRUE & exists("save_sun_plots.dir") == FALSE) {
      dev.new()
    }
    if (show_sun_plot == TRUE & exists("save_sun_plots.dir")) {
      tiff(
        filename = paste(save_sun_plots.dir, logger_yeartracked, "sun maps ", "aut_", ".tiff", sep = ""), height = 30, width = 25.5, units = "cm",
        compression = "lzw", res = 700
      )
    }



    par(mfrow = c(4, 3))
    par(mar = c(2, 3, 2, 2) + 0.1)
    par(oma = c(2, 2, 2, 2))
    t <- 1
    for (t in 1:(length(sun.angles))) {
      for_calib <- posdata[1:4]
      latlon.sun <- double_smoothing(df = for_calib, sun = sun.angles[t])
      latlon.sun <- na.omit(latlon.sun)
      latlon.sun$lon_smooth2[latlon.sun$lon_smooth2 < 0] <- latlon.sun$lon_smooth2[latlon.sun$lon_smooth2 < 0] + adjust_lon
      plot(NA, xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = paste("sun", sun.angles[t]))
      maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), main = paste("sun", sun.angles[t], sep = ""), add = T)
      map.axes()
      lines(latlon.sun$lon_smooth2, latlon.sun$lat_smooth2, lwd = 0.8, col = "grey")
      lines(latlon.sun$lon_smooth2[latlon.sun$eqfilter == 1], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1], lwd = 1, col = "red")
      points(latlon.sun$lon_smooth2, latlon.sun$lat_smooth2, cex = 0.4, pch = 16, col = "black")
      points(latlon.sun$lon_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(8, 9, 10)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(8, 9, 10)], cex = 0.6, pch = 16, col = "steelblue")
      points(latlon.sun$lon_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(11, 12, 1)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(11, 12, 1)], cex = 0.6, pch = 16, col = "firebrick")
      points(latlon.sun$lon_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(2, 3, 4)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(2, 3, 4)], cex = 0.6, pch = 16, col = "orange")
      points(latlon.sun$lon_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(5, 6, 7)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(5, 6, 7)], cex = 0.6, pch = 16, col = "forestgreen")
      if (col_lon < 0) points(col_lon + adjust_lon, col_lat, cex = 2, pch = 19, col = "blue")
      if (col_lon > 0) points(col_lon, col_lat, cex = 2, pch = 19, col = "blue")
    }

    plot(0, 0, ylim = c(25, 85), xlim = c(2, 10), axes = FALSE)
    text(6, 48, logger_id, cex = 1.5)
    text(6, 40, paste(logger_model, year_tracked, sep = " "), cex = 1.25)
    text(6, 35, colony, cex = 1.25)
    text(6, 30, species, cex = 1.25)
    legend("top",
      title = "Time of the year",
      c("Equinox", "Aug to Oct", "Nov to Jan", "Feb to Apr", "May to Jul"), fill = c("black", "steelblue", "firebrick", "orange", "forestgreen"), horiz = FALSE, cex = 1.1
    )

    if (show_sun_plot == TRUE & exists("save_sun_plots.dir")) {
      dev.off()
    }
  }


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add summer positions---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (add_summer_pos == TRUE) {
    tryCatch(
      {
        ## this function:
        # 1: estimates twilights based on a threshold of 30 (BAS, Biotrack, Lotek) or 300 (Migrate Technology)
        # 2: clean up data based on a intitial sun angle of 0
        # 3: finds sun angle that corresponds to "winter track"
        #     -compares latitudes calculated with various sun angles to the same dates in the  "winter track" to find the sun angle that makes the tracks overlap the most
        # 4: runs again with the most corresponding sun angle
        # 5: add missing dates to te "winter" track at each side of summer (months of 4,5,6,7,8 if breeding in N hemisphere, months of 10,11,12,1,2 if breeding in the S hemisphere)

        add_summer_locations <- summer_locations(
          df = posdata,
          luxfile = luxfile, # specify directory + filename with light data
          speed = speed, # flight speed (km/h) #previously tried half speed (speed/2)
          boundary.box = boundary.box, # distribution limits (c(lonW,lonE,latS,latN)) PS! lons as 0-360 deg if crossing the 180 meridian (otherwise -180 to 180)
          loess_filter_k = loess_filter_k, # n interquartile ranges
          months_breeding = months_breeding, # exp. months with regular presence at breeding location
          species = species,
          midnightsun_removal = midnightsun_removal, # helps removing false positions while breeding under the midnight sun
          split_years = split_years, # end of the first year tracked, start of the next
          year_tracked = year_tracked, # e.g. summer 2015 to summer 2016: "2015_16"
          logger_id = logger_id,
          date_deployed = date_deployed,
          date_retrieved = date_retrieved,
          man_equinox_periods = man_equinox_periods, # SEATRACK: latitudes considered unreliable from 20 Feb to 3 Apr, 8 Sep to 20 Oct.
          aut_equinox_periods = aut_equinox_periods # in development; base start and end date on est. lat. before and after eq. events
        )
        posdata <- rbind(posdata, add_summer_locations)
        posdata <- posdata[order(posdata$date_time), ]
      },
      error = function(e) {}
    )

    tryCatch(
      {
        utvalg <- posdata
        utvalg$distance <- NA
        no_na <- utvalg[!is.na(utvalg$lat), ]
        no_na$distance[1:(length(no_na$lat) - 1)] <- acos(sin(no_na$lat[1:(length(no_na$lat) - 1)] / 180 * pi) * sin(no_na$lat[2:length(no_na$lat)] / 180 * pi) + cos(no_na$lat[1:(length(no_na$lat) - 1)] / 180 * pi) * cos(no_na$lat[2:length(no_na$lat)] / 180 * pi) * cos(no_na$lon[1:(length(no_na$lat) - 1)] / 180 * pi - no_na$lon[2:length(no_na$lat)] / 180 * pi)) * 6371
        no_na$distance[length(no_na$lat)] <- no_na$distance[length(no_na$lat) - 1]
        no_na$time_diff <- NA
        no_na$time_diff[1:(length(no_na$lat) - 1)] <- abs(difftime(no_na$date_time[1:(length(no_na$lat) - 1)], no_na$date_time[2:length(no_na$lat)], units = "hours"))
        no_na$time_diff[length(no_na$lat)] <- no_na$time_diff[length(no_na$lat) - 1]
        travel_distance <- no_na$distance / no_na$time_diff
        utvalg$distance[!is.na(utvalg$lat)] <- travel_distance
        utvalg$keep <- TRUE
        utvalg$keep[utvalg$eqfilter == 1 & utvalg$distance > speed] <- FALSE
        utvalg$keep[utvalg$light_threshold < 29] <- TRUE
        utvalg <- utvalg[utvalg$keep == TRUE, ]
        utvalg$keep <- NULL
        utvalg$distance <- NULL
        posdata <- utvalg
      },
      error = function(e) {}
    )

    tryCatch(
      {
        tab3 <- posdata


        # insert colony as the two first and two last coordinates (removed afterwards)
        tab3 <- rbind(tab3[1, ], tab3)
        tab3$date_time[1] <- tab3$date_time[1] - 43200
        tab3$lon[1] <- col_lon
        tab3$lat[1] <- col_lat
        if (tab3$type[2] %in% 1) tab3$type[1] <- 2
        if (tab3$type[2] %in% 2) tab3$type[1] <- 1
        tab3 <- rbind(tab3[1, ], tab3)
        tab3$date_time[1] <- tab3$date_time[1] - 43200
        tab3$lon[1] <- col_lon
        tab3$lat[1] <- col_lat
        if (tab3$type[2] %in% 1) tab3$type[1] <- 2
        if (tab3$type[2] %in% 2) tab3$type[1] <- 1
        tab3 <- rbind(tab3[1, ], tab3)
        tab3$date_time[1] <- tab3$date_time[1] - 43200
        tab3$lon[1] <- col_lon
        tab3$lat[1] <- col_lat
        if (tab3$type[2] %in% 1) tab3$type[1] <- 2
        if (tab3$type[2] %in% 2) tab3$type[1] <- 1
        tab3 <- rbind(tab3[1, ], tab3)
        tab3$date_time[1] <- tab3$date_time[1] - 43200
        tab3$lon[1] <- col_lon
        tab3$lat[1] <- col_lat
        if (tab3$type[2] %in% 1) tab3$type[1] <- 2
        if (tab3$type[2] %in% 2) tab3$type[1] <- 1
        tab3 <- rbind(tab3, tab3[nrow(tab3), ])
        tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
        tab3$lon[nrow(tab3)] <- col_lon
        tab3$lat[nrow(tab3)] <- col_lat
        if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
        if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
        tab3 <- rbind(tab3, tab3[nrow(tab3), ])
        tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
        tab3$lon[nrow(tab3)] <- col_lon
        tab3$lat[nrow(tab3)] <- col_lat
        if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
        if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
        tab3 <- rbind(tab3, tab3[nrow(tab3), ])
        tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
        tab3$lon[nrow(tab3)] <- col_lon
        tab3$lat[nrow(tab3)] <- col_lat
        if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
        if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
        tab3 <- rbind(tab3, tab3[nrow(tab3), ])
        tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
        tab3$lon[nrow(tab3)] <- col_lon
        tab3$lat[nrow(tab3)] <- col_lat
        if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
        if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1



        distlim1 <- (((speed * 2) * 12) / 2) * 1000
        distlim2 <- (((speed * 2) * 12) * 1000)

        tab3.1 <- tab3[tab3$type %in% 1, ]
        tab3.2 <- tab3[tab3$type %in% 2, ]
        tab3.1$argosfilter1 <- sdafilter(tab3.1$lat, tab3.1$lon, tab3.1$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
        tab3.2$argosfilter1 <- sdafilter(tab3.2$lat, tab3.2$lon, tab3.2$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
        tab3 <- rbind(tab3.1, tab3.2)
        tab3 <- tab3[order(tab3$date_time), ]

        distlim1 <- ((speed * 12) / 2) * 1000
        distlim2 <- ((speed * 12) * 1000)

        tab3$argosfilter2 <- sdafilter(tab3$lat, tab3$lon, tab3$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))

        # remove colony coord
        tab3 <- tab3[5:(nrow(tab3) - 4), ]

        tab4 <- NULL
        tab4 <- tab3[tab3$argosfilter1 %in% c("not", "end_location") & tab3$eqfilter == 1, ]
        tab4 <- tab4[tab4$argosfilter2 %in% c("not", "end_location") & tab4$eqfilter == 1, ]
        tab5 <- tab3[tab3$eqfilter == 0, ]

        tab6 <- rbind(tab4, tab5)
        posdata <- tab6[order(tab6$date_time), ]



        posdata$argosfilter1 <- NULL
        posdata$argosfilter2 <- NULL
      },
      error = function(e) {}
    )
  }

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Add winter positions---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  if (length(posdata$lat[month(posdata$date_time) %in% c(11, 12, 1) & posdata$lat > 68]) > 4 | length(posdata$lat[month(posdata$date_time) %in% c(5, 6, 7) & posdata$lat < (-68)]) > 4) {
    if (add_winter_pos == TRUE) {
      tryCatch(
        {
          ## this function:
          # 1: estimates twilights based on a threshold of 1 (BAS, Biotrack, Lotek) or 1 (Migrate Technology)
          # 2: clean up data based on a intitial sun angle of -5
          # 3: finds sun angle that corresponds to "track"
          #     -compares latitudes calculated with various sun angles to the same dates in the  "track" to find the sun angle that makes the tracks overlap the most
          # 4: runs again with the most corresponding sun angle
          # 5: add missing dates to the "summer" track at each side of winter (months of 10,11,12,1,2 if breeding in N hemisphere, months of 4,5,6,7,8 if breeding in the S hemisphere)

          add_winter_locations <- winter_locations(
            df = posdata,
            luxfile = luxfile, # specify directory + filename with light data
            speed = speed, # flight speed (km/h) #previously tried half speed (speed/2)
            boundary.box = boundary.box, # distribution limits (c(lonW,lonE,latS,latN)) PS! lons as 0-360 deg if crossing the 180 meridian (otherwise -180 to 180)
            loess_filter_k = loess_filter_k, # n interquartile ranges
            months_breeding = months_breeding, # exp. months with regular presence at breeding location
            species = species,
            midnightsun_removal = midnightsun_removal, # helps removing false positions while breeding under the midnight sun
            split_years = split_years, # end of the first year tracked, start of the next
            year_tracked = year_tracked, # e.g. summer 2015 to summer 2016: "2015_16"
            logger_id = logger_id,
            date_deployed = date_deployed,
            date_retrieved = date_retrieved,
            man_equinox_periods = man_equinox_periods, # SEATRACK: latitudes considered unreliable from 20 Feb to 3 Apr, 8 Sep to 20 Oct.
            aut_equinox_periods = aut_equinox_periods # in development; base start and end date on est. lat. before and after eq. events
          )
          posdata <- rbind(posdata, add_winter_locations)
          posdata <- posdata[order(posdata$date_time), ]
        },
        error = function(e) {}
      )

      tryCatch(
        {
          utvalg <- posdata
          utvalg$distance <- NA
          no_na <- utvalg[!is.na(utvalg$lat), ]
          no_na$distance[1:(length(no_na$lat) - 1)] <- acos(sin(no_na$lat[1:(length(no_na$lat) - 1)] / 180 * pi) * sin(no_na$lat[2:length(no_na$lat)] / 180 * pi) + cos(no_na$lat[1:(length(no_na$lat) - 1)] / 180 * pi) * cos(no_na$lat[2:length(no_na$lat)] / 180 * pi) * cos(no_na$lon[1:(length(no_na$lat) - 1)] / 180 * pi - no_na$lon[2:length(no_na$lat)] / 180 * pi)) * 6371
          no_na$distance[length(no_na$lat)] <- no_na$distance[length(no_na$lat) - 1]
          no_na$time_diff <- NA
          no_na$time_diff[1:(length(no_na$lat) - 1)] <- abs(difftime(no_na$date_time[1:(length(no_na$lat) - 1)], no_na$date_time[2:length(no_na$lat)], units = "hours"))
          no_na$time_diff[length(no_na$lat)] <- no_na$time_diff[length(no_na$lat) - 1]
          travel_distance <- no_na$distance / no_na$time_diff
          utvalg$distance[!is.na(utvalg$lat)] <- travel_distance
          utvalg$keep <- TRUE
          utvalg$keep[utvalg$eqfilter == 1 & utvalg$distance > speed] <- FALSE
          utvalg$keep[utvalg$light_threshold < 29] <- TRUE
          utvalg <- utvalg[utvalg$keep == TRUE, ]
          utvalg$keep <- NULL
          utvalg$distance <- NULL
          posdata <- utvalg
        },
        error = function(e) {}
      )

      tryCatch(
        {
          tab3 <- posdata


          # insert colony as the two first and two last coordinates (removed afterwards)
          tab3 <- rbind(tab3[1, ], tab3)
          tab3$date_time[1] <- tab3$date_time[1] - 43200
          tab3$lon[1] <- col_lon
          tab3$lat[1] <- col_lat
          if (tab3$type[2] %in% 1) tab3$type[1] <- 2
          if (tab3$type[2] %in% 2) tab3$type[1] <- 1
          tab3 <- rbind(tab3[1, ], tab3)
          tab3$date_time[1] <- tab3$date_time[1] - 43200
          tab3$lon[1] <- col_lon
          tab3$lat[1] <- col_lat
          if (tab3$type[2] %in% 1) tab3$type[1] <- 2
          if (tab3$type[2] %in% 2) tab3$type[1] <- 1
          tab3 <- rbind(tab3[1, ], tab3)
          tab3$date_time[1] <- tab3$date_time[1] - 43200
          tab3$lon[1] <- col_lon
          tab3$lat[1] <- col_lat
          if (tab3$type[2] %in% 1) tab3$type[1] <- 2
          if (tab3$type[2] %in% 2) tab3$type[1] <- 1
          tab3 <- rbind(tab3[1, ], tab3)
          tab3$date_time[1] <- tab3$date_time[1] - 43200
          tab3$lon[1] <- col_lon
          tab3$lat[1] <- col_lat
          if (tab3$type[2] %in% 1) tab3$type[1] <- 2
          if (tab3$type[2] %in% 2) tab3$type[1] <- 1
          tab3 <- rbind(tab3, tab3[nrow(tab3), ])
          tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
          tab3$lon[nrow(tab3)] <- col_lon
          tab3$lat[nrow(tab3)] <- col_lat
          if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
          if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
          tab3 <- rbind(tab3, tab3[nrow(tab3), ])
          tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
          tab3$lon[nrow(tab3)] <- col_lon
          tab3$lat[nrow(tab3)] <- col_lat
          if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
          if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
          tab3 <- rbind(tab3, tab3[nrow(tab3), ])
          tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
          tab3$lon[nrow(tab3)] <- col_lon
          tab3$lat[nrow(tab3)] <- col_lat
          if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
          if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1
          tab3 <- rbind(tab3, tab3[nrow(tab3), ])
          tab3$date_time[nrow(tab3)] <- tab3$date_time[nrow(tab3)] + 43200
          tab3$lon[nrow(tab3)] <- col_lon
          tab3$lat[nrow(tab3)] <- col_lat
          if (tab3$type[nrow(tab3) - 1] %in% 1) tab3$type[nrow(tab3)] <- 2
          if (tab3$type[nrow(tab3) - 1] %in% 2) tab3$type[nrow(tab3)] <- 1



          distlim1 <- (((speed * 2) * 12) / 2) * 1000
          distlim2 <- (((speed * 2) * 12) * 1000)

          tab3.1 <- tab3[tab3$type %in% 1, ]
          tab3.2 <- tab3[tab3$type %in% 2, ]
          tab3.1$argosfilter1 <- sdafilter(tab3.1$lat, tab3.1$lon, tab3.1$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
          tab3.2$argosfilter1 <- sdafilter(tab3.2$lat, tab3.2$lon, tab3.2$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
          tab3 <- rbind(tab3.1, tab3.2)
          tab3 <- tab3[order(tab3$date_time), ]

          distlim1 <- ((speed * 12) / 2) * 1000
          distlim2 <- ((speed * 12) * 1000)

          tab3$argosfilter2 <- sdafilter(tab3$lat, tab3$lon, tab3$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))

          # remove colony coord
          tab3 <- tab3[5:(nrow(tab3) - 4), ]

          tab4 <- NULL
          tab4 <- tab3[tab3$argosfilter1 %in% c("not", "end_location") & tab3$eqfilter == 1, ]
          tab4 <- tab4[tab4$argosfilter2 %in% c("not", "end_location") & tab4$eqfilter == 1, ]
          tab5 <- tab3[tab3$eqfilter == 0, ]

          tab6 <- rbind(tab4, tab5)
          posdata <- tab6[order(tab6$date_time), ]



          posdata$argosfilter1 <- NULL
          posdata$argosfilter2 <- NULL
        },
        error = function(e) {}
      )
    }
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Landmask---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  land_mask <- land_mask(lon = posdata$lon, lat = posdata$lat, coast_to_land = coast_to_land, coast_to_sea = coast_to_sea, eqfilter = posdata$eqfilter)
  posdata <- posdata[land_mask == FALSE, ]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Order and save data---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  posdata$tfirst <- posdata$tFirst
  posdata$tsecond <- posdata$tSecond
  posdata$tFirst <- NULL
  posdata$tSecond <- NULL
  posdata$twl_type <- posdata$type
  posdata$type <- NULL
  posdata$age_deployed <- age_deployed

  posdata$tfirst <- as_datetime(posdata$tfirst)
  posdata$tsecond <- as_datetime(posdata$tsecond)
  posdata$date_time <- as_datetime(posdata$date_time)

  # Re-order columns
  col_order <- c(
    "date_time", "logger_id", "logger_model", "year_tracked",
    "year_deployed", "year_retrieved", "ring_number", "species",
    "age_deployed", "colony", "col_lon", "col_lat", "lon_unsmooth",
    "lat_unsmooth", "lon", "lat", "eqfilter", "tfirst", "tsecond",
    "twl_type", "sun", "light_threshold", "script_version"
  )


  posdata_reordered <- posdata[, col_order]
  filtered_rows <- filtering

  list.tables <- list(posdata_reordered, filtered_rows, twilights_with_no_positional_filtering)
  names(list.tables) <- c("tracks", "number of datapoints removed or retained", "cleaned twilights with no positional filtering")

  options(warn = oldw)
  return(list.tables)
}
