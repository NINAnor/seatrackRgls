#' Estimate and Filter Summer Locations from GLS Data
#'
#' This function performs the following steps:
#' \enumerate{
#'   \item Estimates twilight times based on a threshold of 50 (for BAS, Biotrack, Lotek), or 300 units (for Migrate Technology).
#'   \item Cleans up the data using an initial sun angle of 0 degrees.
#'   \item Identifies the sun angle that best matches the main manually calibrated track by comparing tracks calculated with various sun angles to similar dates in the main manually calibrated track and selecting the most similar one.
#'   \item Recalculates the track using the most corresponding sun angle.
#'   \item Add locations to dates where data is missing in the main manually calibrated track.
#' }
#'
#' @param df Already processed and manually calibrated data (main).
#' @param luxfile Input data containing light readings.
#' @param speed Maximum expected movement rate as km/h, sustained between two locations with ~12 hours in between. Second location in a pair will be removed.
#' @param boundary.box Distribution limits in decimal degrees (c(West,East,South,North)) PS! lons as 0-360 deg if crossing the 180 meridian (otherwise -180 to 180)
#' @param loess_filter_k N interquartile ranges used to filter outliers in twilight timing using the a loess function.
#' @param months_breeding Expected months with regular presence at breeding location.
#' @param species default NA, information can be added for convenience.
#' @param midnightsun_removal If TRUE - helps removing false location estimates while breeding under the midnight sun.
#' @param split_years Define day/month where to end and start (split) tracking year. E.g. 'c("05-31","06-01")'.
#' @param year_tracked e.g. June 2015 to May 2016 = "2015_16", January 2016 to December 2016 = "2016_16".
#' @param logger_id default NA, information can be added for convenience
#' @param date_deployed deployment date set start point
#' @param date_retrieved retrieval date set end point
#' @param man_equinox_periods Set start and end day of a spring and autumn equinox period where latitudes are not reliable. Previously settings in SEATRACK:'c("02-20","04-03","09-08","10-20")'
#' @param aut_equinox_periods If TRUE - start and end point of equinox periods is informed by birds' estimated latitude before and after equinox, plus the calibrated sun angle value.
#' @return A data frame with refined summer locations added to the main manually calibrated track.
#' @export
summer_locations <- function(df, luxfile, speed, boundary.box, loess_filter_k, months_breeding, species,
                             midnightsun_removal, split_years, year_tracked, logger_id, date_deployed,
                             date_retrieved, man_equinox_periods, aut_equinox_periods) {
  winter <- df
  low_threshold_df <- df
  if (any(grepl(".lux", luxfile)) | any(grepl("c65_super", luxfile)) | any(grepl("c330", luxfile)) | any(grepl("c250", luxfile)) | any(grepl("c65", luxfile)) | any(grepl("f100", luxfile))) {
    light_threshold <- 300
  } else {
    light_threshold <- 50
  }
  sun_angle_start <- 0
  sun_angle_end <- 0
  colony <- winter$colony[1]
  col_lon <- winter$col_lon[1]
  col_lat <- winter$col_lat[1]
  species <- winter$species[1]
  ring_number <- winter$ring_number[1]
  logger_model <- winter$logger_model[1]

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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # advance sunset (shorten twilightCalc's advance by 1 minute,
  #          to follow procedures from transEdit and IntiProc)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  trn2$tFirst[trn2$type == 2] <- trn2$tFirst[trn2$type == 2] + 60
  trn2$tSecond[trn2$type == 1] <- trn2$tSecond[trn2$type == 1] + 60

  trn2 <- trn2[, c(1:3)]

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # remove twilights of same type, less than 22 hours apart within the same day
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  months_extensive_legtucking <- NA # to be removed - must first remove the option from the relevant function

  trn2 <- twilight_cleanup(df = trn2, breedingloc_lon = col_lon, breedingloc_lat = col_lat, months_breeding = months_breeding, species = species, sun_angle_start = sun_angle_start, sun_angle_end = sun_angle_end, show_plot = FALSE)

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # detect outliers and move them back to mean of their neighbors
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

  # this filter require sun angles in order to get location estimates
  temp <- trn2
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
  }
  tab2 <- move_twilights(df = trn2, speed = speed, sun = temp$sun, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # daylengthfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tab2 <- daylengthfilter(df = tab2, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # noonfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tab2 <- noonfilter(df = tab2, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CALCULATE POSITIONS---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- tab2
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
  }
  latlon <- GeoLight::coord(tab2$tFirst, tab2$tSecond, tab2$type, degElevation = temp$sun, note = F)
  postab1 <- cbind(tab2, latlon)
  temp <- NULL
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Temp smoothing x 2---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- postab1
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
  }
  posdata <- double_smoothing(df = postab1, sun = temp$sun)
  temp <- NULL
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Equinox-filter ---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  posdata$eqfilter <- 1
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Speed filtering --------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # does not affect latitudes within the equinox period
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

  postab1 <- utvalg
  postab1$distance <- NULL

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # BOUNDARY BOX---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  tab2 <- NULL
  postab1 <- NULL
  postab1 <- postab3
  postab1$lon_filter <- NULL
  postab1$lat_filter <- NULL
  if (boundary.box[2] > 180 | boundary.box[1] > 180) postab1$lon[!is.na(postab1$lon) & postab1$lon > 180] <- postab1$lon[!is.na(postab1$lon) & postab1$lon > 180] - 360
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ARGOSFILTER---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    },
    error = function(e) {}
  )


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOESS FILTER---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  postab1_loess <- postab1
  if (difftime(postab1_loess$tFirst[length(postab1_loess$tFirst)], postab1_loess$tFirst[1], units = "days") > 45) {
    loess_filter <- loessFilter(postab1_loess$tFirst, postab1_loess$tSecond, postab1_loess$type, k = loess_filter_k, plot = F)
    postab1_loess_2 <- postab1_loess[loess_filter, ]
  }
  if (difftime(postab1_loess$tFirst[length(postab1_loess$tFirst)], postab1_loess$tFirst[1], units = "days") < 46) {
    postab1_loess_2 <- postab1_loess
  }
  postab1 <- NULL
  postab1 <- postab1_loess_2[c(1:7)]
  postab1$eqfilter <- postab1_loess_2$eqfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Exclude midnight sun period at colony-----------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # logic: i+3<=25h = START, i-3 <=25h = STOP
  aut_midnightsun_removal_loop <- midnightsun_removal
  # if(col_lat<60)aut_midnightsun_removal_loop<-FALSE
  if (aut_midnightsun_removal_loop == TRUE) {
    postab1 <- aut_midnight_sun_removal(df = postab1)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # END OF FILTERING---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Smoothing x 2---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- postab1
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
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
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    posdata$sun <- as.numeric(0)
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
  posdata$script_version <- "seatrackGLSv2"
  posdata$light_threshold <- light_threshold
  posdata$conf <- NULL
  posdata$lon_smooth1 <- NULL
  posdata$lat_smooth1 <- NULL
  posdata$lon <- posdata$lon_smooth2
  posdata$lat <- posdata$lat_smooth2
  posdata$lon_smooth2 <- NULL
  posdata$lat_smooth2 <- NULL
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Calibration,  lat vs time plots---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  sun.angles <- seq(-3, 8, by = 0.25)
  winter <- winter[winter$eqfilter == 1 & !is.na(winter$lat) & winter$type == 1, ]
  compare_tracks <- as.data.frame(sun.angles)
  compare_tracks$start_of_track <- NA
  compare_tracks$end_of_track <- NA
  t <- 1
  for (t in 1:(length(sun.angles))) {
    latlon.sun <- GeoLight::coord(posdata$tFirst, posdata$tSecond, posdata$type, degElevation = sun.angles[t], note = F)
    latlon.sun <- data.frame(cbind(posdata[, 1:3], latlon.sun))
    as.Date(latlon.sun[, 1])
    latlon.sun[, 5]

    summer <- latlon.sun[latlon.sun$type == 1, ]
    winter2 <- winter[as.Date(winter$date_time) %in% as.Date(summer$tFirst), ]
    summer <- summer[as.Date(summer$tFirst) %in% as.Date(winter2$date_time), ]
    summer$delta_lat_diff <- abs(winter2$lat - summer[, 5])

    compare_tracks$start_of_track[t] <- mean(summer$delta_lat_diff[1:20])
    compare_tracks$end_of_track[t] <- mean(summer$delta_lat_diff[(nrow(summer) - 20):nrow(summer)])
  }
  sun_angle_start <- compare_tracks$sun.angle[compare_tracks$start_of_track %in% min(na.omit(compare_tracks$start_of_track))]
  sun_angle_end <- compare_tracks$sun.angle[compare_tracks$end_of_track %in% min(na.omit(compare_tracks$end_of_track))]

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

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # advance sunset (shorten twilightCalc's advance by 1 minute,
  #          to follow procedures from transEdit and IntiProc)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  trn2$tFirst[trn2$type == 2] <- trn2$tFirst[trn2$type == 2] + 60
  trn2$tSecond[trn2$type == 1] <- trn2$tSecond[trn2$type == 1] + 60
  trn2 <- trn2[, c(1:3)]
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # remove twilights of same type, less than 22 hours apart within the same day
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  months_extensive_legtucking <- NA # to be removed - must first remove the option from the relevant function
  trn2 <- twilight_cleanup(df = trn2, breedingloc_lon = col_lon, breedingloc_lat = col_lat, months_breeding = months_breeding, species = species, sun_angle_start = sun_angle_start, sun_angle_end = sun_angle_end, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # detect outliers and move them back to mean of their neighbors
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # this filter require sun angles in order to get location estimates
  temp <- trn2
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
  }
  tab2 <- move_twilights(df = trn2, speed = speed, sun = temp$sun, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # daylengthfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tab2 <- daylengthfilter(df = tab2, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # noonfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  tab2 <- noonfilter(df = tab2, show_plot = FALSE)
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # CALCULATE POSITIONS---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- tab2
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
  }
  latlon <- GeoLight::coord(tab2$tFirst, tab2$tSecond, tab2$type, degElevation = temp$sun, note = F)
  postab1 <- cbind(tab2, latlon)
  temp <- NULL
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Temp smoothing x 2---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- postab1
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
  }
  posdata <- double_smoothing(df = postab1, sun = temp$sun)
  temp <- NULL
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Equinox-filter ---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  posdata$eqfilter <- 1

  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Speed filtering --------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # does not affect latitudes within the equinox period
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

  postab1 <- utvalg
  postab1$distance <- NULL
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # BOUNDARY BOX---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
  tab2 <- NULL
  postab1 <- NULL
  postab1 <- postab3
  postab1$lon_filter <- NULL
  postab1$lat_filter <- NULL
  if (boundary.box[2] > 180 | boundary.box[1] > 180) postab1$lon[!is.na(postab1$lon) & postab1$lon > 180] <- postab1$lon[!is.na(postab1$lon) & postab1$lon > 180] - 360
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ARGOSFILTER---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
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
    },
    error = function(e) {}
  )


  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # LOESS FILTER---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  postab1_loess <- postab1
  if (difftime(postab1_loess$tFirst[length(postab1_loess$tFirst)], postab1_loess$tFirst[1], units = "days") > 45) {
    loess_filter <- loessFilter(postab1_loess$tFirst, postab1_loess$tSecond, postab1_loess$type, k = loess_filter_k, plot = F)
    postab1_loess_2 <- postab1_loess[loess_filter, ]
  }
  if (difftime(postab1_loess$tFirst[length(postab1_loess$tFirst)], postab1_loess$tFirst[1], units = "days") < 46) {
    postab1_loess_2 <- postab1_loess
  }
  postab1 <- NULL
  postab1 <- postab1_loess_2[c(1:7)]
  postab1$eqfilter <- postab1_loess_2$eqfilter
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Exclude midnight sun period at colony-----------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # logic: i+3<=25h = START, i-3 <=25h = STOP
  aut_midnightsun_removal_loop <- midnightsun_removal
  # if(col_lat<60)aut_midnightsun_removal_loop<-FALSE
  if (aut_midnightsun_removal_loop == TRUE) {
    postab1 <- aut_midnight_sun_removal(df = postab1)
  }
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # END OF FILTERING---------------------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # Smoothing x 2---------------------
  # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  temp <- postab1
  if (is.null(sun_angle_start)) {
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    temp$sun <- as.numeric(0)
  }
  if (!is.na(sun_angle_start)) {
    temp$sun <- sun_angle_start
  }
  if (is.null(sun_angle_end)) {
    sun_angle_end <- sun_angle_start
  }
  if (!is.na(sun_angle_end)) {
    temp$sun <- seq(sun_angle_start, sun_angle_end, length.out = length(temp$tFirst))
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
    sun_angle_start <- as.numeric(0)
  }
  if (is.na(sun_angle_start)) {
    posdata$sun <- as.numeric(0)
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
  posdata$script_version <- "seatrackGLSv2"
  posdata$light_threshold <- light_threshold
  posdata$conf <- NULL
  posdata$lon_smooth1 <- NULL
  posdata$lat_smooth1 <- NULL
  posdata$lon <- posdata$lon_smooth2
  posdata$lat <- posdata$lat_smooth2
  posdata$lon_smooth2 <- NULL
  posdata$lat_smooth2 <- NULL

  # if(col_lat>0){(summer_months<-c(4,5,6,7,8,9))}else{summer_months<-c(10,11,12,1,2,3)}
  # add_to_winter<-posdata[month(posdata$date_time)%in%summer_months,]
  # add_to_winter<-add_to_winter[!(as.Date(add_to_winter$date_time)%in% as.Date(winter$date_time)),]
  # add_to_winter <- add_to_winter[order(add_to_winter$date_time),]
  # output<-add_to_winter
  # return(output)

  if (col_lat > 0) {
    (summer_days <- c(98:247))
  } else {
    summer_days <- c(285:366, 1:60)
  }
  if (species %in% c("Arctic tern", "arctic tern", "Sterna paradisaea", "sterna paradisaea", "ARTE")) {
    (summer_days <- c(285:366, 1:60, 110:247))
  }
  add_to_winter <- posdata[yday(posdata$date_time) %in% summer_days, ]
  add_to_winter <- add_to_winter[!(as.Date(add_to_winter$date_time) %in% as.Date(low_threshold_df$date_time)), ]
  add_to_winter <- add_to_winter[order(add_to_winter$date_time), ]
  output <- add_to_winter
  return(output)
}
