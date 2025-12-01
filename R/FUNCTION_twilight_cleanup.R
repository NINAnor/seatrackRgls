#' Estimate and Refine Locations From Light Data Recorded by Geolocators.
#'
#' Filter that remove false days and nights caused by recording shading or artificial light patterns that produce errornous twilights,
#' The filter is made to identify dates with an impossible number of twilights.
#' Normally, there is one sunset and one sunrise to a date, but to not affect long-range migration,
#' the filter allow dates with two sunsets or two sunrises as long as sunsets or sunrises occur more than 22 hours apart from each other.
#' The function then predict timing of twilights to be used for selecting the most likely set of twilights within these dates.
#' It use predicted times to avoid referring to singular datapoints that very well can be outliers.
#'
#' This function performs the following steps:
#' \enumerate{
#'   \item find dates with too many twilights (1 sunset and 1 sunrise per 22 hours)
#'   \item Build predictions of time of sunset and sunrise by keeping unlikely times out of predictions with loess filter and std deviation.
#'   \item Standard deviation: calculate SD every 5th day to keep points with SD> 60mins out when making predictions
#'   \item Fill predictions for dates with no light with twilights that makes up a daylength of 0 hours.
#'   \item Retain the candidate twilight that is closest to a predicted twilight, remove the others
#' }
#'
#' @param df data.frame with 'tFirst', 'tSecond', 'type','sun'.
#' @param breedingloc_lon longitude where bird was instrumented informs the algorithm
#' @param breedingloc_lat latitude where bird was instrumented informs the algorithm
#' @param months_breeding Expected months with regular presence at breeding location
#' @param species default NA, information can be added for convenience
#' @param show_plot TRUE/FALSE default NA, information can be added for convenience
#' @param sun_angle_start if NA, default is -3.5. Analyser to add manually calibrated value after an initial run of the function
#' @param sun_angle_end filled if compensating for a change in light sensitivity (default is NA)
#' @return A data frame with raw and smoothed locations, twilight time, threshold and sun angle used to estimate locations, as well as some convenient info about logger and individual.
#' @export
twilight_cleanup <- function(df, breedingloc_lon, breedingloc_lat, months_breeding, species, show_plot, sun_angle_start, sun_angle_end, show_filter_plots = FALSE) {
  # datetime_conversion
  df$time <- strftime(df$tFirst, format = "%H:%M:%S")
  df$time_mins <- as.numeric(difftime(as.POSIXct(df$time, format = "%H:%M:%S"), as.POSIXct("00:00:00", format = "%H:%M:%S"), units = "min"))

  # find dates with too many twilights
  ok <- NULL
  ok <- df
  ok$remove <- FALSE
  ok$date <- as.Date(df$tFirst, format = "%Y-%mm-%dd")
  for (i in 1:length(ok$time[ok$type == 1])) {
    tryCatch(
      {
        if (ok$date[ok$type == 1][i] == ok$date[ok$type == 1][i + 1]) {
          ok$remove[ok$type == 1][i] <- TRUE
        }
        if (ok$date[ok$type == 1][i] == ok$date[ok$type == 1][i + 1]) {
          ok$remove[ok$type == 1][i + 1] <- TRUE
        }
      },
      error = function(e) {}
    )
  }
  for (i in 1:length(ok$time[ok$type == 2])) {
    tryCatch(
      {
        if (ok$date[ok$type == 2][i] == ok$date[ok$type == 2][i + 1]) {
          ok$remove[ok$type == 2][i] <- TRUE
        }
        if (ok$date[ok$type == 2][i] == ok$date[ok$type == 2][i + 1]) {
          ok$remove[ok$type == 2][i + 1] <- TRUE
        }
      },
      error = function(e) {}
    )
  }



  # allow dates with two sunrises, if more than 22 hours apart from each other
  rise <- ok[ok$type == 1, ]
  for (i in 1:length(rise$tFirst)) {
    return_to_false <- rise[rise$date == rise$date[i], ]
    if (rise$remove[i] == TRUE & nrow(return_to_false) == 2 && (difftime(return_to_false$tFirst[2], return_to_false$tFirst[1], units = "hours")) > 22) {
      rise$remove[rise$date == rise$date[i]] <- FALSE
    }
  }

  # allow dates with two sunsets, if more than 22 hours apart from each other
  set <- ok[ok$type == 2, ]
  for (i in 1:length(set$tFirst)) {
    return_to_false <- set[set$date == set$date[i], ]
    if (set$remove[i] == TRUE & nrow(return_to_false) == 2 && (difftime(return_to_false$tFirst[2], return_to_false$tFirst[1], units = "hours")) > 22) {
      set$remove[set$date == set$date[i]] <- FALSE
    }
  }



  ###########################
  together <- NULL
  together <- as.data.frame(rbind(set, rise))
  together$DTime <- as.POSIXct(together$tFirst, format = "%d/%m/%y %H:%M", tz = "GMT")
  ok <- NULL
  ok <- together[order(together$DTime, decreasing = FALSE), ]
  ok2 <- ok
  ###########################
  if (species %in% c(
    "Common eider", "common eider", "Somateria mollissima", "somateria mollissima",
    "Common murre", "Common guillemot", "Uria aalge",
    "common murre", "common guillemot", "uria aalge",
    "Thick-billed murre", "Br\u00FCnnich's guillemot", "Uria lomvia",
    "thick-billed murre", "br\u00FCnnich's guillemot", "uria lomvia",
    "Pallas' murre", "Uria lomvia arra", "pallas' murre", "uria lomvia arra",
    "Little auk", "Dovekie", "Alle alle", "little auk", "dovekie", "alle alle",
    "Razorbill", "Razor-billed auk", "Lesser auk", "Alca torda",
    "razorbill", "razor-billed auk", "lesser auk", "alca torda",
    "Xantus's murrelet", "xantus's murrelet",
    "Scripps's murrelet", "Synthliboramphus scrippsi", "scripps's murrelet", "synthliboramphus scrippsi",
    "Guadalupe murrelet", "Synthliboramphus hypoleucus", "guadalupe murrelet", "synthliboramphus hypoleucus",
    "Craveri's murrelet", "Synthliboramphus craveri", "craveri's murrelet", "synthliboramphus craveri",
    "Ancient murrelet", "ancient murrelet", "Synthliboramphus antiquus", "synthliboramphus antiquus",
    "Japanese murrelet", "japanese murrelet", "Synthliboramphus wumizusume", "synthliboramphus wumizusume",
    "Black guillemot", "tystie", "Cepphus grylle", "black guillemot", "tystie", "cepphus grylle",
    "Pigeon guillemot", "Cepphus columba", "pigeon guillemot", "cepphus columba",
    "Kurile guillemot", "Cepphus columba snowi", "kurile guillemot", "cepphus columba snowi",
    "Spectacled guillemot", "Cepphus carbo", "spectacled guillemot", "cepphus carbo",
    "Marbled murrelet", "Brachyramphus marmoratus", "brachyramphus marmoratus", "marbled murrelet",
    "Long-billed murrelet", "long-billed murrelet", "Brachyramphus perdix", "brachyramphus perdix",
    "Kittlitz's murrelet", "kittlitz's murrelet", "Brachyramphus brevirostris", "brachyramphus brevirostris",
    "Cassin's auklet", "cassin's auklet", "Ptychoramphus aleuticus", "ptychoramphus aleuticus",
    "Parakeet auklet", "parakeet auklet", "Aethia psittacula", "aethia psittacula",
    "Crested auklet", "crested auklet", "Aethia cristatella", "aethia cristatella",
    "Whiskered auklet", "whiskered auklet", "Aethia pygmaea", "aethia pygmaea",
    "Least auklet", "least auklet", "Aethia pusilla", "aethia pusilla",
    "Rhinoceros auklet", "rhinoceros auklet", "Cerorhinca monocerata", "cerorhinca monocerata",
    "Atlantic puffin", "atlantic puffin", "Fratercula arctica", "fratercula arctica",
    "Horned puffin", "horned puffin", "Fratercula corniculata", "fratercula corniculata",
    "Tufted puffin", "tufted puffin", "Fratercula cirrhata", "fratercula cirrhata",
    "Snow petrel", "snow petrel", "Pagodroma nivea", "pagodroma nivea"
  )) {
    # if diving species, allow earliest sunrise outside of defined breeding months
    rise <- ok2[ok2$type == 1, ]
    rise_breeding <- rise[month(rise$tFirst) %in% months_breeding, ]
    rise <- rise[!(month(rise$tFirst) %in% months_breeding), ]
    rise$date <- as.Date(rise$tFirst)
    for (i in 1:length(unique(rise$date))) {
      rise$remove[rise$date %in% unique(rise$date)[i] & rise$time_mins %in% min(rise$time_mins[rise$date %in% unique(rise$date)[i]])] <- FALSE
    }
    rise$date <- NULL

    # if diving species, allow latest sunset outside of defined breeding months
    set <- ok2[ok2$type == 2, ]
    set_breeding <- set[month(set$tFirst) %in% months_breeding, ]
    set <- set[!(month(set$tFirst) %in% months_breeding), ]
    set$date <- as.Date(set$tFirst)
    for (i in 1:length(unique(set$date))) {
      set$remove[set$date %in% unique(set$date)[i] & set$time_mins %in% max(set$time_mins[set$date %in% unique(set$date)[i]])] <- FALSE
    }
    set$date <- NULL

    ###########################
    set_breeding$date <- NULL
    rise_breeding$date <- NULL
    together <- NULL
    together <- as.data.frame(rbind(set, rise, set_breeding, rise_breeding))
    together$DTime <- as.POSIXct(together$tFirst, format = "%d/%m/%y %H:%M", tz = "GMT")
    ok <- NULL
    ok <- together[order(together$DTime, decreasing = FALSE), ]
    ok2 <- ok
    ###########################
  }

  ## predict time of sunsets and sunrises:
  # keep unlikely timed twilights out of the predictions with use of loess and standard deviation
  # NB! if the dataset is too short, this can create error when finding outliers with loess and will have no effect!
  # loess-filtering:
  ok2$loess_filter <- TRUE
  tryCatch(
    {
      ok2$loess_filter[ok2$remove == FALSE] <- GeoLight::loessFilter(ok2$tFirst[ok2$remove == FALSE], ok2$tSecond[ok2$remove == FALSE], ok2$type[ok2$remove == FALSE], k = 3, plot = F)
    },
    error = function(e) {}
  )

  # Standard deviation: calculate SD every 5th day to keep points with SD> 60mins out when making predictions:
  ok2$sd[ok2$type == 1] <- roll::roll_sd((hour(ok2$tFirst[ok2$type == 1]) * 60) + minute(ok2$tFirst[ok2$type == 1]), 5,
    weights = rep(1, 5), center = TRUE,
    min_obs = 1, complete_obs = FALSE, na_restore = FALSE,
    online = TRUE
  )
  ok2$sd[ok2$type == 2] <- roll::roll_sd((hour(ok2$tFirst[ok2$type == 2]) * 60) + minute(ok2$tFirst[ok2$type == 2]), 5,
    weights = rep(1, 5), center = TRUE,
    min_obs = 1, complete_obs = FALSE, na_restore = FALSE,
    online = TRUE
  )
  ok2$sd[is.na(ok2$sd)] <- 10000
  ok2$loess_filter[ok2$remove == FALSE & !is.na(ok2$sd)] <- ok2$sd[ok2$remove == FALSE & !is.na(ok2$sd)] < 60


  # predict twilights
  ok3 <- ok2

  # circular times
  hours <- as.numeric(format(ok3[, 1], "%H")) + as.numeric(format(ok3[, 1], "%M")) / 60
  for (t in 1:2) {
    cor <- rep(NA, 24)
    for (i in 0:23) {
      cor[i + 1] <- max(abs((c(hours[ok3$type == t][1], hours[ok3$type == t]) + i) %% 24 - (c(hours[ok3$type == t], hours[ok3$type == t][length(hours)]) + i) %% 24), na.rm = T)
    }
    hours[ok3$type == t] <- (hours[ok3$type == t] + (which.min(round(cor, 2))) - 1) %% 24
  }

  # NB! if the dataset is too short, this can create error in loess prediction and this filter will have no effect!
  ok3$hours <- hours
  ok3$predict <- NA
  for (d in seq(30, 1, length = 5)) {
    tryCatch(
      {
        ok3$predict[ok3$type == 1 & ok3$loess_filter & ok3$remove == FALSE] <- predict(loess(ok3$hours[ok3$type == 1 & ok3$loess_filter & ok3$remove == FALSE] ~ as.numeric(ok3$tFirst[ok3$type == 1 & ok3$loess_filter & ok3$remove == FALSE]), span = 0.05))
        ok3$predict[ok3$type == 2 & ok3$loess_filter & ok3$remove == FALSE] <- predict(loess(ok3$hours[ok3$type == 2 & ok3$loess_filter & ok3$remove == FALSE] ~ as.numeric(ok3$tFirst[ok3$type == 2 & ok3$loess_filter & ok3$remove == FALSE]), span = 0.05))
      },
      error = function(e) {}
    )
  }

  # instruct max.fill to fill by number of dates, not number of rows:
  fillMisspred1 <- ok3
  fillMisspred1 <- fillMisspred1[fillMisspred1$type %in% 1, ]
  fillMisspred1 <- fillMisspred1[!duplicated(as.Date(fillMisspred1$tFirst)), ]

  fillMisspred2 <- ok3
  fillMisspred2 <- fillMisspred2[fillMisspred2$type %in% 2, ]
  fillMisspred2 <- fillMisspred2[!duplicated(as.Date(fillMisspred2$tFirst)), ]

  fillMisspred1 <- as.data.frame(fillMisspred1 %>%
    mutate(Date = as.Date(tFirst)) %>%
    mice::complete(Date = seq.Date(min(as.Date(fillMisspred1$tFirst)), max(as.Date(fillMisspred1$tFirst)), by = "day")))
  fillMisspred2 <- as.data.frame(fillMisspred2 %>%
    mutate(Date = as.Date(tFirst)) %>%
    mice::complete(Date = seq.Date(min(as.Date(fillMisspred2$tFirst)), max(as.Date(fillMisspred2$tFirst)), by = "day")))

  # fill predictions for dates with no light with twilights that makes up a daylength of 0 hours:
  doy2 <- yday(fillMisspred2$Date[!is.na(fillMisspred2$predict)])
  doy1 <- yday(fillMisspred1$Date[!is.na(fillMisspred1$predict)])
  doy <- doy1[doy1 %in% doy2]

  # define "summer" months
  summer_months <- c(4, 5, 6, 7, 8, 9)
  if (breedingloc_lat < 0) {
    summer_months <- c(10, 11, 12, 1, 2, 3)
  }
  doy_polar_night2 <- yday(fillMisspred2$Date[is.na(fillMisspred2$tFirst) & !(month(fillMisspred2$Date) %in% summer_months)])
  doy_polar_night1 <- yday(fillMisspred1$Date[is.na(fillMisspred1$tFirst) & !(month(fillMisspred1$Date) %in% summer_months)])
  doy_polar_night <- doy_polar_night1[doy_polar_night1 %in% doy_polar_night2]



  hours2_1 <- as.numeric(format(ok3[, 1][ok3$type == 1], "%H")) + as.numeric(format(ok3[, 1][ok3$type == 1], "%M")) / 60
  hours2_2 <- as.numeric(format(ok3[, 1][ok3$type == 2], "%H")) + as.numeric(format(ok3[, 1][ok3$type == 2], "%M")) / 60
  test1 <- hours2_1 - ok3$hours[ok3$type == 1]
  test2 <- hours2_2 - ok3$hours[ok3$type == 2]

  tryCatch(
    {
      if (length(doy_polar_night) > 0) {
        i <- 1
        for (i in 1:length(doy_polar_night)) {
          select <- doy[which.min(abs(doy - doy_polar_night[i]))]
          fillMisspred2$predict[yday(fillMisspred2$Date) == select]
          pred_to_fill <- (abs(fillMisspred2$predict[yday(fillMisspred2$Date) == select] + median(test2)) + abs(fillMisspred1$predict[yday(fillMisspred1$Date) == select] + median(test1))) / 2
          fillMisspred2$predict[yday(fillMisspred2$Date) == doy_polar_night[i]] <- pred_to_fill - median(test2)
          fillMisspred1$predict[yday(fillMisspred1$Date) == doy_polar_night[i]] <- pred_to_fill - median(test1)
        }
      }
    },
    error = function(e) {}
  )

  # fill short holes in the dataset:
  fillMisspred1$predict <- baytrends::fillMissing(fillMisspred1$predict, span = 1, max.fill = 5)
  fillMisspred2$predict <- baytrends::fillMissing(fillMisspred2$predict, span = 1, max.fill = 5)


  # define "nonbreeding" months. For practicalities, add first and last summermonth of breeding to the "months_nonbreeding"
  #  months_nonbreeding<-1:12
  #  if(length(months_breeding)>0){
  #    months_nonbreeding<-months_nonbreeding[!(months_nonbreeding%in%months_breeding)]
  #    months_nonbreeding=c(months_nonbreeding,min(months_breeding),max(months_breeding))
  #  }
  # define "winter" months.

  winter_months <- c(10, 11, 12, 1, 2, 3)
  if (breedingloc_lat < 0) {
    winter_months <- c(4, 5, 6, 7, 8, 9)
  }

  if (length(months_breeding) > 0) {
    winter_months <- c(1:12)
    winter_months <- winter_months[!(winter_months %in% months_breeding)]
  }



  # fill larger holes in the dataset up to 20 days during non-breeding:
  fillMisspred1$predict[month(fillMisspred1$Date) %in% winter_months] <- baytrends::fillMissing(fillMisspred1$predict[month(fillMisspred1$Date) %in% winter_months], span = 5, max.fill = 60)
  fillMisspred2$predict[month(fillMisspred2$Date) %in% winter_months] <- baytrends::fillMissing(fillMisspred2$predict[month(fillMisspred2$Date) %in% winter_months], span = 5, max.fill = 60)

  fillMisspred1 <- fillMisspred1[!is.na(fillMisspred1$type), ]
  fillMisspred2 <- fillMisspred2[!is.na(fillMisspred2$type), ]

  # add predictions to working table
  i <- 1
  for (i in 1:length(unique(as.Date(fillMisspred1$tFirst))))
  {
    ok3$predict[ok3$type %in% 1 & as.Date(ok3$tFirst) %in% unique(as.Date(fillMisspred1$tFirst))[i]] <- fillMisspred1$predict[as.Date(fillMisspred1$tFirst) %in% unique(as.Date(fillMisspred1$tFirst))[i]]
  }
  i <- 1
  for (i in 1:length(unique(as.Date(fillMisspred2$tFirst))))
  {
    ok3$predict[ok3$type %in% 2 & as.Date(ok3$tFirst) %in% unique(as.Date(fillMisspred2$tFirst))[i]] <- fillMisspred2$predict[as.Date(fillMisspred2$tFirst) %in% unique(as.Date(fillMisspred2$tFirst))[i]]
  }

  ok3$hours2 <- as.numeric(format(ok3[, 1], "%H")) + as.numeric(format(ok3[, 1], "%M")) / 60
  ok3$test <- ok3$hours2 - ok3$hours
  ok3$date <- as.Date(ok3$tFirst, format = "%Y-%mm-%dd")

  # When breeding months are specified, find time of civil dusk and dawn for the breeding location during the year.

  if (length(months_breeding) > 0) {
    sun_data <- suncalc::getSunlightTimes(
      date = ok3$date, lat = breedingloc_lat, lon = breedingloc_lon,
      keep = c("dawn", "dusk"), tz = "GMT"
    ) # tz changed from "UTC" to "GMT"
    sun_data <- as.data.frame(sun_data)
    colnames(sun_data) <- c("date", "lat", "lon", "dawn", "dusk")

    #    hours_dusk<-as.numeric(format(sun_data$dusk, "%H")) + as.numeric(format(sun_data$dusk, "%M"))/60
    #    hours_dawn<-as.numeric(format(sun_data$dawn, "%H")) + as.numeric(format(sun_data$dawn, "%M"))/60
    # use calibrated sun angle instead of a sun angle of -6

    if (is.null(sun_angle_start)) {
      sun_angle_start <- as.numeric(-3.5)
    }
    if (is.na(sun_angle_start)) {
      sun_angle_start <- as.numeric(-3.5)
    }
    if (is.null(sun_angle_end)) {
      sun_angle_end <- sun_angle_start
    }
    if (is.na(sun_angle_end)) {
      sun_angle_end <- sun_angle_start
    }
    sun <- seq(sun_angle_start, sun_angle_end, length.out = nrow(ok3))

    b_location <- sf::st_as_sf(data.frame(x = breedingloc_lon, y = breedingloc_lat), coords = c("x", "y"), crs = "+proj=longlat +datum=WGS84")
    dawn2 <- suntools::crepuscule(b_location, ok3$tFirst, abs(sun), direction = c("dawn"), POSIXct.out = T)
    dusk2 <- suntools::crepuscule(b_location, ok3$tFirst, abs(sun), direction = c("dusk"), POSIXct.out = T)

    hours_dusk <- as.numeric(format(dusk2$time, "%H")) + as.numeric(format(dusk2$time, "%M")) / 60
    hours_dawn <- as.numeric(format(dawn2$time, "%H")) + as.numeric(format(dawn2$time, "%M")) / 60

    ok3 <- cbind(ok3, hours_dusk, hours_dawn)
    ok3$hours_dusk <- as.POSIXct(paste(as.Date(ok3$tFirst), " ", as.character(floor(ok3$hours_dusk)), ":", as.character(floor((ok3$hours_dusk - floor(ok3$hours_dusk)) * 60)), ":00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
    ok3$hours_dawn <- as.POSIXct(paste(as.Date(ok3$tFirst), " ", as.character(floor(ok3$hours_dawn)), ":", as.character(floor((ok3$hours_dawn - floor(ok3$hours_dawn)) * 60)), ":00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  }

  ok3$predict <- abs(ok3$predict + ok3$test)
  # ok3$predict[ok3$type==1]<-abs(ok3$predict[ok3$type==1]+median(ok3$test[ok3$type==1])) #updated code 29april2022 - predicted times get right more often
  # ok3$predict[ok3$type==2]<-abs(ok3$predict[ok3$type==2]+median(ok3$test[ok3$type==2])) #updated code 29april2022 - predicted times get right more often


  ok3$predict[!is.na(ok3$predict) & ok3$predict > 24] <- 24 - (ok3$predict[!is.na(ok3$predict) & ok3$predict > 24] - 24)

  ok3$predict2 <- as.POSIXct(paste(as.Date(ok3$tFirst), " ", as.character(floor(ok3$predict)), ":", as.character(floor((ok3$predict - floor(ok3$predict)) * 60)), ":00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
  ok3$fillMissing_predict <- as.POSIXct(paste(as.Date(ok3$tFirst), " ", as.character(floor(ok3$predict)), ":", as.character(floor((ok3$predict - floor(ok3$predict)) * 60)), ":00", sep = ""), format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

  # when breeding months are specified use the breeding locations' civil (sun -6) dawn and dusk as predicted twilights during the summer if predict/predict2 is NA:
  if (length(months_breeding) > 0) {
    ok3$predict2[month(ok3$date) %in% months_breeding & is.na(ok3$predict2) & ok3$type == 1] <- ok3$hours_dawn[month(ok3$date) %in% months_breeding & is.na(ok3$predict2) & ok3$type == 1]
    ok3$predict2[month(ok3$date) %in% months_breeding & is.na(ok3$predict2) & ok3$type == 2] <- ok3$hours_dusk[month(ok3$date) %in% months_breeding & is.na(ok3$predict2) & ok3$type == 2]
  }


  #################################
  #This function returns the time in 'timeVector' that is ' closest to 'time'
  closest.time <- function(timeVector, time) {
    x <- chron::times(timeVector)
    v <- chron::times(time)
    clockwise_distance <- abs(x - v)
    anticlockwise_distance <- chron::times("23:59:59") - clockwise_distance + chron::times("00:00:01")
    clockwise_and_anticlockwise <- matrix(c(anticlockwise_distance, clockwise_distance), ncol = 2)
    shortest_distance_of_the_two <- apply(clockwise_and_anticlockwise, 1, min)
    indx <- which(shortest_distance_of_the_two == min(shortest_distance_of_the_two))
    x[indx]
  }
  ################################

  ## retain the candidate twilight that is closest to a predicted twilight, remove the others

  check <- ok3[!is.na(ok3$predict2) & ok3$type == 1, ]
  # check<-ok3[ok3$remove==FALSE & ok3$type==1,]
  rise <- ok3[ok3$type == 1, ]

  i <- 1
  for (i in 1:length(rise$tFirst)) {
    keep <- NULL
    tFirst <- NULL
    # if(rise$remove[i]==TRUE){
    x <- as.Date(rise$date[i])
    cand_to_keep <- rise[rise$date == rise$date[i], ]
    cand_to_keep$time <- strftime(cand_to_keep$tFirst, format = "%H:%M:%S")

    # in months with extensive leg tucking due to molt, candidates are allowed to be compared to a reference time from other dates up to 10 days
    # (because predictions might have failed to be calculated for a period due to light disturbance, but the bird keeps to the same area all the time)

    if (nrow(check) > 0) {
      compare_to_this <- check[which(abs(check$date - x) == min(abs(check$date - x)))[1], ]
      compare_to_this$comp_predict_time <- strftime(compare_to_this$predict2, format = "%H:%M:%S")
      if (month(x) %in% NA & is.na(cand_to_keep$predict2[1]) & as.numeric(compare_to_this$date - x) < 11) {
        keep <- closest.time(cand_to_keep$time, compare_to_this$comp_predict_time)
        to_keep <- cand_to_keep[cand_to_keep$time == keep[1], ]
        if (!is.na(compare_to_this$comp_predict_time)) {
          if (rise$remove[i] == TRUE & rise$tFirst[i] == to_keep$tFirst) {
            rise$remove[i] <- FALSE
          }
        }
      }
    }
    # for all other months of the year: candidates can only be compared to times within the same date.
    compare_to_this <- cand_to_keep[1, ]
    compare_to_this$comp_predict_time <- strftime(compare_to_this$predict2, format = "%H:%M:%S")
    keep <- closest.time(cand_to_keep$time, compare_to_this$comp_predict_time)
    to_keep <- cand_to_keep[cand_to_keep$time == keep[1], ]
    if (!is.na(compare_to_this$comp_predict_time)) {
      if (rise$remove[i] == TRUE & rise$tFirst[i] == to_keep$tFirst) {
        rise$remove[i] <- FALSE
      }
    }
  }
  # }
  false_rise_removed <- rise[rise$remove == FALSE, ]
  # now only one rise remain for each day

  check <- ok3[!is.na(ok3$predict2) & ok3$type == 2, ]
  # check<-ok3[ok3$remove==FALSE & ok3$type==2,]
  set <- ok3[ok3$type == 2, ] # rise

  i <- 1
  for (i in 1:length(set$tFirst)) {
    keep <- NULL
    tFirst <- NULL
    x <- as.Date(set$date[i])
    cand_to_keep <- set[set$date == set$date[i], ]
    cand_to_keep$time <- strftime(cand_to_keep$tFirst, format = "%H:%M:%S")

    # in months with extensive leg tucking due to molt, candidates are allowed to be compared to a reference time from other dates up to 10 days
    # (because predictions might have failed to be calculated for a period due to light disturbance, but the bird keeps to the same area all the time)

    if (nrow(check) > 0) {
      compare_to_this <- check[which(abs(check$date - x) == min(abs(check$date - x)))[1], ]
      compare_to_this$comp_predict_time <- strftime(compare_to_this$predict2, format = "%H:%M:%S")
      if (month(x) %in% NA & is.na(cand_to_keep$predict2[1]) & as.numeric(compare_to_this$date - x) < 11) {
        keep <- closest.time(cand_to_keep$time, compare_to_this$comp_predict_time)
        to_keep <- cand_to_keep[cand_to_keep$time == keep[1], ]
        if (!is.na(compare_to_this$comp_predict_time)) {
          if (set$remove[i] == TRUE & set$tFirst[i] == to_keep$tFirst) {
            set$remove[i] <- FALSE
          }
        }
      }
    }
    # for all other months of the year: candidates can only be compared to times within the same date.
    compare_to_this <- cand_to_keep[1, ]
    compare_to_this$comp_predict_time <- strftime(compare_to_this$predict2, format = "%H:%M:%S")
    keep <- closest.time(cand_to_keep$time, compare_to_this$comp_predict_time)
    to_keep <- cand_to_keep[cand_to_keep$time == keep[1], ]
    if (!is.na(compare_to_this$comp_predict_time)) {
      if (set$remove[i] == TRUE & set$tFirst[i] == to_keep$tFirst) {
        set$remove[i] <- FALSE
      }
    }
  }
  false_set_removed <- set[set$remove == FALSE, ]
  # now only one set remain for each day



  ###########################
  df2 <- NULL
  df2 <- as.data.frame(rbind(false_set_removed[, c(1:3)], false_rise_removed[, c(1:3)]))
  df2 <- df2[order(df2$tFirst, decreasing = FALSE), ]
  df2$tSecond[1:(length(df2$tSecond) - 1)] <- df2$tFirst[2:length(df2$tSecond)]
  df <- NULL
  df <- df2

  ###########################

  # plot
  if (show_plot == TRUE) {
    base <- ok
    chosen <- ok3
    chosen$remove2 <- NA
    chosen$remove2[chosen$type %in% 2] <- set$remove
    chosen$remove2[chosen$type %in% 1] <- rise$remove
    chosen <- chosen[chosen$remove != chosen$remove2, ]
    chosen$hours <- as.numeric(format(chosen[, 1], "%H")) + as.numeric(format(chosen[, 1], "%M")) / 60
    base$hours <- as.numeric(format(base[, 1], "%H")) + as.numeric(format(base[, 1], "%M")) / 60
    result <- df
    result$hours <- as.numeric(format(result[, 1], "%H")) + as.numeric(format(result[, 1], "%M")) / 60
    set$hours <- as.numeric(format(set[, 1], "%H")) + as.numeric(format(set[, 1], "%M")) / 60
    rise$hours <- as.numeric(format(rise[, 1], "%H")) + as.numeric(format(rise[, 1], "%M")) / 60
    predicted_line <- ok3
    predicted_line$fillMissing_predict <- as.numeric(format(predicted_line$fillMissing_predict, "%H")) + as.numeric(format(predicted_line$fillMissing_predict, "%M")) / 60
    predicted_line$predict2 <- as.numeric(format(predicted_line$predict2, "%H")) + as.numeric(format(predicted_line$predict2, "%M")) / 60

    plot(base$tFirst, base$hours, col = "grey", cex = 0.3, pch = 19, ylim = c(0, 26), yaxt = "n", xaxt = "n", ann = FALSE)
    lines(result$tFirst[result$type == 1], result$hours[result$type == 1], col = "grey", lwd = 0.7)
    lines(result$tFirst[result$type == 2], result$hours[result$type == 2], col = "grey", lwd = 0.7)
    mtext(side = 1, text = "Month", line = 1, cex = 0.7)
    mtext(side = 2, text = "Time of day (GMT)", line = 1, cex = 0.7)
    points(rise$tFirst[rise$remove %in% TRUE], rise$hours[rise$remove %in% TRUE], col = "firebrick", pch = 19, cex = 0.3)
    points(set$tFirst[set$remove %in% TRUE], set$hours[set$remove %in% TRUE], col = "firebrick", pch = 19, cex = 0.3)
    points(chosen$tFirst, chosen$hours, col = "cornflowerblue", pch = 19, cex = 0.3)
    lines(predicted_line$tFirst[predicted_line$type == 1 & predicted_line$remove == FALSE], predicted_line$predict2[predicted_line$type == 1 & predicted_line$remove == FALSE], type = "l", col = "chartreuse3", lwd = 0.7, lty = 1, )
    lines(predicted_line$tFirst[predicted_line$type == 2 & predicted_line$remove == FALSE], predicted_line$predict2[predicted_line$type == 2 & predicted_line$remove == FALSE], type = "l", col = "chartreuse3", lwd = 0.7, lty = 1)

    lines(predicted_line$tFirst[predicted_line$type == 1 & predicted_line$remove == FALSE], predicted_line$fillMissing_predict[predicted_line$type == 1 & predicted_line$remove == FALSE], type = "l", col = "orange", lwd = 0.7, )
    lines(predicted_line$tFirst[predicted_line$type == 2 & predicted_line$remove == FALSE], predicted_line$fillMissing_predict[predicted_line$type == 2 & predicted_line$remove == FALSE], type = "l", col = "orange", lwd = 0.7)

    daterange <- c(as.POSIXlt(min(base$tFirst)), as.POSIXlt(max(base$tFirst)))
    axis.POSIXct(1, at = seq(daterange[1], daterange[2], by = "month"), format = "%b", cex.axis = 0.6, tck = -0.02, mgp = c(3, 0, 0))
    axis(side = 2, at = c(1:24), labels = c(1:24), tck = -0.02, cex.axis = 0.6, las = 2, mgp = c(3, 0.3, 0))
    legend("top",
      legend = c("ok", "del. twl", "retained twl", "pred. twl", "pred.twl (nest)"), horiz = TRUE,
      col = c("grey", "firebrick", "cornflowerblue", "orange", "chartreuse3"),
      pch = c(19, 19, 19, NA, NA), lty = c(NA, NA, NA, 1, 1), cex = 0.3
    )
  }

  output <- df
  return(output)
}
