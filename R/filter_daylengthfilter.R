#' Remove twilights that form subsequent outliers in length of day or night.
#'
#' This function performs the following steps:
#' \enumerate{
#'   \item Separate rows of day (type=1)/night(type=2), and calculate length of each day or night
#'   \item Fill short time gaps with baytrends::fillMissing().
#'   \item Fill large time gaps assuming polar night or midnight sun: Add 23.5 hrs day/0.5hrs night (midnight sun) or 0.5 hrs day/23.5 hrs night (polar night) for dates without twilights, assuming summer or winter solstice if otherwise quite long (>16hrs) or quite short day (<8hrs) in that period of the year.
#'   \item Calculate different length in day/night between consecutive dates.
#'   \item Initial filtering: Endpoints are filtered by comparing day/night lengths to a 20-day mean, to accommodate the often poor data quality due to shading from nest attendance at start and end of logging periods
#'   \item Main filtering: daily day/night lengths compared to loess predictions of day/night length based on 5 day running means.
#'   \item Twilights that results in lengths being more than 6hrs, but less than 48 hour different from the previous date removed.
#'   \item At last, repeat all above steps, but now filter twilights that results in lengths being more than 3hrs, but less than 48 hour different from the previous date removed.
#' }
#'
#' @param df Input data with twilight times in GMT shown as 'tfirst' and 'tsecond' (yyyy-mm-dd hh:mm:ss UTC) and 'type', with being 1 (tfirst is sunrise) or 2 (tfirst is sunset).
#' @param show_plot if TRUE, plot is produced with day/night length and affected twilights highlighted
#' @return A filtered data.frame in the same format.
#' @concept filtering
#' @export
daylengthfilter <- function(df, show_plot) {
  before_changes <- df
  test <- df[df$type == 1, ]
  test$daylength <- difftime(test$tSecond, test$tFirst, units = "hours")
  test$date <- as.POSIXlt.Date(as.Date(test$tFirst))

  # include all dates from date of year tracked to last date of year tracked:
  df <- as.data.frame(seq.POSIXt(min(test$date), max(test$date), by = "day"))
  colnames(df) <- c("date")
  df$tFirst <- NA
  df$tSecond <- NA
  df$type <- NA
  df$daylength <- NA
  col_order <- c("tFirst", "tSecond", "type", "daylength", "date")
  df <- df[, col_order]
  df$date <- as.Date(df$date)

  full_test <- rbind(df, test)
  full_test <- arrange(full_test, date)
  dates_to_clean <- full_test$date[duplicated(full_test$date)]
  full_test <- full_test[full_test$date %in% dates_to_clean & !is.na(full_test$tFirst) | !(full_test$date %in% dates_to_clean), ]
  full_test$tFirst <- as_datetime(full_test$tFirst)
  full_test$tSecond <- as_datetime(full_test$tSecond)

  # taking periods of polar night or midnight sun into account.
  # short time gaps are filled with fillMissing and median values.
  # large time gaps are filled assuming polar night or midnight sun:
  # Add 24 hrs daylength (midnight sun) or 0 hrs daylength (polar night) for dates without twilights in periods close to summer or winter solstice and if otherwise quite long (>16hrs) or short daylength (<8hrs) in that period of the year:

  # short (2days)
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmedian(full_test$daylength[!is.na(full_test$daylength)], k = 5, fill = TRUE)
  full_test$mediandaylength <- baytrends::fillMissing(full_test$mediandaylength, span = 1, max.fill = 2)
  full_test$daylength[is.na(full_test$daylength)] <- full_test$mediandaylength[is.na(full_test$daylength)]
  full_test$mediandaylength <- NULL

  # long
  if (TRUE %in% (c(5, 6, 7, 8, 9) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8, 9)]) > 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8, 9)] <- 23.5
    }
  }
  if (TRUE %in% (c(5, 6, 7, 8) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8)]) <= 7) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8)] <- 0.5
    }
  }
  if (TRUE %in% (c(10, 11, 12, 1, 2) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)]) > 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)] <- 23.5
    }
  }
  if (TRUE %in% (c(11, 12, 1, 2) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1, 2)]) <= 7) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1, 2)] <- 0.5
    }
  }

  # build a dataset to make a predicted daylength by filtering the originals after also adding periods of midnight sun and polar night:
  full_test$var <- 0
  full_test$var[1:(nrow(full_test) - 1)] <- abs(full_test$daylength[1:(nrow(full_test) - 1)] - full_test$daylength[2:nrow(full_test)])
  full_test$daylength[full_test$var > 6] <- NA # if 1: animal cant move north or south further than 832 km per 12 hours = 69.33km/h
  full_test$daylength[full_test$daylength > 23.5] <- NA
  full_test$daylength[full_test$daylength < 0.5] <- NA

  median_start20d <- mean(full_test$daylength[!is.na(full_test$daylength)][1:20]) # switched to mean from median
  median_end20d <- mean(full_test$daylength[!is.na(full_test$daylength)][(length(full_test$daylength[!is.na(full_test$daylength)]) - 20):length(full_test$daylength[!is.na(full_test$daylength)])]) # switched to mean from median
  full_test$adjust_startandend <- full_test$daylength
  full_test$adjust_startandend[!is.na(full_test$daylength)][1:20] <- median_start20d
  full_test$adjust_startandend[!is.na(full_test$daylength)][(nrow(full_test[!is.na(full_test$daylength), ]) - 20):nrow(full_test[!is.na(full_test$daylength), ])] <- median_end20d
  full_test$difference <- abs(full_test$adjust_startandend - full_test$daylength)
  full_test$daylength[full_test$difference > 6] <- NA
  full_test$difference <- NULL
  full_test$adjust_startandend <- NULL

  # build loess predictions based on 5 day running means:
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmean(full_test$daylength[!is.na(full_test$daylength)], k = 5, fill = NA)
  full_test$predict <- NA
  full_test$predict[!is.na(full_test$mediandaylength)] <- predict(loess(as.numeric(full_test$mediandaylength[!is.na(full_test$mediandaylength)]) ~ as.numeric(full_test$date[!is.na(full_test$mediandaylength)]), span = 0.1))
  full_test$predict[1] <- full_test$predict[!is.na(full_test$predict)][1]
  full_test$predict[nrow(full_test)] <- full_test$predict[!is.na(full_test$predict)][length(full_test$predict[!is.na(full_test$predict)])]
  full_test$predict <- baytrends::fillMissing(full_test$predict, span = 1, max.fill = 60)

  full_test$daylength_orig <- difftime(full_test$tSecond, full_test$tFirst, units = "hours")

  # mark errornous dates:
  # full_test$difference<-abs(full_test$predict-full_test$daylength_orig)

  full_test$difference <- NA
  for (c in 5:(nrow(full_test) - 4)) {
    full_test$difference[c] <- min(abs(full_test$predict[(c - 4):(c + 4)] - full_test$daylength_orig[c]))[1]
  }

  end <- nrow(full_test)
  full_test$difference[1] <- min(abs(full_test$predict[1:5] - full_test$daylength_orig[1]))[1]
  full_test$difference[2] <- min(abs(full_test$predict[1:6] - full_test$daylength_orig[2]))[1]
  full_test$difference[3] <- min(abs(full_test$predict[1:7] - full_test$daylength_orig[3]))[1]
  full_test$difference[4] <- min(abs(full_test$predict[1:8] - full_test$daylength_orig[4]))[1]
  full_test$difference[end] <- min(abs(full_test$predict[(end - 4):end] - full_test$daylength_orig[end]))[1]
  full_test$difference[end - 1] <- min(abs(full_test$predict[(end - 5):end] - full_test$daylength_orig[end - 1]))[1]
  full_test$difference[end - 2] <- min(abs(full_test$predict[(end - 6):end] - full_test$daylength_orig[end - 2]))[1]
  full_test$difference[end - 3] <- min(abs(full_test$predict[(end - 7):end] - full_test$daylength_orig[end - 3]))[1]

  full_test$remove <- FALSE
  full_test$remove[full_test$difference > 6] <- TRUE
  full_test$remove[full_test$daylength_orig > 23.9] <- TRUE
  full_test$remove[full_test$daylength_orig < 0.1] <- TRUE
  full_test$remove[full_test$difference > 48] <- FALSE
  full_test_day <- full_test

  ############################
  # Again, but now night length:
  ############################

  test <- before_changes[before_changes$type == 2, ]
  test$daylength <- difftime(test$tSecond, test$tFirst, units = "hours")
  test$date <- as.POSIXlt.Date(as.Date(test$tFirst))

  # include all dates from date of year tracked to last date of year tracked:
  df <- as.data.frame(seq.POSIXt(min(test$date), max(test$date), by = "day"))
  colnames(df) <- c("date")
  df$tFirst <- NA
  df$tSecond <- NA
  df$type <- NA
  df$daylength <- NA
  col_order <- c("tFirst", "tSecond", "type", "daylength", "date")
  df <- df[, col_order]
  df$date <- as.Date(df$date)

  full_test <- rbind(df, test)
  full_test <- arrange(full_test, date)
  dates_to_clean <- full_test$date[duplicated(full_test$date)]
  full_test <- full_test[full_test$date %in% dates_to_clean & !is.na(full_test$tFirst) | !(full_test$date %in% dates_to_clean), ]
  full_test$tFirst <- as_datetime(full_test$tFirst)
  full_test$tSecond <- as_datetime(full_test$tSecond)

  # taking periods of polar night or midnight sun into account.
  # short time gaps are filled with fillMissing and median values.
  # large time gaps are filled assuming polar night or midnight sun:
  # Add 24 hrs daylength (midnight sun) or 0 hrs daylength (polar night) for dates without twilights in periods close to summer or winter solstice and if otherwise quite long (>16hrs) or short daylength (<8hrs) in that period of the year:

  # short (2days)
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmedian(full_test$daylength[!is.na(full_test$daylength)], k = 5, fill = TRUE)
  full_test$mediandaylength <- baytrends::fillMissing(full_test$mediandaylength, span = 1, max.fill = 2)
  full_test$daylength[is.na(full_test$daylength)] <- full_test$mediandaylength[is.na(full_test$daylength)]
  full_test$mediandaylength <- NULL

  # long
  if (TRUE %in% (c(5, 6, 7, 8) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8)]) > 17) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8)] <- 23.5
    }
  }
  if (TRUE %in% (c(4, 5, 6, 7, 8) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8, 9)]) <= 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8)] <- 0.5
    }
  }
  if (TRUE %in% (c(11, 12, 1) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1, 2)]) > 17) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1)] <- 23.5
    }
  }
  if (TRUE %in% (c(10, 11, 12, 1, 2) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)]) <= 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)] <- 0.5
    }
  }


  # build a dataset to make a predicted daylength by filtering the originals after also adding periods of midnight sun and polar night:
  full_test$var <- 0
  full_test$var[1:(nrow(full_test) - 1)] <- abs(full_test$daylength[1:(nrow(full_test) - 1)] - full_test$daylength[2:nrow(full_test)])
  full_test$daylength[full_test$var > 6] <- NA # if 1: animal cant move north or south further than 832 km per 12 hours = 69.33km/h
  full_test$daylength[full_test$daylength > 23.5] <- NA
  full_test$daylength[full_test$daylength < 0.5] <- NA

  median_start20d <- mean(full_test$daylength[!is.na(full_test$daylength)][1:20]) # switched to mean from median
  median_end20d <- mean(full_test$daylength[!is.na(full_test$daylength)][(length(full_test$daylength[!is.na(full_test$daylength)]) - 20):length(full_test$daylength[!is.na(full_test$daylength)])]) # switched to mean from median
  full_test$adjust_startandend <- full_test$daylength
  full_test$adjust_startandend[!is.na(full_test$daylength)][1:20] <- median_start20d
  full_test$adjust_startandend[!is.na(full_test$daylength)][(nrow(full_test[!is.na(full_test$daylength), ]) - 20):nrow(full_test[!is.na(full_test$daylength), ])] <- median_end20d
  full_test$difference <- abs(full_test$adjust_startandend - full_test$daylength)
  full_test$daylength[full_test$difference > 6] <- NA
  full_test$difference <- NULL
  full_test$adjust_startandend <- NULL

  # build loess predictions based on 5 day running medians:
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmean(full_test$daylength[!is.na(full_test$daylength)], k = 5, fill = TRUE)
  full_test$predict <- NA
  full_test$predict[!is.na(full_test$mediandaylength)] <- predict(loess(as.numeric(full_test$mediandaylength[!is.na(full_test$mediandaylength)]) ~ as.numeric(full_test$date[!is.na(full_test$mediandaylength)]), span = 0.1))
  full_test$predict[1] <- full_test$predict[!is.na(full_test$predict)][1]
  full_test$predict[nrow(full_test)] <- full_test$predict[!is.na(full_test$predict)][length(full_test$predict[!is.na(full_test$predict)])]
  full_test$predict <- baytrends::fillMissing(full_test$predict, span = 1, max.fill = 60)

  full_test$daylength_orig <- difftime(full_test$tSecond, full_test$tFirst, units = "hours")

  # mark errornous dates:
  # full_test$difference<-abs(full_test$predict-full_test$daylength_orig)

  full_test$difference <- NA
  for (c in 5:(nrow(full_test) - 4)) {
    full_test$difference[c] <- min(abs(full_test$predict[(c - 4):(c + 4)] - full_test$daylength_orig[c]))[1]
  }

  end <- nrow(full_test)
  full_test$difference[1] <- min(abs(full_test$predict[1:5] - full_test$daylength_orig[1]))[1]
  full_test$difference[2] <- min(abs(full_test$predict[1:6] - full_test$daylength_orig[2]))[1]
  full_test$difference[3] <- min(abs(full_test$predict[1:7] - full_test$daylength_orig[3]))[1]
  full_test$difference[4] <- min(abs(full_test$predict[1:8] - full_test$daylength_orig[4]))[1]
  full_test$difference[end] <- min(abs(full_test$predict[(end - 4):end] - full_test$daylength_orig[end]))[1]
  full_test$difference[end - 1] <- min(abs(full_test$predict[(end - 5):end] - full_test$daylength_orig[end - 1]))[1]
  full_test$difference[end - 2] <- min(abs(full_test$predict[(end - 6):end] - full_test$daylength_orig[end - 2]))[1]
  full_test$difference[end - 3] <- min(abs(full_test$predict[(end - 7):end] - full_test$daylength_orig[end - 3]))[1]

  full_test$remove <- FALSE
  full_test$remove[full_test$difference > 6] <- TRUE
  full_test$remove[full_test$daylength_orig > 23.9] <- TRUE
  full_test$remove[full_test$daylength_orig < 0.1] <- TRUE
  full_test$remove[full_test$difference > 48] <- FALSE

  ################
  df <- NULL
  df <- before_changes
  df <- df[!(df$type == 1 & as.Date(df$tFirst) %in% full_test_day$date[full_test_day$remove == TRUE]), ]
  df <- df[!(df$type == 2 & as.Date(df$tFirst) %in% full_test_day$date[full_test_day$remove == TRUE]), ]

  df <- df[!(df$type == 1 & as.Date(df$tSecond) %in% full_test$date[full_test$remove == TRUE]), ]
  df <- df[!(df$type == 2 & as.Date(df$tSecond) %in% full_test$date[full_test$remove == TRUE]), ]
  df <- df[, 1:3]
  after_changes1 <- df[, 1:3]
  #####################

  test <- after_changes1[after_changes1$type == 1, ]
  test$daylength <- difftime(test$tSecond, test$tFirst, units = "hours")
  test$date <- as.POSIXlt.Date(as.Date(test$tFirst))

  # include all dates from date of year tracked to last date of year tracked:
  df <- as.data.frame(seq.POSIXt(min(test$date), max(test$date), by = "day"))
  colnames(df) <- c("date")
  df$tFirst <- NA
  df$tSecond <- NA
  df$type <- NA
  df$daylength <- NA
  col_order <- c("tFirst", "tSecond", "type", "daylength", "date")
  df <- df[, col_order]
  df$date <- as.Date(df$date)

  full_test <- rbind(df, test)
  full_test <- arrange(full_test, date)
  dates_to_clean <- full_test$date[duplicated(full_test$date)]
  full_test <- full_test[full_test$date %in% dates_to_clean & !is.na(full_test$tFirst) | !(full_test$date %in% dates_to_clean), ]
  full_test$tFirst <- as_datetime(full_test$tFirst)
  full_test$tSecond <- as_datetime(full_test$tSecond)

  # taking periods of polar night or midnight sun into account.
  # short time gaps are filled with fillMissing and median values.
  # large time gaps are filled assuming polar night or midnight sun:
  # Add 24 hrs daylength (midnight sun) or 0 hrs daylength (polar night) for dates without twilights in periods close to summer or winter solstice and if otherwise quite long (>16hrs) or short daylength (<8hrs) in that period of the year:

  # short (3days)
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmedian(full_test$daylength[!is.na(full_test$daylength)], k = 5, fill = TRUE)
  full_test$mediandaylength <- baytrends::fillMissing(full_test$mediandaylength, span = 1, max.fill = 3)
  full_test$daylength[is.na(full_test$daylength)] <- full_test$mediandaylength[is.na(full_test$daylength)]
  full_test$mediandaylength <- NULL

  # long
  if (TRUE %in% (c(5, 6, 7, 8, 9) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8, 9)]) > 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8, 9)] <- 23.5
    }
  }
  if (TRUE %in% (c(5, 6, 7, 8) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8)]) <= 7) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8)] <- 0.5
    }
  }
  if (TRUE %in% (c(10, 11, 12, 1, 2) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)]) > 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)] <- 23.5
    }
  }
  if (TRUE %in% (c(11, 12, 1, 2) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1, 2)]) <= 7) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1, 2)] <- 0.5
    }
  }

  # build a dataset to make a predicted daylength by filtering the originals after also adding periods of midnight sun and polar night:
  full_test$var <- 0
  full_test$var[1:(nrow(full_test) - 1)] <- abs(full_test$daylength[1:(nrow(full_test) - 1)] - full_test$daylength[2:nrow(full_test)])
  full_test$daylength[full_test$var > 8] <- NA # animal cant move north or south further than 832 km per 12 hours = 69.33km/h
  full_test$daylength[full_test$daylength > 23.5] <- NA
  full_test$daylength[full_test$daylength < 0.5] <- NA

  median_start20d <- median(full_test$daylength[!is.na(full_test$daylength)][1:10])
  median_end20d <- median(full_test$daylength[!is.na(full_test$daylength)][(length(full_test$daylength[!is.na(full_test$daylength)]) - 10):length(full_test$daylength[!is.na(full_test$daylength)])])
  full_test$adjust_startandend <- full_test$daylength
  full_test$adjust_startandend[!is.na(full_test$daylength)][1:10] <- median_start20d
  full_test$adjust_startandend[!is.na(full_test$daylength)][(nrow(full_test[!is.na(full_test$daylength), ]) - 10):nrow(full_test[!is.na(full_test$daylength), ])] <- median_end20d
  full_test$difference <- abs(full_test$adjust_startandend - full_test$daylength)
  full_test$daylength[full_test$difference > 8] <- NA
  full_test$difference <- NULL
  full_test$adjust_startandend <- NULL

  # build loess predictions based on 3 day running medians:
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmedian(full_test$daylength[!is.na(full_test$daylength)], k = 3, fill = NA)
  full_test$predict <- NA
  full_test$predict[!is.na(full_test$mediandaylength)] <- predict(loess(as.numeric(full_test$mediandaylength[!is.na(full_test$mediandaylength)]) ~ as.numeric(full_test$date[!is.na(full_test$mediandaylength)]), span = 0.1))
  full_test$predict[1] <- full_test$predict[!is.na(full_test$predict)][1]
  full_test$predict[nrow(full_test)] <- full_test$predict[!is.na(full_test$predict)][length(full_test$predict[!is.na(full_test$predict)])]
  full_test$predict <- baytrends::fillMissing(full_test$predict, span = 1, max.fill = 60)

  full_test$daylength_orig <- difftime(full_test$tSecond, full_test$tFirst, units = "hours")

  # mark errornous dates:
  # full_test$difference<-abs(full_test$predict-full_test$daylength_orig)

  full_test$difference <- NA
  for (c in 5:(nrow(full_test) - 4)) {
    full_test$difference[c] <- min(abs(full_test$predict[(c - 4):(c + 4)] - full_test$daylength_orig[c]))[1]
  }

  end <- nrow(full_test)
  full_test$difference[1] <- min(abs(full_test$predict[1:5] - full_test$daylength_orig[1]))[1]
  full_test$difference[2] <- min(abs(full_test$predict[1:6] - full_test$daylength_orig[2]))[1]
  full_test$difference[3] <- min(abs(full_test$predict[1:7] - full_test$daylength_orig[3]))[1]
  full_test$difference[4] <- min(abs(full_test$predict[1:8] - full_test$daylength_orig[4]))[1]
  full_test$difference[end] <- min(abs(full_test$predict[(end - 4):end] - full_test$daylength_orig[end]))[1]
  full_test$difference[end - 1] <- min(abs(full_test$predict[(end - 5):end] - full_test$daylength_orig[end - 1]))[1]
  full_test$difference[end - 2] <- min(abs(full_test$predict[(end - 6):end] - full_test$daylength_orig[end - 2]))[1]
  full_test$difference[end - 3] <- min(abs(full_test$predict[(end - 7):end] - full_test$daylength_orig[end - 3]))[1]

  full_test$remove <- FALSE
  full_test$remove[full_test$difference > 3] <- TRUE
  full_test$remove[full_test$daylength_orig > 23.9] <- TRUE
  full_test$remove[full_test$daylength_orig < 0.1] <- TRUE
  full_test$remove[full_test$difference > 48] <- FALSE
  full_test_day <- full_test

  ############################
  # Again, but now nightlength:
  ############################

  test <- after_changes1[after_changes1$type == 2, ]
  test$daylength <- difftime(test$tSecond, test$tFirst, units = "hours")
  test$date <- as.POSIXlt.Date(as.Date(test$tFirst))

  # include all dates from date of year tracked to last date of year tracked:
  df <- as.data.frame(seq.POSIXt(min(test$date), max(test$date), by = "day"))
  colnames(df) <- c("date")
  df$tFirst <- NA
  df$tSecond <- NA
  df$type <- NA
  df$daylength <- NA
  col_order <- c("tFirst", "tSecond", "type", "daylength", "date")
  df <- df[, col_order]
  df$date <- as.Date(df$date)

  full_test <- rbind(df, test)
  full_test <- arrange(full_test, date)
  dates_to_clean <- full_test$date[duplicated(full_test$date)]
  full_test <- full_test[full_test$date %in% dates_to_clean & !is.na(full_test$tFirst) | !(full_test$date %in% dates_to_clean), ]
  full_test$tFirst <- as_datetime(full_test$tFirst)
  full_test$tSecond <- as_datetime(full_test$tSecond)

  # taking periods of polar night or midnight sun into account.
  # short time gaps are filled with fillMissing and median values.
  # large time gaps are filled assuming polar night or midnight sun:
  # Add 24 hrs daylength (midnight sun) or 0 hrs daylength (polar night) for dates without twilights in periods close to summer or winter solstice and if otherwise quite long (>16hrs) or short daylength (<8hrs) in that period of the year:

  # short (3days)
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmedian(full_test$daylength[!is.na(full_test$daylength)], k = 5, fill = NA)
  full_test$mediandaylength <- baytrends::fillMissing(full_test$mediandaylength, span = 1, max.fill = 3)
  full_test$daylength[is.na(full_test$daylength)] <- full_test$mediandaylength[is.na(full_test$daylength)]
  full_test$mediandaylength <- NULL

  # long
  if (TRUE %in% (c(5, 6, 7, 8) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8)]) > 17) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(5, 6, 7, 8)] <- 23.5
    }
  }
  if (TRUE %in% (c(4, 5, 6, 7, 8) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8, 9)]) <= 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(4, 5, 6, 7, 8)] <- 0.5
    }
  }
  if (TRUE %in% (c(11, 12, 1) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1, 2)]) > 17) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(11, 12, 1)] <- 23.5
    }
  }
  if (TRUE %in% (c(10, 11, 12, 1, 2) %in% unique(month(full_test$date[!is.na(full_test$tFirst)])))) {
    if (median(full_test$daylength[!is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)]) <= 15) {
      full_test$daylength[is.na(full_test$daylength) & month(full_test$date) %in% c(10, 11, 12, 1, 2)] <- 0.5
    }
  }


  # build a dataset to make a predicted daylength by filtering the originals after also adding periods of midnight sun and polar night:
  full_test$var <- 0
  full_test$var[1:(nrow(full_test) - 1)] <- abs(full_test$daylength[1:(nrow(full_test) - 1)] - full_test$daylength[2:nrow(full_test)])
  full_test$daylength[full_test$var > 8] <- NA # animal cant move north or south further than 832 km per 12 hours = 69.33km/h
  full_test$daylength[full_test$daylength > 23.5] <- NA
  full_test$daylength[full_test$daylength < 0.5] <- NA

  median_start20d <- median(full_test$daylength[!is.na(full_test$daylength)][1:10])
  median_end20d <- median(full_test$daylength[!is.na(full_test$daylength)][(length(full_test$daylength[!is.na(full_test$daylength)]) - 10):length(full_test$daylength[!is.na(full_test$daylength)])])
  full_test$adjust_startandend <- full_test$daylength
  full_test$adjust_startandend[!is.na(full_test$daylength)][1:10] <- median_start20d
  full_test$adjust_startandend[!is.na(full_test$daylength)][(nrow(full_test[!is.na(full_test$daylength), ]) - 10):nrow(full_test[!is.na(full_test$daylength), ])] <- median_end20d
  full_test$difference <- abs(full_test$adjust_startandend - full_test$daylength)
  full_test$daylength[full_test$difference > 8] <- NA
  full_test$difference <- NULL
  full_test$adjust_startandend <- NULL

  # build loess predictions based on 3 day running medians:
  full_test$mediandaylength <- NA
  full_test$mediandaylength[!is.na(full_test$daylength)] <- zoo::rollmedian(full_test$daylength[!is.na(full_test$daylength)], k = 3, fill = NA)
  full_test$predict <- NA
  full_test$predict[!is.na(full_test$mediandaylength)] <- predict(loess(as.numeric(full_test$mediandaylength[!is.na(full_test$mediandaylength)]) ~ as.numeric(full_test$date[!is.na(full_test$mediandaylength)]), span = 0.1))
  full_test$predict[1] <- full_test$predict[!is.na(full_test$predict)][1]
  full_test$predict[nrow(full_test)] <- full_test$predict[!is.na(full_test$predict)][length(full_test$predict[!is.na(full_test$predict)])]
  full_test$predict <- baytrends::fillMissing(full_test$predict, span = 1, max.fill = 60)

  full_test$daylength_orig <- difftime(full_test$tSecond, full_test$tFirst, units = "hours")

  # mark errornous dates:
  # full_test$difference<-abs(full_test$predict-full_test$daylength_orig)

  full_test$difference <- NA
  for (c in 5:(nrow(full_test) - 4)) {
    full_test$difference[c] <- min(abs(full_test$predict[(c - 4):(c + 4)] - full_test$daylength_orig[c]))[1]
  }

  end <- nrow(full_test)
  full_test$difference[1] <- min(abs(full_test$predict[1:5] - full_test$daylength_orig[1]))[1]
  full_test$difference[2] <- min(abs(full_test$predict[1:6] - full_test$daylength_orig[2]))[1]
  full_test$difference[3] <- min(abs(full_test$predict[1:7] - full_test$daylength_orig[3]))[1]
  full_test$difference[4] <- min(abs(full_test$predict[1:8] - full_test$daylength_orig[4]))[1]
  full_test$difference[end] <- min(abs(full_test$predict[(end - 4):end] - full_test$daylength_orig[end]))[1]
  full_test$difference[end - 1] <- min(abs(full_test$predict[(end - 5):end] - full_test$daylength_orig[end - 1]))[1]
  full_test$difference[end - 2] <- min(abs(full_test$predict[(end - 6):end] - full_test$daylength_orig[end - 2]))[1]
  full_test$difference[end - 3] <- min(abs(full_test$predict[(end - 7):end] - full_test$daylength_orig[end - 3]))[1]

  full_test$remove <- FALSE
  full_test$remove[full_test$difference > 3] <- TRUE
  full_test$remove[full_test$daylength_orig > 23.9] <- TRUE
  full_test$remove[full_test$daylength_orig < 0.1] <- TRUE
  full_test$remove[full_test$difference > 48] <- FALSE


  ################
  df <- NULL
  df <- after_changes1
  df <- df[!(df$type == 1 & as.Date(df$tFirst) %in% full_test_day$date[full_test_day$remove == TRUE]), ]
  df <- df[!(df$type == 2 & as.Date(df$tFirst) %in% full_test_day$date[full_test_day$remove == TRUE]), ]

  df <- df[!(df$type == 1 & as.Date(df$tSecond) %in% full_test$date[full_test$remove == TRUE]), ]
  df <- df[!(df$type == 2 & as.Date(df$tSecond) %in% full_test$date[full_test$remove == TRUE]), ]
  df <- df[, 1:3]
  #####################

  # plot
  if (show_plot == TRUE) {
    before_changes$hours <- as.numeric(format(before_changes[, 1], "%H")) + as.numeric(format(before_changes[, 1], "%M")) / 60
    before_changes$date <- as.Date(before_changes$tFirst)
    plot(before_changes$date, before_changes$hours, col = "firebrick", pch = 19, cex = 0.3, ylim = c(0, 26), yaxt = "n", xaxt = "n", ann = FALSE)
    mtext(side = 1, text = "Month", line = 1, cex = 0.7)
    mtext(side = 2, text = "Hours (GMT)", line = 1, cex = 0.7)

    after_changes <- df
    after_changes$hours <- as.numeric(format(after_changes[, 1], "%H")) + as.numeric(format(after_changes[, 1], "%M")) / 60
    after_changes$date <- as.Date(after_changes$tFirst)
    points(after_changes$date, after_changes$hours, col = "grey80", pch = 19, cex = 0.3)

    lines(full_test_day$date, full_test_day$daylength_orig, col = "grey", lwd = 0.7)
    lines(full_test_day$date, full_test_day$predict, col = "orange", lwd = 0.7)

    lines(full_test$date, full_test$daylength_orig, col = "lightblue", lwd = 0.7)
    lines(full_test$date, full_test$predict, col = "black", lwd = 0.7)

    daterange <- c((min(before_changes$date)), (max(before_changes$date)))
    axis.Date(1, at = seq(daterange[1], daterange[2], by = "month"), format = "%b", cex.axis = 0.6, tck = -0.02, mgp = c(3, 0, 0))
    axis(side = 2, at = c(1:24), labels = c(1:24), tck = -0.02, cex.axis = 0.6, las = 2, mgp = c(3, 0.3, 0))
    legend("top", legend = c("twl", "del. twl", "pred. day L.", "day L.", "pred. night L.", "night L."), horiz = TRUE, col = c("grey", "firebrick", "orange", "grey", "black", "lightblue"), pch = c(19, 19, NA, NA, NA, NA), lty = c(NA, NA, 1, 1, 1, 1), cex = 0.25)
  }


  output <- na.omit(df)
  return(output)
}
