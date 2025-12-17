#' Remove twilights that form subsequent outliers in time of noon and midnight.
#'
#' This function performs the following steps:
#' \enumerate{
#'   \item Calculate noon/midnight based on twilight.
#'   \item make noon/midnight circular.
#'   \item split data set into day and night
#'   \item loess predictions of noon/midnight (after excluding twilights with extreme variation from predictions)
#'   \item fill data gaps in predictions with baytrends::fillMissing()
#'   \item find the minimum difference between a noon/midnight directly calculated from twilights and any predicted noon/midnight 4 days ahead or after candidate noon/midnight.
#'   \item threshold for removing twilight is when difference between noon/midnight vs prediction overreach 0.4h, but under under 12h
#'   }
#'
#' @param df Input data with twilight times in GMT shown as 'tfirst' and 'tsecond' (yyyy-mm-dd hh:mm:ss UTC) and 'type', with being 1 (tfirst is sunrise) or 2 (tfirst is sunset).
#' @param show_plot if TRUE, plot is produced with timing of noon/midnight and affected twilights highlighted
#' @return A filtered data.frame in the same format.
#' @concept filtering
#' @export
noonfilter <- function(df, show_plot) {
  # to be used in later plotting:
  before_changes <- df

  # start of function:
  test <- df
  test$date <- as.Date(test$tFirst)

  # filter out outliers in twilights. Will be disregarded when making predictions
  test$loess_filter <- TRUE
  tryCatch(
    {
      test$loess_filter <- loessFilter(test$tFirst, test$tSecond, test$type, k = 3, plot = F)
    },
    error = function(e) {}
  )


  # calc noon/midnight based on twilights
  test$mid <- as_datetime(NA)
  test$mid <- test$tFirst + ((test$tSecond - test$tFirst) / 2)

  # make noon/midnight circular
  mid <- as.numeric(format(test$mid, "%H")) + as.numeric(format(test$mid, "%M")) / 60
  for (t in 1:2) {
    cor <- rep(NA, 24)
    for (i in 0:23) {
      cor[i + 1] <- max(abs((c(mid[test$type == t][1], mid[test$type == t]) + i) %% 24 - (c(mid[test$type == t], mid[test$type == t][length(mid)]) + i) %% 24), na.rm = T)
    }
    mid[test$type == t] <- (mid[test$type == t] + (which.min(round(cor, 2))) - 1) %% 24
  }
  test$mid <- mid

  ## split dataset into day and night and
  # exclude twilights from predictions based on variation in noon or midnight regardless of time gaps
  # combine again
  test$var <- 0
  day <- test[test$type == 1, ]
  night <- test[test$type == 2, ]

  day$var[1:(nrow(day) - 1)] <- abs(day$mid[1:(nrow(day) - 1)] - day$mid[2:nrow(day)])
  day$loess_filter[day$var > 1] <- FALSE

  night$var[1:(nrow(night) - 1)] <- abs(night$mid[1:(nrow(night) - 1)] - night$mid[2:nrow(night)])
  night$loess_filter[night$var > 1] <- FALSE

  day$var[2:nrow(day)] <- abs(day$mid[2:nrow(day)] - day$mid[1:(nrow(day) - 1)])
  day$loess_filter[day$var > 1] <- FALSE

  night$var[2:nrow(night)] <- abs(night$mid[2:nrow(night)] - night$mid[1:(nrow(night) - 1)])
  night$loess_filter[night$var > 1] <- FALSE

  test <- rbind(day, night)
  test <- arrange(test, date)
  test$var <- NULL

  # make predictions on noon/midnights that have not been marked by the loess filter
  test$predict <- NA
  for (d in seq(30, 1, length = 5)) {
    tryCatch(
      {
        test$predict[test$type == 1 & test$loess_filter] <- predict(loess(test$mid[test$type == 1 & test$loess_filter] ~ as.numeric(test$tFirst[test$type == 1 & test$loess_filter]), span = 0.05))
        test$predict[test$type == 2 & test$loess_filter] <- predict(loess(test$mid[test$type == 2 & test$loess_filter] ~ as.numeric(test$tFirst[test$type == 2 & test$loess_filter]), span = 0.05))
      },
      error = function(e) {}
    )
  }

  ## split dataset into day and night
  day <- test[test$type == 1, ]
  night <- test[test$type == 2, ]

  # day:
  # fill inn missing dates
  df <- as.data.frame(seq.POSIXt(min(day$tFirst), max(day$tFirst), by = "day"))
  colnames(df) <- c("date")
  df$tFirst <- NA
  df$tSecond <- NA
  df$type <- NA
  df$loess_filter <- TRUE
  df$mid <- NA
  df$predict <- NA
  col_order <- c("tFirst", "tSecond", "type", "date", "loess_filter", "mid", "predict")
  df <- df[, col_order]
  df$date <- as.Date(df$date)


  full_test <- rbind(df, day)
  full_test <- arrange(full_test, date)
  dates_to_clean <- full_test$date[duplicated(full_test$date)]
  full_test <- full_test[full_test$date %in% dates_to_clean & !is.na(full_test$tFirst) | !(full_test$date %in% dates_to_clean), ]
  full_test$tFirst <- as_datetime(full_test$tFirst)
  full_test$tSecond <- as_datetime(full_test$tSecond)

  # fill datagaps in predictions by baytrends::fillMissing()
  full_test$predict <- baytrends::fillMissing(full_test$predict, span = 1, max.fill = 90)
  if (all(is.na(full_test$predict))) {
    print("No predictions returned. Cannot apply noon filter.")
    return(before_changes)
  }

  # fill missing predictions at the end and beginning of the year_tracked, respectively:
  full_test$predict[1:(nrow(full_test) / 2)] <- zoo::na.locf(full_test$predict[1:(nrow(full_test) / 2)], fromLast = TRUE)
  full_test$predict <- zoo::na.locf(full_test$predict)

  day <- full_test

  # find the difference between a noon directly calculated from twilights
  # and predicted noons 4 days ahead or after respective noon:
  day$difference <- NA
  for (c in 5:(nrow(day) - 4)) {
    day$difference[c] <- min(abs(day$predict[(c - 4):(c + 4)] - day$mid[c]))[1]
  }

  end <- nrow(day)
  day$difference[1] <- min(abs(day$predict[1:5] - day$mid[1]))[1]
  day$difference[2] <- min(abs(day$predict[1:6] - day$mid[2]))[1]
  day$difference[3] <- min(abs(day$predict[1:7] - day$mid[3]))[1]
  day$difference[4] <- min(abs(day$predict[1:8] - day$mid[4]))[1]
  day$difference[end] <- min(abs(day$predict[(end - 4):end] - day$mid[end]))[1]
  day$difference[end - 1] <- min(abs(day$predict[(end - 5):end] - day$mid[end - 1]))[1]
  day$difference[end - 2] <- min(abs(day$predict[(end - 6):end] - day$mid[end - 2]))[1]
  day$difference[end - 3] <- min(abs(day$predict[(end - 7):end] - day$mid[end - 3]))[1]


  # night:
  # fill inn missing dates
  df <- as.data.frame(seq.POSIXt(min(night$tFirst), max(night$tFirst), by = "day"))
  colnames(df) <- c("date")
  df$tFirst <- NA
  df$tSecond <- NA
  df$type <- NA
  df$loess_filter <- TRUE
  df$mid <- NA
  df$predict <- NA
  col_order <- c("tFirst", "tSecond", "type", "date", "loess_filter", "mid", "predict")
  df <- df[, col_order]
  df$date <- as.Date(df$date)


  full_test <- rbind(df, night)
  full_test <- arrange(full_test, date)
  dates_to_clean <- full_test$date[duplicated(full_test$date)]
  full_test <- full_test[full_test$date %in% dates_to_clean & !is.na(full_test$tFirst) | !(full_test$date %in% dates_to_clean), ]
  full_test$tFirst <- as_datetime(full_test$tFirst)
  full_test$tSecond <- as_datetime(full_test$tSecond)

  # fill datagaps in predictions by baytrends::fillMissing()
  full_test$predict <- baytrends::fillMissing(full_test$predict, span = 1, max.fill = 90)
  if (all(is.na(full_test$predict))) {
    print("No predictions returned. Cannot apply noon filter.")
    return(before_changes)
  }

  # fill missing predictions at the end and beginning of the year_tracked, respectivly:
  full_test$predict[1:(nrow(full_test) / 2)] <- zoo::na.locf(full_test$predict[1:(nrow(full_test) / 2)], fromLast = TRUE)
  full_test$predict <- zoo::na.locf(full_test$predict)
  night <- full_test

  # find the difference between a midnight directly calculated from twilights
  # and predicted midnight 4 days ahead or after respective midnight:
  night$difference <- NA
  for (c in 5:(nrow(night) - 4)) {
    night$difference[c] <- min(abs(night$predict[(c - 4):(c + 4)] - night$mid[c]))[1]
  }

  end <- nrow(night)
  night$difference[1] <- min(abs(night$predict[1:5] - night$mid[1]))[1]
  night$difference[2] <- min(abs(night$predict[1:6] - night$mid[2]))[1]
  night$difference[3] <- min(abs(night$predict[1:7] - night$mid[3]))[1]
  night$difference[4] <- min(abs(night$predict[1:8] - night$mid[4]))[1]
  night$difference[end] <- min(abs(night$predict[(end - 4):end] - night$mid[end]))[1]
  night$difference[end - 1] <- min(abs(night$predict[(end - 5):end] - night$mid[end - 1]))[1]
  night$difference[end - 2] <- min(abs(night$predict[(end - 6):end] - night$mid[end - 2]))[1]
  night$difference[end - 3] <- min(abs(night$predict[(end - 7):end] - night$mid[end - 3]))[1]

  # combine datasets for day and night again
  test <- rbind(day, night)
  test <- arrange(test, date)

  ## threshold for removing twilight is when difference between noon/midnight vs prediction overreach 0.4h, but under under 12h
  test$remove <- FALSE
  test$remove[test$difference > 0.4 & test$difference < 11.9] <- TRUE


  if (show_plot == TRUE) {
    base <- before_changes
    base$hours <- as.numeric(format(base[, 1], "%H")) + as.numeric(format(base[, 1], "%M")) / 60
    result <- test
    result$hours <- as.numeric(format(result[, 1], "%H")) + as.numeric(format(result[, 1], "%M")) / 60

    line3 <- test
    line3$remove1 <- as.numeric(format(line3$tFirst, "%H")) + as.numeric(format(line3$tFirst, "%M")) / 60
    line3$remove2 <- as.numeric(format(line3$tSecond, "%H")) + as.numeric(format(line3$tSecond, "%M")) / 60

    plot(base$tFirst, base$hours, col = "white", cex = 0.3, pch = 19, ylim = c(0, 26), yaxt = "n", xaxt = "n", ann = FALSE)
    points(result$tFirst[result$type == 1], result$hours[result$type == 1], col = "grey", cex = 0.3, pch = 19)
    points(result$tFirst[result$type == 2], result$hours[result$type == 2], col = "grey", cex = 0.3, pch = 19)
    mtext(side = 1, text = "Month", line = 1, cex = 0.7)
    mtext(side = 2, text = "Time of day (GMT)", line = 1, cex = 0.7)

    lines(line3$tFirst[line3$type == 1 & !is.na(line3$mid)], line3$mid[line3$type == 1 & !is.na(line3$mid)], type = "l", col = "grey", lwd = 0.7, lty = 1)
    lines(line3$tFirst[line3$type == 2 & !is.na(line3$mid)], line3$mid[line3$type == 2 & !is.na(line3$mid)], type = "l", col = "lightblue", lwd = 0.7, lty = 1)

    points(line3$tFirst[line3$remove], line3$remove1[line3$remove], col = "firebrick", pch = 19, cex = 0.3)
    points(line3$tSecond[line3$remove], line3$remove2[line3$remove], col = "firebrick", pch = 19, cex = 0.3)

    lines(line3$tFirst[line3$type == 1 & !is.na(line3$mid)], line3$predict[line3$type == 1 & !is.na(line3$mid)], type = "l", col = "orange", lwd = 0.7)
    lines(line3$tSecond[line3$type == 2 & !is.na(line3$mid)], line3$predict[line3$type == 2 & !is.na(line3$mid)], type = "l", col = "black", lwd = 0.7)
    # lines(line3$predict2[!is.na(line3$midday_predict)],line3$midday_predict[!is.na(line3$midday_predict)]+2, type = "l",col="orange",lwd=0.7,lty=2)
    # lines(line3$predict2[!is.na(line3$midday_predict)],line3$midday_predict[!is.na(line3$midday_predict)]-2, type = "l",col="orange",lwd=0.7,lty=2)

    daterange <- c(as.POSIXlt(min(base$tFirst)), as.POSIXlt(max(base$tFirst)))
    axis.POSIXct(1, at = seq(daterange[1], daterange[2], by = "month"), format = "%b", cex.axis = 0.6, tck = -0.02, mgp = c(3, 0, 0))
    axis(side = 2, at = c(1:24), labels = c(1:24), tck = -0.02, cex.axis = 0.6, las = 2, mgp = c(3, 0.3, 0))
    legend("top",
      legend = c("ok", "del. twl", "noon", "pred. noon", "midnight", "pred.midnight"), horiz = TRUE,
      col = c("grey", "firebrick", "grey", "orange", "lightblue", "black"),
      pch = c(19, 19, NA, NA, NA, NA), lty = c(NA, NA, 1, 1, 1, 1), cex = 0.25
    )
  }


  test <- test[test$remove == FALSE, ]
  test <- test[!is.na(test$type), ]
  df <- test[, 1:3]

  output <- na.omit(df)
  return(output)
}
