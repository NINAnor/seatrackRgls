#' Remove false positions during breeding or midnight sun periods
#'
#' At the head or tail of a yearly track (e.g breedingY to breedingY+1) : removes the first or last positions that are more than 25 hours apart in time from the other positions.
#' Datagaps of >25 hours in the midst of the track won't be assessed or filtered.
#'
#' @param df data.frame where  'tFirst' and 'tSecond' (both date_time) must be part of the first eight columns
#' @return A data.frame where non-consecutive positions have been removed from the head and tail of the original data.frame
#' @export
aut_midnight_sun_removal <- function(df) {
  df$conf <- 9
  tail_tmp <- df
  tail_tmp$tFirst_start3 <- tail_tmp$tFirst
  tail_tmp$tFirst_stop3 <- tail_tmp$tFirst # tail_tmp$tFirst

  for (i in 1:(as.numeric(length(tail_tmp$tSecond)))) {
    tail_tmp$tFirst_start3[i] <- tail_tmp$tFirst[i + 2]
  }
  tail_tmp$diff_positions_start <- difftime(tail_tmp$tFirst, tail_tmp$tFirst_start3, units = "hours")
  for (i in 1:(as.numeric(length(tail_tmp$tSecond)))) {
    ifelse(length(tail_tmp$tFirst[i - 2]) == 0, tail_tmp$tFirst_stop3[i] <- NA, tail_tmp$tFirst_stop3[i] <- tail_tmp$tFirst[i - 2])
  }

  tail_tmp$tFirst_stop3[1] <- NA
  tail_tmp$diff_positions_stop <- difftime(tail_tmp$tFirst, tail_tmp$tFirst_stop3, units = "hours")

  tail_tmp <- tail_tmp[!is.na(tail_tmp$diff_positions_start), ]
  tail_tmp <- tail_tmp[!is.na(tail_tmp$diff_positions_stop), ]
  tail_tmp$diff_positions_stop_minus1 <- NA

  for (i in 1:(as.numeric(length(tail_tmp$tSecond)))) {
    tail_tmp$diff_positions_start_plus1[i] <- tail_tmp$diff_positions_start[i + 1]
  }
  for (i in 1:(as.numeric(length(tail_tmp$tSecond)))) {
    ifelse(length(tail_tmp$tFirst[i - 1]) == 0, tail_tmp$diff_positions_stop_minus1[i] <- NA, tail_tmp$diff_positions_stop_minus1[i] <- tail_tmp$diff_positions_stop[i - 1])
  }

  tail_tmp <- tail_tmp[!is.na(tail_tmp$diff_positions_start_plus1), ]
  tail_tmp <- tail_tmp[!is.na(tail_tmp$diff_positions_stop_minus1), ]
  tail_tmp$conf <- 0

  for (i in 1:(as.numeric(length(tail_tmp$tSecond)))) {
    if (tail_tmp$diff_positions_start_plus1[i] >= -25 & tail_tmp$diff_positions_stop_minus1[i] <= 25) tail_tmp$conf[i] <- 9
    if (tail_tmp$diff_positions_start[i] >= -25 | tail_tmp$diff_positions_stop[i] <= 25) tail_tmp$conf[i] <- 9
  }

  tail_tmp2 <- tail_tmp[tail_tmp$conf == 9, ]
  first_date_light2 <- tail_tmp2$tFirst[1]
  last_date_light2 <- rev(tail_tmp2$tFirst)[1]

  df$conf[df$tFirst < first_date_light2] <- 0
  df$conf[df$tFirst > last_date_light2] <- 0

  df <- df[df$conf == 9, ]


  df <- df[c(1:8)]

  output <- na.omit(df)
  return(output)
}
