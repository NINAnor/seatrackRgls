speed_filter <- function(posdata, speed) {
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

    utvalg <- posdata
    # speed filtering outside of equinox
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
    utvalg$keep[utvalg$eqfilter & utvalg$distance > utvalg$speed] <- FALSE
    utvalg$speed <- NULL
    utvalg <- utvalg[utvalg$keep == TRUE, ]
    utvalg$keep <- NULL

    test <- NULL

    # extra filtering of longitudes during equinox:

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
    test$filter[test$eqfilter] <- 1
    test$filter[test$lon_smooth2 > 170] <- 1
    test$filter[test$lon_smooth2 < -170] <- 1
    utvalg <- utvalg[test$filter == 1, ]
    utvalg$distance <- NULL # Distance is misleading

    return(utvalg)
}
