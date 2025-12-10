#' @return a dataframe where the first two columns indicate timing of twilight events and a column type, where type == 1 indicates that tFirst is sunrise and type == 2 indicates that tFirst is sunset.
twilight_estimation <- function(light_data, light_data_calibration, show_filter_plots = FALSE) {
    # define recording interval:
    # Median difference between one light reading and the next
    light_interval <- median(difftime(light_data$dtime[2:nrow(light_data)], light_data$dtime[1:(nrow(light_data) - 1)], units = "mins"))
    light_data <- light_data[light_data$V1 == "ok", ]
    twilight_data <- NULL
    # turn light curves into twilight data
    twilight_data <- tryCatch(
        {
             twilightCalc_bugfree(
                light_data$dtime,
                light_data$lux,
                ask = FALSE,
                preSelection = TRUE,
                allTwilights = FALSE,
                LightThreshold = light_data_calibration$light_threshold,
                maxLight = light_interval
            )
        },
        error = function(e) {
            print(paste("Error in twilight estimation", e))
            return(NULL)
        }
    )

    # among small migrate tech. loggers (c65, f100) some throw an error when running twilightCalc, the code below fixes that:
    if (is.null(twilight_data) && light_data_calibration$logger_model %in% c("f100", "c65")) {
        light_data$lux <- log(lu$lux)
    }

    if (is.null(twilight_data) && light_data_calibration$logger_model %in% c("f100", "c65")) {
        print("Trying alternative twilight calculation")
        twilight_data <- twilightCalc_bugfree(light_data$dtime,
            light_data$lux,
            ask = FALSE,
            preSelection = TRUE,
            allTwilights = FALSE,
            LightThreshold = light_data_calibration$light_threshold,
            maxLight = light_interval
        )
    }


    if (show_filter_plots) {
        plot_twilight_data(
            light_data,
            twilight_data,
            light_threshold = light_data_calibration$light_threshold,
            light_intervals = light_interval
        )
    }

    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    # advance sunset (shorten twilightCalc's advance by 1 minute,
    #          to follow procedures from transEdit and IntiProc)
    # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

    twilight_data$tFirst[twilight_data$type == 2] <- twilight_data$tFirst[twilight_data$type == 2] + 60
    twilight_data$tSecond[twilight_data$type == 1] <- twilight_data$tSecond[twilight_data$type == 1] + 60

    twilight_data <- twilight_data[, c(1:3)]

    return(twilight_data)
}

plot_twilight_data <- function(light_data, twilight_data = NULL, light_threshold = NULL, light_intervals = NULL) {
    if (!is.null(twilight_data)) {
        with_preslect <- twilight_data
        with_preslect$hours <- as.numeric(format(with_preslect[, 1], "%H")) + as.numeric(format(with_preslect[, 1], "%M")) / 60
    }
    no_preselect <- twilightCalc_bugfree(light_data$dtime, light_data$lux, ask = F, preSelection = FALSE, allTwilights = FALSE, LightThreshold = light_threshold, maxLight = light_intervals)
    no_preselect$hours <- as.numeric(format(no_preselect[, 1], "%H")) + as.numeric(format(no_preselect[, 1], "%M")) / 60

    par(mfrow = c(1, 1))

    plot(no_preselect$tFirst, no_preselect$hours, col = "firebrick", ylim = c(0, 26), ann = FALSE, pch = 19, yaxt = "none", xaxt = "none", cex = 0.3)
    mtext(side = 1, text = "Month", line = 1, cex = 0.7)
    mtext(side = 2, text = "Time of day (GMT)", line = 1, cex = 0.7)
    daterange <- c(as.POSIXlt(min(no_preselect$tFirst)), as.POSIXlt(max(no_preselect$tFirst)))

    if (!is.null(twilight_data)) {
        points(with_preslect$tFirst[with_preslect$type == 2], with_preslect$hours[with_preslect$type == 2], col = "cornflowerblue", pch = 19, cex = 0.3)
        points(with_preslect$tFirst[with_preslect$type == 1], with_preslect$hours[with_preslect$type == 1], col = "lightblue", pch = 19, cex = 0.3)
    }
    axis.POSIXct(1, at = seq(daterange[1], daterange[2], by = "month"), format = "%b", cex.axis = 0.6, tck = -0.02, mgp = c(3, 0, 0))
    axis(side = 2, at = c(1:24), labels = c(1:24), tck = -0.02, cex.axis = 0.6, las = 2, mgp = c(3, 0.3, 0))
    legend("top", legend = c("sunrise", "sunset", "ignored"), horiz = TRUE, col = c("lightblue", "cornflowerblue", "firebrick"), pch = 19, cex = 0.3)
}
