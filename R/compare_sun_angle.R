compare_sun_angle <- function(prev_posdata_export, new_posdata_export, type) {
    main_data <- prev_posdata_export[prev_posdata_export$eqfilter == 1 & !is.na(prev_posdata_export$lat) & prev_posdata_export$type == 1, ]
    sun_angle_seq <- seatrackRgls::sun_angles[[type]]
    compare_tracks <- data.frame(sun.angle = sun_angle_seq)
    compare_tracks$start_of_track <- NA
    compare_tracks$end_of_track <- NA
    for (t in 1:(length(sun_angle_seq))) {
        latlon.sun <- GeoLight::coord(
            new_posdata_export$tFirst,
            new_posdata_export$tSecond,
            new_posdata_export$type,
            degElevation = sun_angle_seq[t], note = FALSE
        )
        latlon.sun <- data.frame(cbind(new_posdata_export[, c("tFirst", "tSecond", "type")], latlon.sun))

        season <- latlon.sun[latlon.sun$type == 1, ]
        main_season <- main_data[as.Date(main_data$date_time) %in% as.Date(season$tFirst), ]
        season <- season[as.Date(season$tFirst) %in% as.Date(main_season$date_time), ]
        season$delta_lat_diff <- abs(main_season$lat - season$lat)

        compare_tracks$start_of_track[t] <- mean(season$delta_lat_diff[1:20])
        compare_tracks$end_of_track[t] <- mean(season$delta_lat_diff[(nrow(season) - 20):nrow(season)])
    }
    sun_angle_start <- compare_tracks$sun.angle[compare_tracks$start_of_track == min(compare_tracks$start_of_track, na.rm = TRUE)][1]
    sun_angle_end <- compare_tracks$sun.angle[compare_tracks$end_of_track == min(compare_tracks$end_of_track, na.rm = TRUE)][1]
    if (is.null(sun_angle_start) | is.na(sun_angle_start)) {
        stop("NULL")
    }
    return(list(sun_angle_start = sun_angle_start, sun_angle_end = sun_angle_end))
}
