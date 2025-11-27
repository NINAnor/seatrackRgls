equinox_filter <- function(posdata, posdata_lat, light_data_calibration, logger_colony_info) {
    if (is.null(light_data_calibration$spring_eq_start)) {
        posdata$eqfilter <- assign_equinox_periods(
            lats = posdata_lat,
            dates = posdata$tFirst,
            breedingloc_lat = logger_colony_info$col_lat,
            sun = get_sun_angle_seq(posdata, light_data_calibration)
        )
    } else {
        posdata$eqfilter <- TRUE
        posdata$eqfilter[posdata$date_time >= light_data_calibration$spring_eq_start & posdata$date_time <= light_data_calibration$spring_eq_end] <- FALSE
        posdata$eqfilter[posdata$date_time >= light_data_calibration$aut_eq_start & posdata$date_time <= light_data_calibration$aut_eq_end] <- FALSE
    }
    return(posdata)
}
