#' Equinox Filter for Position Data
#'
#' Applies an equinox filter to position data based on calibrated light data.
#' @param posdata A data frame containing position data with columns `date_time`.
#' @param posdata_lat A numeric vector of latitudes corresponding to the position data.
#' @param light_data_calibration A list containing light data calibration parameters, including equinox start and end dates.
#' @param logger_colony_info A list containing logger colony information, including `col_lat`.
#' @return A data frame with an additional column `eqfilter` indicating equinox effect presence (TRUE = no equinox effect, FALSE = equinox effect).
#' @concept filtering
#' @export
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
