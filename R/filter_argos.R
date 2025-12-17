#' Argos Filter for Position Data
#'
#' Filters position data using the Argos SDA filter, removing positions classified as "removed" or "end_location", while considering equinox periods.
#' @param posdata A data frame containing position data with columns `lat_smooth2`, `lon_smooth2`, `date_time`, `type`, and `eqfilter`.
#' @param light_data_calibration A list containing light data calibration parameters.
#' @param logger_colony_info A list containing logger colony information, including `col_lat` and `col_lon`.
#' @param logger_filter A list containing logger filter parameters, including `speed`.
#' @return A filtered data frame with positions classified as "removed" or "end_location" by the Argos SDA filter removed, except during equinox periods.
#' @concept filtering
#' @export
argos_filter <- function(posdata, light_data_calibration, logger_colony_info, logger_filter){

    half_day <- 43200

    # insert colony as the four first and four last coordinates (removed afterwards)
    start_date_time <- posdata$date_time[1]
    start_datetime_seq <- as.POSIXct(seq(start_date_time-(half_day * 4), start_date_time - half_day, length.out = 4), tz = "UTC")
    start_type <- posdata$type[1]
    start_type_seq <- rep(c(start_type, ifelse(start_type == 1, 2, 1)), 2)

    end_date_time <- posdata$date_time[length(posdata$date_time)]
    end_datetime_seq <- as.POSIXct(seq(end_date_time + half_day, end_date_time+ (half_day *4), length.out = 4), tz = "UTC")
    end_type <- posdata$type[length(posdata$type)]
    end_type_seq <- rep(c(ifelse(end_type == 1, 2, 1), end_type), 2)

    all_datetime_seq <- c(start_datetime_seq, end_datetime_seq)
    all_type_seq <- c(start_type_seq, end_type_seq)    
    
    fake_pos <- data.frame(
        lat_smooth2 = logger_colony_info$col_lat, 
        lon_smooth2 = logger_colony_info$col_lon, 
        date_time = all_datetime_seq,
        type = all_type_seq)

    missing_names <- names(posdata)[!names(posdata) %in% names(fake_pos)]
    for(missing_name in missing_names){
        fake_pos[[missing_name]] <- NA
    }

    fake_pos <- fake_pos[,names(posdata)]
    all_pos <- rbind(posdata, fake_pos)
    all_pos <- all_pos[order(all_pos$date_time),]

    all_pos$lc <- 1

    distlim1 <- (((logger_filter$speed * 2) * 12) / 2) * 1000
    distlim2 <- (((logger_filter$speed * 2) * 12) * 1000)

    all_pos$argosfilter1 <- NA
    type_1_bool <- all_pos$type == 1
    type_2_bool <- all_pos$type == 2
    all_pos$argosfilter1[type_1_bool] <- argosfilter::sdafilter(all_pos$lat_smooth2[type_1_bool], all_pos$lon_smooth2[type_1_bool], all_pos$date_time[type_1_bool], 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))
    all_pos$argosfilter1[type_2_bool] <- argosfilter::sdafilter(all_pos$lat_smooth2[type_2_bool], all_pos$lon_smooth2[type_2_bool], all_pos$date_time[type_2_bool], 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))

    all_pos$argosfilter2 <- argosfilter::sdafilter(all_pos$lat_smooth2, all_pos$lon_smooth2, all_pos$date_time, 1, vmax = 200, ang = c(15, 35), distlim = c(distlim1, distlim2))

    bad_argos <- c("removed", "end_location")

    all_pos_filtered <- all_pos[
        (!all_pos$eqfilter & !is.na(all_pos$eqfilter)) | 
        (!(all_pos$argosfilter1 %in% bad_argos)&
         !(all_pos$argosfilter2 %in% bad_argos)), ]
         
    return(all_pos_filtered)
}