#' Reformat to SEATRACK database format (outdated per Oct, 2025)
#'
#'
#' @param df seatrackGLS dataset
#' @return dataset in database format
#' @export
seatrack_database_format <- function(df) {
    all_tracks <- df

    posdata_template <- as.data.frame(all_tracks$date_time)
    colnames(posdata_template) <- c("date_time")
    posdata_template$date_time <- as_datetime(posdata_template$date_time)
    posdata_template$logger <- paste(all_tracks$logger_id, "_", all_tracks$logger_model, sep = "")
    posdata_template$logger_id <- all_tracks$logger_id
    posdata_template$logger_model <- all_tracks$logger_model
    posdata_template$year_tracked <- all_tracks$year_tracked
    posdata_template$year_deployed <- all_tracks$year_deployed
    posdata_template$year_retrieved <- all_tracks$year_retrieved
    posdata_template$ring_number <- all_tracks$ring_number
    posdata_template$euring_code <- NA
    posdata_template$species <- all_tracks$species
    posdata_template$colony <- all_tracks$colony
    posdata_template$lon_raw <- all_tracks$lon_unsmooth
    posdata_template$lat_raw <- all_tracks$lat_unsmooth
    posdata_template$lon_smooth1 <- NA
    posdata_template$lat_smooth1 <- NA
    posdata_template$lon_smooth2 <- all_tracks$lon
    posdata_template$lat_smooth2 <- all_tracks$lat
    posdata_template$disttocol_s2 <- NA
    posdata_template$eqfilter1 <- NA
    posdata_template$eqfilter2 <- NA
    posdata_template$eqfilter3 <- all_tracks$eqfilter
    posdata_template$lat_smooth2_eqfilt3 <- NA
    posdata_template$sex <- NA
    posdata_template$morph <- NA
    posdata_template$subspecies <- NA
    posdata_template$age <- all_tracks$age_deployed
    posdata_template$col_lon <- all_tracks$col_lon
    posdata_template$col_lat <- all_tracks$col_lat
    posdata_template$tfirst <- all_tracks$tfirst
    posdata_template$tsecond <- all_tracks$tsecond
    posdata_template$twl_type <- all_tracks$twl_type
    posdata_template$conf <- NA
    posdata_template$sun <- all_tracks$sun
    posdata_template$software <- all_tracks$script_version
    posdata_template$light_threshold <- as.integer(all_tracks$light_threshold)
    posdata_template$analyzer <- all_tracks$analyzer
    posdata_template$data_responsible <- all_tracks$data_responsible
    posdata_template$logger_yeartracked <- paste(all_tracks$logger_id, "_", all_tracks$year_tracked, sep = "")
    posdata_template$posdata_file <- all_tracks$posdata_file
    posdata_template$automated_ver <- "seatrackGLSv3"

    output <- posdata_template
    return(output)
}
