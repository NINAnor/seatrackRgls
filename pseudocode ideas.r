library(seatrackR)
library(sf)

# CAN LIVE IN METADATA PACKAGE
prepare_seatrack_colony_info <- function() {
    all_colony_info <- getColonies()
    all_colony_info <- data.frame(colony = all_colony_info$colony_int_name, col_lat = all_colony_info$lat, col_lon = all_colony_info$lon)
    return(all_colony_info)
}

# CAN LIVE IN METADATA PACKAGE
# SHOULD APPEND SOME KIND OF breeding_start_month, breeding_end_month
# NEED TO MODIFY THIS TO ACCOMODATE BOTH EXISTING CALIBRATED DATA AND BASIC METADATA.
prepare_seatrack_calibration <- function(metadata_path, split_years = "06-01") {
    metadata <- openxlsx2::read_xlsx(metadata_path)
    # convert to cleaner dataframes
    calibration_data <- metadata[, c("logger_id", "logger_model", "species", "date_deployed", "date_retrieved", "colony", "sun_angle_start", "sun_angle_end", "light_threshold", "analyzer")]
    calibration_data <- calibration_data[!is.na(calibration_data$logger_id), ]
    # add time window start/ends
    calibration_data_list <- lapply(unique(calibration_data$logger_id), function(logger_id) {
        logger_data <- calibration_data[calibration_data$logger_id == logger_id, ]
        tz <- attr(logger_data$date_deployed, "tzone")
        if (is.null(tz)) tz <- ""
        start_datetime <- as.POSIXct(as.Date(logger_data$date_deployed[1]) + 1, tz = tz) # Assuming we ignore the actual day the bird was caught
        start_year <- as.numeric(format(start_datetime, "%Y"))
        end_datetime <- as.Date(logger_data$date_retrieved[1])
        end_year <- as.numeric(format(end_datetime, "%Y"))

        # If this track lasts more than one year
        if (start_year != end_year && (end_year - 1) != start_year) {
            # years in between
            all_years <- seq(start_year, end_year)
            missing_years <- all_years[!all_years %in% c(start_year, end_year)]
            split_dates <- paste(missing_years, split_years, sep = "-")
            split_datetimes <- as.POSIXct(as.Date(split_dates))
            all_dates <- c(start_datetime, split_datetimes, end_datetime)

            ends <- all_dates[-1] - as.difftime(1, units = "secs")

            # set final end to 00:00:00 of the final date_retrieved, because I assume the datapoint during the actual day the bird was caught is not good
            # (preserve tz if present)

            ends[length(ends)] <- as.POSIXct(as.Date(all_dates[length(all_dates)]), tz = tz)
            time_windows <- data.frame(
                start_datetime = all_dates[-length(all_dates)],
                end_datetime   = ends
            )
        } else {
            time_windows <- data.frame(
                start_datetime = start_datetime,
                end_datetime   = end_datetime
            )
        }

        new_logger_data <- data.frame(logger_data[, c("logger_id", "logger_model")], time_windows, logger_data[, c("species", "colony", "sun_angle_start", "sun_angle_end", "light_threshold", "analyzer")])

        return(new_logger_data)
    })

    new_calibration_data <- do.call(rbind, calibration_data_list)

    extra_metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "date_retrieved", "date_deployed", "logger_producer", "ring_number", "country_code", "data_responsible", "age_deployed")]

    return(list(calibration_data = new_calibration_data, extra_metadata = extra_metadata))
}


import_directory <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Logger data/Raw logger data/ALL/"
metadata_path <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Positions_seatrackGLS/Scripts/metadata.xlsx"

all_colony_info <- prepare_seatrack_colony_info()
result <- prepare_seatrack_calibration(metadata_path)
# calibration_data <- result$calibration_data
extra_metadata <- result$extra_metadata

# Minimum columns in filter_list
# logger_id, logger_model, species, colony
# if start_datetime is not provided, deployment and retrieval must be provided

metadata <- openxlsx2::read_xlsx(metadata_path)
metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "logger_model", "date_deployed", "date_retrieved", "colony", "species")]
