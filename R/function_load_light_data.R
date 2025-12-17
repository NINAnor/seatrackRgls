#' Load lightdata from files
#'
#' Loads light data from specified file paths. Will handle different file formats based on file extensions.
#'
#' @param filepaths A vector of file paths to logger data files.
#' @return A data frame containing the loaded light data.
#' @keywords internal
get_light_data <- function(filepaths) {
    # Taken from Vegard's original script
    # Reworking some of this to use column headers would make it more readable.
    light_filepath <- filepaths[tolower(tools::file_ext(filepaths)) %in% c("lig", "lux")]
    if (length(light_filepath) == 0) {
        stop("No light data file found (expected .lig or .lux file).")
    }
    file_extension <- tools::file_ext(light_filepath)
    if (all(tolower(file_extension) == "lig")) {
        all_light_data <- lapply(seq_along(light_filepath), function(i) {
            light_data <- read.table(light_filepath[i], sep = ",", skip = 1, header = FALSE, fill = TRUE)
            light_data$dtime <- datetime_conversion(light_data[, 2])
            light_data$lux <- light_data[, 4]
            return(light_data)
        })
    } else if (all(tolower(file_extension) == "lux")) {
        all_light_data <- lapply(seq_along(light_filepath), function(i) {
            light_data <- read.table(light_filepath[1], sep = "\t", header = FALSE, fill = TRUE, skip = 20)
            light_data$dtime <- datetime_conversion(light_data[, 1])
            light_data$lux <- light_data[, 2]
            light_data$V1 <- "ok"
            return(light_data)
        })
    }
    all_light_data <- do.call(rbind, all_light_data)
    all_light_data$lux <- as.numeric(gsub("\\,", ".", all_light_data$lux))
    all_light_data$date <- as.Date(all_light_data$dtime)
    all_light_data$date <- date_conversion(all_light_data$date)
    all_light_data$time <- strptime(paste("01.01.2000", substr(all_light_data$dtime, 12, 19), sep = " "), "%d.%m.%Y %H:%M:%S") # Not sure what is going on here?

    all_light_data$V1 <- tolower(all_light_data$V1)
    return(all_light_data)
}

#' Limit light data to calibration time windows
#'
#' @param light_data Data frame containing light data with a 'dtime' column.
#' @param logger_calibration_data Data frame containing calibration data with 'start_datetime' and 'end_datetime' columns.
#' @return A list of data frames, each containing light data limited to the corresponding calibration time window.
#' @keywords internal
limit_light_data <- function(light_data, logger_calibration_data) {
    limited_light_data <- lapply(seq_len(nrow(logger_calibration_data)), function(i) {
        start_datetime <- logger_calibration_data$start_datetime[i]
        end_datetime <- logger_calibration_data$end_datetime[i]

        return(light_data[light_data$dtime >= start_datetime & light_data$dtime <= end_datetime, ])
    })

    return(limited_light_data)
}
