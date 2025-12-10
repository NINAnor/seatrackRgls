#' Process GLS data for a single logger and year
#'
#' Given a logger ID and year, this function processes all light data files for that logger/year combination,
#' applying calibration data, filter settings, colony info, and extra metadata as needed.
#' For more information on the exact steps see the help file for `process_logger_positions()`.
#'
#' @param logger_id The ID of the logger to process.
#' @param year The END year to process for the logger.
#' @param import_directory The directory containing the light data files. File are expected to be named in the format 'logger_id'_`end_year`.
#' @param calibration_data A data frame containing calibration data for all loggers or a string providing a filepath to read this data from. This can be from an excel file, CSV file or a directory containing multiple calibration files.
#' In calibration mode, this dataframe can consist of a single row per logger/year combination.
#' In calibration mode, the minimum required columns are `logger_id`, `species`, `colony`, `date_deployed` and `date_retrieved`.
#' If not in calibration mode, the data frame is expected to be in the format output by running this function in calibration mode.
#' @param all_colony_info A data frame containing colony information for all loggers.
#' @param filter_list A list of filter settings for different species. Defaults to `seatrack_settings_list`.
#' @param extra_metadata A data frame containing extra metadata for all loggers.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to FALSE.
#' @param export_maps A logical indicating whether to export maps of the processed positions. Defaults to TRUE.
#' @param plotting_dir An optional directory path to save plotting outputs. Defaults to NULL.
#' @param output_dir An optional directory path to save processed outputs. Defaults to NULL.
#' @param calibration_mode A logical indicating whether to run in calibration mode. Defaults to TRUE
#' @param split_years A character string indicating the month and day to split light data into time windows on when only deployment and retrieval dates are provided (format "MM-DD"). Defaults to "06-01".
#' @param analyzer An optional string indicating the analyzer who provided calibration data, if this column is not provided in calibration data. Defaults to an empty string.
#' @return If calibration_mode is FALSE, returns a list containing:
#'          - `twilight_estimates`: A data frame of twilight estimates.
#'          - `posdata_export`: A data frame of processed position data.
#'          - `filtering`: A data frame summarizing the filtering steps applied.
#' If calibration_mode is TRUE, returns data frame of default calibration outputs and (if plottinging dir is not NULL) exports calibration plots.
#' @export
process_logger_year <- function(
    logger_id,
    year,
    import_directory,
    calibration_data,
    all_colony_info,
    filter_list = seatrackRgls::seatrack_settings_list,
    extra_metadata = NULL,
    show_filter_plots = FALSE,
    export_maps = TRUE,
    plotting_dir = NULL,
    output_dir = NULL,
    calibration_mode = TRUE,
    split_years = "06-01",
    analyzer = "") {
    print(paste("Processing logger", logger_id, "for year", year))
    file_info <- scan_import_dir(import_directory)
    file_info <- file_info[file_info$logger_id == logger_id & file_info$year_downloaded == year, ]

    if (nrow(file_info) == 0) {
        stop("No files found for this logger/year combination.")
    }

    if (is.character(calibration_data)) {
        calibration_data <- read_cal_files(calibration_data)
    }

    if (is.null(calibration_data$species)) {
        stop("calibration_data must contain species")
    }

    if (is.null(calibration_data$colony)) {
        stop("calibration_data must contain colony name")
    }

    logger_calibration_data <- calibration_data[calibration_data$logger_id == logger_id, ]


    if (is.null(logger_calibration_data$logger_model)) {
        print("logger_model not found in calibration_data, setting to empty string")
        logger_calibration_data$logger_model <- ""
    } else {
        model <- logger_calibration_data$logger_model[1]
        if (nrow(file_info) > 1) {
            file_info <- file_info[tolower(file_info$logger_model) == tolower(model), ]
        }
        if (nrow(file_info) == 0) {
            stop("No files found due to mismatch between logger model", model, "in metadata and filenames")
        } else if (!all(file_info$logger_model == model)) {
            print("Mismatch between logger model", model, "in metadata and filenames")
        }
    }

    filepaths <- file_info$filename




    if (is.null(logger_calibration_data$total_years_tracked)) {
        # Must have deployment date and retrieval date instead
        if (is.null(logger_calibration_data$date_deployed) || is.null(logger_calibration_data$date_retrieved)) {
            stop("Calibration data must contain either start/end datetimes or deployment/retrieval dates.")
        }

        retrieval_year <- format(logger_calibration_data$date_retrieved, "%Y")
        logger_calibration_data <- logger_calibration_data[retrieval_year == year, ]

        if (nrow(logger_calibration_data) == 0) {
            stop("No calibration data for this logger/year combination")
        }

        # split by deployment/retrieval dates
        time_windows <- get_calibration_splits(logger_calibration_data, split_years)
        logger_calibration_data <- data.frame(
            logger_calibration_data[, c("logger_id", "logger_model")],
            time_windows,
            logger_calibration_data[, !names(logger_calibration_data) %in% c("logger_id", "logger_model", "date_deployed", "date_retrieved")]
        )
    } else {
        calibration_year <- sapply(strsplit(logger_calibration_data$total_years_tracked, "_"), function(x) x[2])

        logger_calibration_data <- logger_calibration_data[calibration_year == year, ]
        if (nrow(logger_calibration_data) == 0) {
            stop("No calibration data for this logger/year combination")
        }
    }

    deployment_year <- format(min(logger_calibration_data$start_datetime), "%Y")
    retrieval_year <- format(max(logger_calibration_data$end_datetime), "%Y")
    logger_calibration_data$total_years_tracked <- paste(deployment_year, retrieval_year, sep = "_")

    start_years <- format(logger_calibration_data$start_datetime, "%Y")
    end_years <- format(logger_calibration_data$end_datetime, "%Y")
    logger_calibration_data$year_tracked <- paste(start_years, end_years, sep = "_")

    if (is.null(logger_calibration_data$analyzer)) {
        logger_calibration_data$analyzer <- analyzer
    }

    # Get extra metadata for this logger and end year
    if (!is.null(extra_metadata)) {
        extra_metadata_year <- format(extra_metadata$date_retrieved, "%Y")
        logger_extra_metadata <- extra_metadata[extra_metadata$logger_id == logger_id & extra_metadata_year == year, ]
    } else {
        logger_extra_metadata <- NULL
    }

    # Get logger colony info
    logger_colony_info <- all_colony_info[all_colony_info$colony == logger_calibration_data$colony[1], ]

    # Get logger filter settings
    logger_filter <- filter_list[[tolower(logger_calibration_data$species[1])]]

    result <- process_logger_light_data(
        filepaths = filepaths,
        logger_calibration_data = logger_calibration_data,
        logger_filter = logger_filter,
        logger_colony_info = logger_colony_info,
        logger_extra_metadata = logger_extra_metadata,
        show_filter_plots = show_filter_plots,
        plotting_dir = plotting_dir,
        calibration_mode = calibration_mode
    )

    if (calibration_mode == FALSE) {
        if (!is.null(output_dir) && !is.null(result)) {
            pos_output_dir <- file.path(output_dir, "processed_positions")
            if (!dir.exists(pos_output_dir)) {
                dir.create(pos_output_dir, recursive = TRUE)
            }
            output_filepath <- file.path(pos_output_dir, paste0("positions_-_", logger_id, "_", year, ".csv"))
            write.csv(result$posdata_export, output_filepath, row.names = FALSE)
            print(paste("Exported processed positions to", output_filepath))

            filter_output_dir <- file.path(output_dir, "filtering_summaries")
            if (!dir.exists(filter_output_dir)) {
                dir.create(filter_output_dir, recursive = TRUE)
            }
            filter_filepath <- file.path(filter_output_dir, paste0("filtering_-_", logger_id, "_", year, ".csv"))
            write.csv(result$filtering, filter_filepath, row.names = FALSE)
            print(paste("Exported filtering summary to", filter_filepath))

            twilight_output_dir <- file.path(output_dir, "twilight_estimates")
            if (!dir.exists(twilight_output_dir)) {
                dir.create(twilight_output_dir, recursive = TRUE)
            }
            twilight_filepath <- file.path(twilight_output_dir, paste0("twilights_-_", logger_id, "_", year, ".csv"))
            write.csv(result$twilight_estimates, twilight_filepath, row.names = FALSE)
            print(paste("Exported twilight estimates to", twilight_filepath))
            if (export_maps) {
                map_output_dir <- file.path(output_dir, "maps")
                if (!dir.exists(map_output_dir)) {
                    dir.create(map_output_dir, recursive = TRUE)
                }

                for (year_tracked in unique(result$posdata_export$year_tracked)) {
                    map_filepath <- file.path(map_output_dir, paste0("map_-_", logger_id, "_", year, "_-_", year_tracked, ".png"))

                    png(filename = map_filepath, height = 20, width = 20, units = "cm", res = 500)
                    plot_a_map(result$posdata_export[result$posdata_export$year_tracked == year_tracked, ])
                    dev.off()
                    print(paste("Exported map to", map_filepath))
                }
            }
        }
    } else {
        if (!is.null(output_dir)) {
            calibration_output_dir <- file.path(output_dir, "calibration_data")
            if (!dir.exists(calibration_output_dir)) {
                dir.create(calibration_output_dir, recursive = TRUE)
            }
            calibration_filepath <- file.path(calibration_output_dir, paste0("calibration_-_", logger_id, "_", year, ".csv"))
            write.csv(result, calibration_filepath, row.names = FALSE)
            print(paste("Exported calibration data to", calibration_filepath))
        }
    }

    return(result)
}
