#' Load and process a logger/year combination's light files
#'
#' Given a set of filepaths for light data files, calibration data, filter settings, colony info and extra metadata,
#' this function processes the light data to estimate positions and apply various filters depending on whether calibration mode is active or not.
#'
#' @param filepaths A vector of file paths to the light data files.
#' @param logger_calibration_data A data frame containing calibration data for the logger. If multiple calibration windows are provided, each will be processed in sequence.
#' @param filter_setting_list A list of filter settings for different species. Defaults to `seatrack_settings_list`.
#' @param logger_colony_info A data frame containing colony information for the logger.
#' @param logger_extra_metadata A data frame containing extra metadata for the logger.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to FALSE.
#' @param plotting_dir An optional directory path to save plotting outputs. Defaults to NULL.
#' @param calibration_mode A logical indicating whether to run in calibration mode. Defaults to TRUE.
#' @param min_length Number indicating minimum length of light data. Anything below this will fail. Defaults to 40.
#'
#' @concept processing
#' @return If calibration_mode is FALSE, returns a list containing:
#'          - `twilight_estimates`: A data frame of twilight estimates.
#'          - `posdata_export`: A data frame of processed position data.
#'          - `filtering`: A data frame summarizing the filtering steps applied.
#' If calibration_mode is TRUE, returns data frame of default calibration outputs and exports calibration plots.
process_logger_light_data <- function(
    filepaths,
    logger_calibration_data,
    filter_setting_list,
    logger_colony_info,
    logger_extra_metadata = NULL,
    show_filter_plots = FALSE,
    plotting_dir = NULL,
    calibration_mode = TRUE,
    min_length = 40) {
    # create dir for plotting
    if (!is.null(plotting_dir) && !dir.exists(plotting_dir)) {
        dir.create(plotting_dir, recursive = TRUE)
    }

    # if there is more than one light file,
    # First check the logger model
    print("Load light data...")
    all_light_data <- tryCatch(
        {
            get_light_data(filepaths)
        },
        error = function(e) {
            print(paste("Error loading file:", e))
            return(NULL)
        }
    )
    if (!is.null(all_light_data)){
        print("Limit light data to calibration time windows...")
        light_data_split <- limit_light_data(all_light_data, logger_calibration_data)
    }else {
        light_data_split <- rep(list(data.frame()), nrow(logger_calibration_data))
    }


    all_results <- list()

    for (i in seq_along(light_data_split)) {
        print(paste("Processing calibration window", i, "of", nrow(logger_calibration_data)))
        light_data <- light_data_split[[i]]
        light_data_calibration <- logger_calibration_data[i, ]
        logger_filter <- filter_setting_list$get_settings_from_list(species = light_data_calibration$species[1], colony = light_data_calibration$colony[1], logger_id = light_data_calibration$logger_id[1], years_tracked = light_data_calibration$years_tracked[1])

        if (nrow(light_data) < min_length) {
            print(paste("Light data has only", nrow(light_data), "rows, skipping."))
            if (calibration_mode) {
                result <- light_data_calibration
                result <- add_default_cols(result)
                result$problem <- TRUE
            }
        } else {
            result <- tryCatch(
                {
                    process_result <- apply_filters(
                        light_data,
                        light_data_calibration,
                        logger_filter,
                        logger_colony_info,
                        logger_extra_metadata,
                        show_filter_plots,
                        plotting_dir,
                        calibration_mode = calibration_mode
                    )
                    if (calibration_mode) {
                        process_result$problem <- FALSE
                    }
                    return(process_result)
                },
                error = function(e) {
                    print(paste("Error in processing:", e))
                    if (calibration_mode) {
                        process_result <- logger_calibration_data[i, ]
                        process_result <- add_default_cols(process_result)
                        process_result$problem <- TRUE
                        return(process_result)
                    } else {
                        return(NULL)
                    }
                }
            )
        }

        result <- result[!sapply(result, is.null)]
        all_results <- c(all_results, list(result))
    }

    # Handle results dependng on calibration mode
    if (calibration_mode == FALSE) {
        # Combine results
        combined_twilight_estimates <- do.call(rbind, lapply(all_results, function(x) x$twilight_estimates))
        combined_posdata_export <- do.call(rbind, lapply(all_results, function(x) x$posdata_export))
        if (!is.null(combined_posdata_export)) {
            combined_posdata_export$raw_data_file <- basename(filepaths[1])
        }

        combined_filtering <- do.call(rbind, lapply(all_results, function(x) x$filtering))

        return(list(
            twilight_estimates = combined_twilight_estimates,
            posdata_export = combined_posdata_export,
            filtering = combined_filtering
        ))
    } else {
        # In calibration mode, combine the new calibration dataframe
        combined_calibration <- do.call(rbind, all_results)
        return(combined_calibration)
    }
}
