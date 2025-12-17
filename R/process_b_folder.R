#' Process all light position files in a folder
#'
#' Given an import directory containing light data files this function processes all light data files in the folder.
#' It applies calibration data, filter settings, colony info, and extra metadata as needed for each logger/year combination found in the folder using 'process_logger_year()'
#' In calibration mode, it can export a combined calibration data file for all processed loggers.
#' If not in calibration mode, it exports processed position data, filtering summaries, and twilight estimates for each logger/year combination.
#'
#' @param import_directory Directory containing raw light data files. These are expected to be named in the format `<logger_id>_<year_retrieved>_<logger_model>`, e.g. `C23_2015_mk4083`
#' @param calibration_data A data frame containing metadata/calibration data for all loggers or a string providing a filepath to read this data from. This can be from an excel file, CSV file or a directory containing multiple calibration files.
#' In calibration mode, this dataframe can consist of a single row per logger/year combination.
#' In calibration mode, the minimum required columns are `logger_id`, `species`, `colony`, `date_deployed` and `date_retrieved`. Providing `logger_model` is strongly advised.
#' If not in calibration mode, the data frame is expected to be in the format output by running this function in calibration mode.
#' @param all_colony_info A data frame containing colony information for all loggers (one row per colony). The required columns are `colony`, `latitude`, and `longitude`.
#' @param filter_setting_list A 'GLSFilterSettingsList' object containing filter settings for loggers or a path to load one using 'read_filter_file()'. Defaults to 'seatrack_settings_list'.
#' @param extra_metadata Optional data frame containing extra metadata for loggers. This must have the column `logger_id` to join extra metadata. A `year_retrieved` column can also be provided to join based on a combination of logger and which session this is.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to `FALSE`.
#' @param output_dir An optional directory path to save processed outputs. Defaults to `NULL`.
#' @param calibration_mode A logical indicating whether to run in calibration mode. Defaults to `TRUE`.
#' If `TRUE`, the function can export or return a combined calibration data file for all logger/year combinations.
#' If `FALSE`, the function exports processed position data, filtering summaries, and twilight estimates for
#' each logger/year combination.
#' @param export_calibration_template A logical indicating whether to export an excel calibration template. Defaults to `TRUE`. If `FALSE`, the calibration template is returned.
#' @concept processing
#' @export
process_folder <- function(
    import_directory, calibration_data, all_colony_info,
    filter_setting_list = seatrackRgls::seatrack_settings_list, extra_metadata = NULL, show_filter_plots = FALSE,
    output_dir = NULL, calibration_mode = TRUE, export_calibration_template = TRUE) {
    if (is.character(calibration_data)) {
        calibration_data <- read_cal_files(calibration_data)
    }

    file_info <- scan_import_dir(import_directory)
    if (nrow(file_info) == 0) {
        print("No light files found in import diretory.")
        return(NULL)
    }
    all_logger_id_year <- file_info[!duplicated(file_info$id_year), ]
    print(paste("Found", nrow(all_logger_id_year), "unique logger ID + year combinations."))

    if (is.null(calibration_data$total_years_tracked)) {
        if (!is.null(calibration_data$date_retrieved)) {
            calibration_id_year <- paste0(calibration_data$logger_id, "_", format(calibration_data$date_retrieved, "%Y"))
        } else {
            stop("Deployment and retrieval date required.")
        }
    } else {
        # split total years tracked into individual years
        retrieval_year <- sapply(strsplit(calibration_data$total_years_tracked, "_"), function(x) x[2])
        calibration_id_year <- paste0(calibration_data$logger_id, "_", retrieval_year)
    }
    file_info_id_year <- paste0(all_logger_id_year$logger_id, "_", all_logger_id_year$year_downloaded)
    # Filter this to only those with calibration data for both logger and year
    all_logger_id_year <- all_logger_id_year[file_info_id_year %in% calibration_id_year, ]
    print(paste("After filtering, processing", nrow(all_logger_id_year), "logger ID + year combinations with calibration data."))

    if (calibration_mode) {
        all_result <- list()
        folder_result_output_dir <- NULL
    } else {
        folder_result_output_dir <- output_dir
    }

    for (logger_idx in seq_len(nrow(all_logger_id_year))) {
        logger_id <- all_logger_id_year$logger_id[logger_idx]
        year <- all_logger_id_year$year_downloaded[logger_idx]
        result <- process_logger_year(
            logger_id = logger_id,
            year = year,
            import_directory = import_directory,
            calibration_data = calibration_data,
            filter_setting_list = filter_setting_list,
            all_colony_info = all_colony_info,
            extra_metadata = extra_metadata,
            show_filter_plots = show_filter_plots,
            plotting_dir = output_dir,
            output_dir = folder_result_output_dir,
            calibration_mode = calibration_mode
        )
        if (calibration_mode) {
            all_result <- c(all_result, list(result))
        }
    }
    if (calibration_mode) {
        all_calibration <- do.call(rbind, all_result)
        calibration_output_dir <- file.path(output_dir, "calibration_data")
        if (export_calibration_template && !file.exists(calibration_output_dir)) {
            # do some workbook formattign to make it easier to fill in
            calibration_to_wb(all_calibration, calibration_output_dir, "calibration.xlsx")
        } else if (file.exists(calibration_output_dir)) {
            print(paste("Calibration output directory", calibration_output_dir, "already exists, not overwriting existing calibration file."))
        }

        return(all_calibration)
    }
}
