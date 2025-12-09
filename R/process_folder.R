#' Process all light position files in a folder
#'
#' Given an import directory containing light data files, calibration data, filter settings, colony info and extra metadata, this function processes all light data files in the folder.
#' It applies calibration data, filter settings, colony info, and extra metadata as needed for each logger/year combination found in the folder.
#' In calibration mode, it exports a combined calibration data file for all processed loggers.
#' If not in calibration mode, it exports processed position data, filtering summaries, and twilight estimates for each logger/year combination.
#' @param import_directory The directory containing the light data files. Files are expected to be named in the format 'logger_id'_`end_year`.
#' @param calibration_data A data frame containing calibration data for all loggers or a string providing a filepath to read this data from. This can be from an excel file, CSV file or a directory containing multiple calibration files.
#' In calibration mode, this dataframe can consist of a single row per logger/year combination.
#' In calibration mode, the minimum required columns are `logger_id`, `species`, `colony`, `date_deployed` and `date_retrieved`.
#' If not in calibration mode, the data frame is expected to be in the format output by running this function in calibration mode.
#' @param all_colony_info A data frame containing colony information for all loggers.
#' @param filter_list A list of filter settings for different species. Defaults to `seatrack_settings_list`.
#' @param extra_metadata A data frame containing extra metadata for all loggers.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to FALSE.
#' @param output_dir An optional directory path to save processed outputs. Defaults to NULL.
#' @param calibration_mode A logical indicating whether to run in calibration mode. Defaults to TRUE.
#' If TRUE, the function exports a combined calibration data file for all processed loggers.
#' If FALSE, the function exports processed position data, filtering summaries, and twilight estimates for
#' each logger/year combination.
#' @param export_calibration_template A logical indicating whether to export an excel calibration template. Defaults to TRUE.
#' @return If calibration_mode is FALSE, returns nothing, but exports processed data to output_dir.
#' If calibration_mode is TRUE, returns data frame of default calibration outputs (which will also be saved as an excel file in output_dir) and (if output_dir is not NULL) exports calibration plots.
#' @export
process_folder <- function(
    import_directory, calibration_data, all_colony_info,
    filter_list = seatrackRgls::seatrack_settings_list, extra_metadata = NULL, show_filter_plots = FALSE,
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
            filter_list = filter_list,
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
        if (export_calibration_template) {
            # do some workbook formattign to make it easier to fill in
            calibration_output_dir <- file.path(output_dir, "calibration_data")
            calibration_to_wb(all_calibration, calibration_output_dir)
        }
        return(all_calibration)
    }
}

#' Write Calibration Data to Excel Workbook
#'
#' Writes the provided calibration data frame to an Excel workbook with formatting.
#' @param all_calibration Data frame containing calibration data for all loggers.
#' @param calibration_output_dir Directory to save the calibration Excel workbook.
#' @param calibration_filename Name of the calibration Excel file. Defaults to "calibration_<current_date>.xlsx".
#' @return None. Saves the calibration data to an Excel workbook at the specified file path.
#' @export
calibration_to_wb <- function(all_calibration, calibration_output_dir, calibration_filename = paste0("calibration_", Sys.Date(), ".xlsx")) {
    if (!dir.exists(calibration_output_dir)) {
        dir.create(calibration_output_dir, recursive = TRUE)
    }
    calibration_filepath <- file.path(calibration_output_dir, calibration_filename)

    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("calibration_data")
    wb$add_data_table(sheet = "calibration_data", x = all_calibration, banded_rows = TRUE, table_style = "TableStyleMedium19", na.strings = "")
    # wb$add_data(sheet = "calibration_data", x = all_calibration)
    wb$set_col_widths(sheet = "calibration_data", cols = 1:ncol(all_calibration), widths = "auto")
    wb$freeze_pane(sheet = "calibration_data", firstActiveRow = 2, firstActiveCol = 5)
    col_dims <- openxlsx2::wb_dims(
        x = all_calibration,
        cols = c("sun_angle_start"), select = "col_names"
    )

    col_letters <- sapply(strsplit(col_dims, ","), function(x) gsub("[[:digit:]]+", "", x))

    cf_formula <- sprintf('$%s2<>""', col_letters[1])

    tryCatch(
        {
            suppressWarnings(
                wb$add_dxfs_style(
                    "seatrack_pos",
                    fontColour = openxlsx2::wb_color(hex = "#006100"),
                    bgFill = openxlsx2::wb_color("#C6EFCE")
                )
            )
        },
        error = function(e) {
            invisible(NULL)
        }
    )

    wb$add_conditional_formatting(
        "calibration_data",
        dims = openxlsx2::wb_dims(x = all_calibration, select = "data"),
        rule = cf_formula,
        style = "seatrack_pos"
    )

    wb$save(file = calibration_filepath, overwrite = TRUE, )
    print(paste("Exported calibration data to", calibration_filepath))

}

read_cal_file <- function(f) {
    ext <- tolower(tools::file_ext(f))
    if (ext == "xlsx") {
        openxlsx2::wb_to_df(f)
    } else if (ext == "csv") {
        utils::read.csv(f, stringsAsFactors = FALSE)
    } else {
        NULL
    }
}

read_cal_files <- function(calibration_data) {
    files <- if (dir.exists(calibration_data)) {
        list.files(calibration_data, full.names = TRUE)
    } else {
        calibration_data
    }
    files <- files[file.exists(files)]
    if (length(files) == 0) stop("No calibration files found at path provided.")

    dfs <- lapply(files, function(f) {
        df <- read_cal_file(f)
        if (is.null(df)) warning("Skipping unsupported calibration file: ", f)
        df
    })
    calibration_data <- do.call(rbind, dfs[!vapply(dfs, is.null, logical(1))])
    return(calibration_data)
}
