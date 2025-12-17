#' Write Calibration Data to Excel Workbook
#'
#' Writes the provided calibration data frame to an Excel workbook with formatting.
#' @param all_calibration Data frame containing calibration data for all loggers.
#' @param calibration_output_dir Directory to save the calibration Excel workbook.
#' @param calibration_filename Name of the calibration Excel file. Defaults to "calibration_<current_date>.xlsx".
#' @return None. Saves the calibration data to an Excel workbook at the specified file path.
#' @concept function
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
    if (dir.exists(calibration_data)) {
        files <- list.files(calibration_data, full.names = TRUE)
    } else {
        files <- calibration_data
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
