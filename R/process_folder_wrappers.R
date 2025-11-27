#' Prepare Calibration Data from a Folder
#'
#' @param import_directory Directory containing raw light data files.
#' @param metadata Data frame containing simple metadata for loggers.
#' @param all_colony_info Data frame containing colony information.
#' @param output_directory Directory to save processed position outputs.
#' @export
prepare_calibration <- function(
    import_directory,
    metadata,
    all_colony_info, output_directory) {
    calibration_template <- process_folder(
        import_directory = import_directory,
        calibration_data = metadata,
        all_colony_info = all_colony_info,
        output_dir = output_directory,
        show_filter_plots = FALSE,
        calibration_mode = TRUE
    )
}

#' Process Positions from a Folder
#'
#' @param import_directory Directory containing raw light data files.
#' @param calibration_data Data frame containing calibration data for loggers.
#' @param all_colony_info Data frame containing colony information.
#' @param output_directory Directory to save processed position outputs.
#' @param extra_metadata Optional data frame containing extra metadata for loggers.
#' @export
process_positions <- function(
    import_directory,
    calibration_data,
    all_colony_info, output_directory, extra_metadata = NULL) {
    result <- process_folder(
        import_directory = import_directory,
        calibration_data = calibration_data,
        all_colony_info = all_colony_info,
        output_dir = output_directory,
        show_filter_plots = TRUE,
        calibration_mode = FALSE,
        extra_metadata = extra_metadata
    )
}
