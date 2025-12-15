#' Prepare Calibration Data from a Folder
#'
#' @param import_directory Directory containing raw light data files.
#' @param metadata A data frame containing calibration data for all loggers or a string providing a filepath to read this data from. This can be from an excel file, CSV file or a directory containing multiple calibration files.
#' In calibration mode, this dataframe can consist of a single row per logger/year combination.
#' In calibration mode, the minimum required columns are `logger_id`, `species`, `colony`, `date_deployed` and `date_retrieved`.
#' @param all_colony_info A data frame containing colony information for all loggers (one row per colony). The required columns are `colony`, `latitude`, and `longitude`.
#' @param output_directory Directory to save processed position outputs.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to FALSE.
#' @param export_calibration_template A logical indicating whether to export a calibration template. Defaults to TRUE.
#' @param filter_list A GLSfilterList object containing filter settings for loggers or a path to load one. Defaults to `seatrackRgls::seatrack_settings_list`.
#' @export
prepare_calibration <- function(
    import_directory,
    metadata,
    all_colony_info, 
    output_directory, 
    show_filter_plots = FALSE,
    export_calibration_template = TRUE,
    filter_list = seatrackRgls::seatrack_settings_list) {
    calibration_template <- process_folder(
        import_directory = import_directory,
        calibration_data = metadata,
        all_colony_info = all_colony_info,
        output_dir = output_directory,
        show_filter_plots = show_filter_plots,
        calibration_mode = TRUE,
        export_calibration_template = export_calibration_template,
        filter_list = filter_list
    )
    if (!export_calibration_template) {
        return(calibration_template)
    }
}

#' Process Positions from a Folder
#'
#' @param import_directory Directory containing raw light data files.
#' @param calibration_data Data frame containing calibration data for loggers or a path pointing to the calibration data.
#' @param all_colony_info A data frame containing colony information for all loggers (one row per colony). The required columns are `colony`, `latitude`, and `longitude`.
#' @param output_directory Directory to save processed position outputs.
#' @param extra_metadata Optional data frame containing extra metadata for loggers.
#' @param filter_list A GLSfilterList object containing filter settings for loggers or a path to load one. Defaults to `seatrackRgls::seatrack_settings_list`.
#' @export
process_positions <- function(
    import_directory,
    calibration_data,
    all_colony_info, 
    output_directory, 
    extra_metadata = NULL,
    filter_list = seatrackRgls::seatrack_settings_list) {
    result <- process_folder(
        import_directory = import_directory,
        calibration_data = calibration_data,
        all_colony_info = all_colony_info,
        output_dir = output_directory,
        show_filter_plots = TRUE,
        calibration_mode = FALSE,
        extra_metadata = extra_metadata,
        filter_list = filter_list
    )
}
