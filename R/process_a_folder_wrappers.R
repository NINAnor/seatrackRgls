#' Prepare Calibration Data from a Folder
#'
#' Convience function for preparing GLS logger data for light data calibration. Uses `process_folder()` in calibration mode to handle a directory of light data. See 'process_folder()' for more details.
#' Custom settings can be provided using `filter_setting_list`. For the creation of an empty filter settings template see `create_filter_settings_file()`.
#' If `export_calibration_template` is `TRUE`, it will export an empty calibration sheet using 'calibration_to_wb()'.
#'
#' Will export calibration plots to assist in choosing appropriate sun angles for processing GLS logger data (see vignette). Once these are decided on, final positions can be exported using process_positions()
#'
#' @param import_directory Directory containing raw light data files. These are expected to be named in the format `<logger_id>_<year_retrieved>_<logger_model>`, e.g. `C23_2015_mk4083`
#' @param metadata A data frame containing calibration data for all loggers or a string providing a filepath to read this data from. This can be from an excel file, CSV file or a directory containing multiple calibration files.
#' This dataframe can consist of a single row per logger/year combination, with the following columns `logger_id`, `species`, `colony`, `date_deployed` and `date_retrieved`. Providing `logger_model` is strongly advised.
#' These will be automatically split into time windows, using the `year_split` setting (defaulting to "01-06" in all 'seatrack_settings_list').
#' Alternatively, custom time windows can be provided by including columns `start_datetime` and `end_datetime`, with one time window per row.
#' @param all_colony_info A data frame containing colony information for all loggers (one row per colony). The required columns are `colony`, `latitude`, and `longitude`.
#' @param output_directory Directory to save calibration templates and calibration plots. If `show_filter_plots` is `TRUE`
#' @param show_filter_plots A logical indicating whether to show individual filter plots for the default sun angle. Defaults to `FALSE`.
#' @param export_calibration_template When `TRUE` will export a calibration template in excel format. Otherwise returns the calibration template as an R object. Defaults to `TRUE`.
#' @param filter_setting_list A 'GLSFilterSettingsList' object containing filter settings for loggers or a path to load one using [seatrackRgls::read_filter_file()]. Defaults to 'seatrackRgls::seatrack_settings_list'.
#' @concept processing_wrapper
#' @export
prepare_calibration <- function(
    import_directory,
    metadata,
    all_colony_info,
    output_directory,
    show_filter_plots = FALSE,
    export_calibration_template = TRUE,
    filter_setting_list = seatrackRgls::seatrack_settings_list) {
    calibration_template <- process_folder(
        import_directory = import_directory,
        calibration_data = metadata,
        all_colony_info = all_colony_info,
        output_dir = output_directory,
        show_filter_plots = show_filter_plots,
        calibration_mode = TRUE,
        export_calibration_template = export_calibration_template,
        filter_setting_list = filter_setting_list
    )
    if (!export_calibration_template) {
        # If not exporting, return the calibration template to R.
        return(calibration_template)
    }
}

#' Process Positions from a Folder
#'
#' Convience function for processing GLS logger data using provided calibration values. Uses 'process_folder()' to process a folder of light data. See 'process_folder()' for more details.
#' Custom settings can be provided using `filter_setting_list`. For the creation of an empty filter settings template see 'create_filter_settings_file()'
#'
#' Will export a folder containing (for each logger), processed positions, twilight estimates, plots showing the effects of filters, a folder of summary files detailing the results of filters used and maps.
#'
#' @param import_directory Directory containing raw light data files. These are expected to be named in the format `<logger_id>_<year_retrieved>_<logger_model>`, e.g. `C23_2015_mk4083`
#' @param calibration_data Data frame containing calibration data for loggers or a path pointing to the calibration data, as created by prepare_calibration().
#' @param all_colony_info A data frame containing colony information for all loggers (one row per colony). The required columns are `colony`, `latitude`, and `longitude`.
#' @param output_directory Directory to save processed position outputs, plots etc.
#' @param extra_metadata Optional data frame containing extra metadata for loggers. This must have the column `logger_id` to join extra metadata. A `year_retrieved` column can also be provided to join based on a combination of logger and which session this is.
#' @param filter_setting_list A 'GLSFilterSettingsList' object containing filter settings for loggers or a path to load one using 'read_filter_file()'. Defaults to 'seatrack_settings_list'.
#' @concept processing_wrapper
#' @export
process_positions <- function(
    import_directory,
    calibration_data,
    all_colony_info,
    output_directory,
    extra_metadata = NULL,
    filter_setting_list = seatrackRgls::seatrack_settings_list) {
    result <- process_folder(
        import_directory = import_directory,
        calibration_data = calibration_data,
        all_colony_info = all_colony_info,
        output_dir = output_directory,
        show_filter_plots = TRUE,
        calibration_mode = FALSE,
        extra_metadata = extra_metadata,
        filter_setting_list = filter_setting_list
    )
}
