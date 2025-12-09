#' Generate a sequence of breeding months
#'
#' This function generates a sequence of months representing the breeding period,
#' taking into account cases where the breeding period spans the end of the year.
#' @param start_month An integer representing the starting month of the breeding period (1-12).
#' @param end_month An integer representing the ending month of the breeding period (1-12).
#' @return A vector of integers representing the months in the breeding period.
get_breeding_month_seq <- function(start_month = 6, end_month = 8) {
    if (end_month < start_month) {
        end_month <- end_month + 12
    }
    breeding_seq <- seq(start_month, end_month)
    breeding_seq[breeding_seq > 12] <- breeding_seq[breeding_seq > 12] - 12
    return(breeding_seq)
}

#' Wrapper to export filter plots
#'
#' This function wraps around a code call to export plots used in filtering steps.
#' If `show_filter_plots` is FALSE, the code is simply evaluated without plotting
#' Otherwise, if `show_filter_plots` is TRUE, the code is evaluated and the resulting plot is saved to a PNG file in the specified `filter_plots_dir`.
#' @param call The code to be evaluated, typically a function call that generates a plot.
#' @param show_filter_plots Logical indicating whether to show and save filter plots.
#' @param plot_name A string specifying the name of the plot file (without extension).
#' @param logger_id_year A string to identify the logger and year, used in the plot filename.
#' @param filter_plots_dir The directory where the plot files should be saved. If NULL, plots are not saved to files.
#' @return The result of evaluating the provided code call.
export_filter_plot <- function(call = {}, show_filter_plots = TRUE, plot_name = "", logger_id_year = "", plotting_dir = NULL) {
    if (show_filter_plots == FALSE) {
        return(eval(call))
    }

    if (!is.null(plotting_dir)) {
        filter_plots_dir <- file.path(plotting_dir, "filter_plots")
        if (!is.null(filter_plots_dir) && !dir.exists(filter_plots_dir)) {
            dir.create(filter_plots_dir, recursive = TRUE)
        }

        png(
            filename = file.path(filter_plots_dir, paste0(logger_id_year, "_", plot_name, ".png")),
            height = 8, width = 10, units = "cm", res = 500
        )
    }

    result <- eval(call)
    mtext(side = 3, text = paste(logger_id_year, plot_name), line = 0.5, cex = 0.7)

    if (!is.null(plotting_dir)) {
        dev.off()
    }
    return(result)
}
get_sun_angle_seq <- function(data, light_data_calibration) {
    sun <- seq(light_data_calibration$sun_angle_start, light_data_calibration$sun_angle_end, length.out = nrow(data))
    return(sun)
}

get_adjust_lon <- function(posdata_export) {
    adjust_lon <- 0
    if ((nrow(posdata_export[lubridate::month(posdata_export$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4) & abs(posdata_export$lon) > 90, ]) / nrow(posdata_export[lubridate::month(posdata_export$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4), ])) > 0.5) adjust_lon <- 360
    return(adjust_lon)
}

get_map_ver <- function(posdata_export) {
    map_ver <- "world"
    adjust_lon <- get_adjust_lon(posdata_export)
    if (adjust_lon == 360) map_ver <- "world2"
    return(map_ver)
}

apply_adjust_lon <- function(posdata, adjust_lon) {
    posdata$lon[posdata$lon < 0] <- posdata$lon[posdata$lon < 0] + adjust_lon
    return(posdata)
}

get_boundary_box <- function(light_data_calibration, logger_filter) {
    if (is.null(light_data_calibration$bbox_xmin)) {
        boundary.box <- logger_filter$boundary.box
    } else {
        boundary.box <- sf::st_bbox(c(
            xmin = light_data_calibration$bbox_xmin,
            ymin = light_data_calibration$bbox_ymin,
            xmax = light_data_calibration$bbox_xmax,
            ymax = light_data_calibration$bbox_ymax
        ))
    }
    return(boundary.box)
}

scan_import_dir <- function(import_directory) {
    print("Scan import directory for files...")
    all_files <- list.files(import_directory, pattern = "*.lux|*.lig", recursive = TRUE, full.names = TRUE)
    if (length(all_files) == 0) {
        return(data.frame())
    }
    all_files_split <- strsplit(basename(all_files), "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], id_year = paste(x[1], x[2]))
    })
    file_info <- do.call(rbind, file_info_list)
    file_info <- data.frame(filename = all_files, file_info)

    return(file_info)
}

get_calibration_splits <- function(logger_data, split_years = "06-01") {
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
    return(time_windows)
}