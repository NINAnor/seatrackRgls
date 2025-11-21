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
#' @export
export_filter_plot <- function(call = {}, show_filter_plots = TRUE, plot_name = "", logger_id_year = "", filter_plots_dir = NULL) {
    if(show_filter_plots == FALSE) {
        return(eval(call))
    }

    if (!is.null(filter_plots_dir)) {
        png(filename = file.path(filter_plots_dir, paste0(logger_id_year, "_", plot_name, ".png")),
            height = 8, width = 10, units = "cm", res = 500
        )
    }

    result <- eval(call)
    mtext(side = 3, text = paste(logger_id_year, plot_name), line = 0.5, cex = 0.7)

    if (!is.null(filter_plots_dir)) {
        dev.off()
    }
    return(result)
}