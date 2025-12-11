#' @title GLSsettings Class
#' @description A convenience class for storing settings lists alongside identifiers to which logger/species/colony they belong
#' @export
#' @family classes
GLSsettings <- R6::R6Class(
    "GLSsettings",
    public = list(
        #' @description
        #' Create a new GLSsettings object
        #' @param logger_id Optional logger ID to associate with these settings.
        #' @param species Optional species name to associate with these settings.
        #' @param colony Optional colony name to associate with these settings.
        #' @param settings Optional list of filter settings. If NULL, defaults to settings for the provided species from `seatrackRgls::seatrack_settings_list`.
        initialize = function(logger_id = NULL, species = NULL, colony = NULL, settings = NULL) {
            self$logger_id <- tolower(logger_id)
            self$species <- tolower(species)
            self$colony <- tolower(colony)

            if (!is.null(settings)) {
                self$settings <- settings
            } else {
                if (!is.null(species)) {
                    self$settings <- seatrackRgls::seatrack_settings_list$get_settings_from_list(species = species)
                } else {
                    self$settings <- seatrackRgls::seatrack_settings_list$get_settings_from_list(species = "black-legged kittiwake")
                }
            }
        },
        #' @field species Species identifier
        species = NULL,
        #' @field colony Colony identifier
        colony = NULL,
        #' @field logger_id Logger ID identifier
        logger_id = NULL,
        #' @field settings A list containing filter settings for the logger/species/colony.
        settings = list(),
        #' @description
        #' Check if these settings are for a particular combination of logger/species/colony
        #' @param species Optional species name to match.
        #' @param logger_id Optional logger ID to match.
        #' @param colony Optional colony name to match.
        #' @return TRUE if the settings match the provided identifiers, FALSE otherwise.
        check_settings_for = function(species = NULL, logger_id = NULL, colony = NULL) {
            if (!is.null(logger_id)) {
                if (!is.null(self$logger_id)) {
                    if (self$logger_id != tolower(logger_id)) {
                        return(FALSE)
                    }
                }
            }
            if (!is.null(species)) {
                if (!is.null(self$species)) {
                    if (self$species != tolower(species)) {
                        return(FALSE)
                    }
                }
            }
            if (!is.null(colony)) {
                if (!is.null(self$colony)) {
                    if (self$colony != tolower(colony)) {
                        return(FALSE)
                    }
                }
            }
            return(TRUE)
        }
    )
)

#' @title GLSfilterList
#' @description A convenience class for storing a list of GLSsettings objects and retrieving settings based on logger/species/colony.
#' @export
#' @family classes
GLSfilterList <- R6::R6Class(
    "GLSfilterList",
    public = list(
        #' @description
        #' Create a new GLSfilterList object
        #' @param filter_list A list of GLSsettings objects.
        initialize = function(filter_list = list()) {
            self$filter_list <- filter_list
        },
        #' @field filter_list List of GLSsettings objects
        filter_list = list(),
        #' @description
        #' Get Settings from settings_list based on logger ID, species, or colony.
        #' @param logger_id Optional logger ID to match.
        #' @param species Optional species name to match.
        #' @param colony Optional colony name to match.
        #' @return A list of filter settings matching the provided identifiers. If no match is found, returns default settings for the species.
        get_settings_from_list = function(species = NULL, colony = NULL, logger_id = NULL) {
            for (settings_obj in self$filter_list) {
                if (settings_obj$check_settings_for(species, colony, logger_id)) {
                    return(settings_obj$settings)
                }
            }
            if (!is.null(species)) {
                print("No matching settings found in filter list, using default settings for species.")
                return(seatrackRgls::seatrack_settings_list$get_settings_from_list(species = species))
            } else {
                print("No matching settings found in filter list, using default settings for black-legged kittiwake.")
                return(seatrackRgls::seatrack_settings_list$get_settings_from_list(species = "black-legged kittiwake"))
            }
        }
    )
)



#' Read Filter File
#'
#' Read filter settings from an Excel or CSV file and return as a GLSfilterList object.
#' The filter file should contain columns for logger_id, species, colony, and various filter settings.
#' See `create_filter_file` for an example of the expected format.
#' @param filepath Filepath to the filter settings file (Excel or CSV).
#' @return A GLSfilterList object containing the filter settings.
#' @export
read_filter_file <- function(filepath) {
    file_extension <- tools::file_ext(filepath)
    if (tolower(file_extension) %in% c("xlsx", "xls")) {
        filter_df <- openxlsx2::wb_to_df(filepath, sheet = "Filter settings")
    } else if (tolower(file_extension) == "csv") {
        filter_df <- read.csv(filepath, header = TRUE)
    } else {
        stop("Unsupported filter file format. Please provide an Excel (.xlsx/.xls) or CSV (.csv) file.")
    }
    filter_list <- list()
    for (i in seq_len(nrow(filter_df))) {
        row <- filter_df[i, ]
        settings <- as.list(row)
        # Convert boundary.box and months_breeding from string to numeric vector
        settings$boundary.box <- c(
            xmin = as.numeric(settings$boundary.box_xmin),
            xmax = as.numeric(settings$boundary.box_xmax),
            ymin = as.numeric(settings$boundary.box_ymin),
            ymax = as.numeric(settings$boundary.box_ymax)
        )
        settings$boundary.box_xmin <- NULL
        settings$boundary.box_xmax <- NULL
        settings$boundary.box_ymin <- NULL
        settings$boundary.box_ymax <- NULL

        settings$months_breeding <- get_breeding_month_seq(settings$months_breeding_start, settings$months_breeding_end)

        settings$months_breeding_start <- NULL
        settings$months_breeding_end <- NULL

        if (is.na(settings$coast_to_sea)) {
            settings$coast_to_sea <- Inf
        }
        if (is.na(settings$coast_to_land)) {
            settings$coast_to_land <- Inf
        }

        # Create SettingsList object
        settings_obj <- SettingsList$new(
            logger_id = ifelse(is.na(settings$logger_id), NULL, settings$logger_id),
            species = ifelse(is.na(settings$species), NULL, settings$species),
            colony = ifelse(is.na(settings$colony), NULL, settings$colony),
            settings = settings
        )
        filter_list[[length(filter_list) + 1]] <- settings_obj
    }
    GLSfilterList$new(filter_list)
}


create_filter_file <- function(filepath, species = c()) {
    empty <- FALSE
    if (length(species) == 0) {
        empty <- TRUE
        species <- "black-legged kittiwake"
    }

    all_rows <- lapply(species, function(species_name) {
        species_name <- tolower(species_name)
        if (species_name %in% names(seatrackRgls::seatrack_settings_list)) {
            seatrack_default <- seatrackRgls::seatrack_settings_list[[species]]
        } else {
            seatrack_default <- seatrackRgls::seatrack_settings_list[["black-legged kittiwake"]]
        }
        months_breeding <- seatrack_default$months_breeding
        seatrack_default$months_breeding_start <- months_breeding[1]
        seatrack_default$months_breeding_end <- months_breeding[length(months_breeding)]
        seatrack_default$months_breeding <- NULL

        boundary.box <- seatrack_default$boundary.box
        names(boundary.box) <- paste("boundary.box", names(boundary.box), sep = "_")
        seatrack_default$boundary.box <- NULL
        seatrack_default <- c(seatrack_default, boundary.box)

        seatrack_default$species <- NULL

        if (is.infinite(seatrack_default$coast_to_sea)) {
            seatrack_default$coast_to_sea <- NA
        }
        if (is.infinite(seatrack_default$coast_to_land)) {
            seatrack_default$coast_to_land <- NA
        }

        seatrack_default_df <- data.frame(species = species_name, colony = NA, logger_id = NA, seatrack_default)
    })
    all_rows <- do.call(rbind, all_rows)
    if (empty) {
        all_rows <- all_rows[-1, ]
    }
    # write to excel file
    wb <- openxlsx2::wb_workbook()
    wb$add_worksheet("Filter settings")
    wb$add_data_table(sheet = "Filter settings", x = all_rows, banded_rows = TRUE, table_style = "TableStyleMedium19", na.strings = "")
    wb$set_col_widths(sheet = "Filter settings", cols = 1:ncol(all_rows), widths = "auto")
    wb$freeze_pane(sheet = "Filter settings", firstActiveRow = 2, firstActiveCol = 4)
    wb$save(file = filepath, overwrite = TRUE)
}
