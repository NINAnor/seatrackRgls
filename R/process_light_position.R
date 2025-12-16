add_default_cols <- function(light_data_calibration) {
    # Add columns if they are missing
    if (is.null(light_data_calibration$sun_angle_start)) {
        light_data_calibration$sun_angle_start <- NA
    }
    if (is.null(light_data_calibration$sun_angle_end)) {
        light_data_calibration$sun_angle_end <- NA
    }
    if (is.null(light_data_calibration$light_threshold)) {
        light_data_calibration$light_threshold <- NA
    }

    return(light_data_calibration)
}

#' Process light data to estimate positions and apply filters
#'
#' Given light data, calibration data, filter settings, colony info, and extra metadata,
#' this function processes the light data to estimate positions and apply various filters.
#'
#' @param light_data A data frame containing the light data.
#' @param light_data_calibration A data frame containing calibration data for the logger.
#' @param logger_colony_info A data frame containing colony information for the logger.
#' @param logger_filter A list of filter settings specific to the logger species.
#' @param logger_extra_metadata A data frame containing extra metadata for the logger. Defaults to NULL.
#' @param show_filter_plots A logical indicating whether to show filter plots. Defaults to FALSE.
#' @param plotting_dir An optional directory path to save plotting outputs. Defaults to NULL.
#' @param prev_posdata_export An optional data frame containing previous position data for comparison in seasonal calibration. Defaults to NULL.
#' @param type A string indicating the type of calibration: "main", "winter", or "summer". Defaults to "main". This setting is primarily for internal use during seasonal calibration. Generally users should only call this function with type = "main".
#' @param calibration_mode A logical indicating whether to run in calibration mode. Defaults to FALSE.
#' @return If calibration_mode is FALSE, returns a list containing:
#'          - `twilight_estimates`: A data frame of twilight estimates.
#'          - `posdata_export`: A data frame of processed position data.
#'          - `filtering`: A data frame summarizing the filtering steps applied.
#' If type is `main` and calibration_mode is FALSE, the returned `posdata_export` will include seasonal adjustments.
#' If calibration_mode is TRUE, returns data frame of default calibration outputs. If type is not `main`, compares sun angles to previous calibration in order to return adjusted sun angles.
#' @export
process_light_position <- function(
    light_data, light_data_calibration, logger_filter, logger_colony_info, logger_extra_metadata,
    show_filter_plots = FALSE, plotting_dir = NULL,
    prev_posdata_export = NULL, type = "main", calibration_mode = FALSE) {
    light_data_calibration <- add_default_cols(light_data_calibration)

    if (is.na(light_data_calibration$sun_angle_start)) {
        if (type != "main") {
            stop("Provide manual calibration values before continuing.")
        } else if (!calibration_mode) {
            print("Skipping due to lack of calibration values.")
            return(NULL)
        }
    }

    # Fill in default if neccesary
    if (type == "main") {
        light_data_calibration$sun_angle_start[is.na(light_data_calibration$sun_angle_start)] <- -3.5
        light_data_calibration$sun_angle_end[is.na(light_data_calibration$sun_angle_end)] <- light_data_calibration$sun_angle_start[is.na(light_data_calibration$sun_angle_end)]
        light_data_calibration$light_threshold[is.na(light_data_calibration$light_threshold)] <- 1
    } else if (type == "winter") {
        if (calibration_mode) {
            light_data_calibration$sun_angle_start <- -5
            light_data_calibration$sun_angle_end <- -5
        }
        light_data_calibration$light_threshold <- min(light_data$lux) + 0.1
    } else if (type == "summer") {
        if (calibration_mode) {
            light_data_calibration$sun_angle_start <- 0
            light_data_calibration$sun_angle_end <- 0
        }
        light_data_calibration$light_threshold <- get_threshold(light_data_calibration$logger_model)
    }

    if (!is.null(light_data_calibration$breeding_start_month)) {
        print("Using breeding months from calibration data")
        months_breeding <- get_breeding_month_seq(light_data_calibration$breeding_start_month, light_data_calibration$breeding_end_month)
    } else {
        print("Using breeding months from filter settings")
        months_breeding <- logger_filter$months_breeding
    }

    filtering <- data.frame(
        logger_id = light_data_calibration$logger_id,
        start_datetime = light_data_calibration$start_datetime,
        end_datetime = light_data_calibration$end_datetime
    )

    logger_id_year <- paste0(light_data_calibration$logger_id, "_", light_data_calibration$year_tracked)

    # Estimate twilights
    print("Estimating twilights...")
    twilight_data <-
        export_filter_plot(
            {
                twilight_estimation(
                    light_data,
                    light_data_calibration,
                    show_filter_plots
                )
            },
            show_filter_plots,
            plot_name = "1_twilight_estimation",
            logger_id_year = logger_id_year,
            plotting_dir
        )

    filtering$nrow_twilightCalc <- nrow(twilight_data)
    print(paste("Estimated", filtering$nrow_twilightCalc, "twilights."))

    # Add sun angle etc. to twilight data
    twilight_data <- data.frame(twilight_data, light_data_calibration[, c("sun_angle_start", "sun_angle_end", "light_threshold")])

    # Removes twilights of the same type less than 22 hours apart (tc)
    print("Cleaning twilight data...")
    twilight_data_tc <- export_filter_plot(
        {
            twilight_cleanup(
                df = twilight_data,
                breedingloc_lon = logger_colony_info$col_lon,
                breedingloc_lat = logger_colony_info$col_lat,
                months_breeding = months_breeding,
                species = light_data_calibration$species,
                sun_angle_start = light_data_calibration$sun_angle_start,
                sun_angle_end = light_data_calibration$sun_angle_end,
                show_plot = show_filter_plots
            )
        },
        show_filter_plots,
        plot_name = "2_twilight_cleanup",
        logger_id_year = logger_id_year,
        plotting_dir
    )

    # keep track of n edited twilights/filtered positions
    filtering$removed_twilight_cleanup <- (filtering$nrow_twilightCalc - nrow(twilight_data_tc))
    print(paste("Removed", filtering$removed_twilight_cleanup, "twilights during cleanup."))

    # detect outliers and move them back to mean of their neighbors
    twilight_data_mt <-
        export_filter_plot(
            {
                move_twilights(
                    df = twilight_data_tc,
                    speed = logger_filter$speed,
                    sun = get_sun_angle_seq(twilight_data_tc, light_data_calibration),
                    show_plot = show_filter_plots
                )
            },
            show_filter_plots,
            plot_name = "3_twilight_move",
            logger_id_year = logger_id_year,
            plotting_dir
        )

    filtering$moved_twilight_positions <- nrow(twilight_data_mt[!(twilight_data_mt$tFirst %in% twilight_data_tc$tFirst), ])
    print(paste("Moved", filtering$moved_twilight_positions, "twilight positions."))

    # daylengthfilter
    if (logger_filter$daylength_filter) {
        twilight_data_dl <-
            export_filter_plot(
                {
                    daylengthfilter(df = twilight_data_mt, show_plot = show_filter_plots)
                },
                show_filter_plots,
                plot_name = "4_daylength",
                logger_id_year = logger_id_year,
                plotting_dir
            )
    } else {
        twilight_data_dl <- twilight_data_mt
    }
    filtering$removed_daylengthfilter <- (nrow(twilight_data_mt) - nrow(twilight_data_dl))
    print(paste("Removed", filtering$removed_daylengthfilter, "twilights during daylength filtering."))

    # noonfilter
    if (logger_filter$noon_filter) {
        twilight_data_nf <-
            export_filter_plot(
                {
                    noonfilter(df = twilight_data_dl, show_plot = show_filter_plots)
                },
                show_filter_plots,
                plot_name = "5_noonfilter",
                logger_id_year = logger_id_year,
                plotting_dir
            )
    } else {
        twilight_data_nf <- twilight_data_dl
    }
    if (nrow(twilight_data_dl) == nrow(twilight_data_nf)) {
        logger_filter$noon_filter <- FALSE
    }
    filtering$removed_noonfilter <- (nrow(twilight_data_dl) - nrow(twilight_data_nf))
    print(paste("Removed", filtering$removed_noonfilter, "twilights during noon filtering."))

    # CALCULATE POSITIONS---------------------------------
    latlon <- GeoLight::coord(twilight_data_nf$tFirst,
        twilight_data_nf$tSecond,
        twilight_data_nf$type,
        degElevation = get_sun_angle_seq(twilight_data_nf, light_data_calibration),
        note = FALSE
    )

    postab <- data.frame(twilight_data_nf, latlon)
    print(paste("Calculated", nrow(postab), "positions."))

    # Temp smoothing x 2---------------------
    posdata <- double_smoothing(df = postab, sun = get_sun_angle_seq(postab, light_data_calibration))
    filtering$twilight_mismatch <- nrow(postab) - nrow(posdata)
    print("Applied double smoothing to positions.")
    # Equinox-filter ---------------------
    # CHANGE BEHAVIOUR FOR WINTER/SUMMER MODE ALWAYS TRUE OR FALSE
    if (type == "main") {
        posdata <- equinox_filter(posdata, posdata$lat_smooth2, light_data_calibration, logger_colony_info)
    } else {
        posdata$eqfilter <- TRUE
    }
    print(paste(sum(!posdata$eqfilter, na.rm = TRUE), "positions marked as during equinox periods."))
    print("Applied equinox filter to positions.")

    # Speed filtering --------------------
    posdata_sf <- speed_filter(posdata, logger_filter$speed)
    filtering$removed_speed <- nrow(posdata) - nrow(posdata_sf)
    print(paste("Removed", filtering$removed_speed, "positions during speed filtering."))

    # BOUNDARY BOX---------------------------------
    posdata_bb <- bounding_box_filter(posdata_sf, light_data_calibration, logger_filter)
    filtering$removed_boundbox <- nrow(posdata_sf) - nrow(posdata_bb)
    print(paste("Removed", filtering$removed_boundbox, "positions outside boundary box."))

    # ARGOSFILTER---------------------------------
    posdata_argos <- argos_filter(posdata_bb, light_data_calibration, logger_colony_info, logger_filter)
    filtering$removed_argos <- nrow(posdata_bb) - nrow(posdata_argos)
    print(paste("Removed", filtering$removed_argos, "positions during ARGOS filtering."))

    # LOESS FILTER---------------------------------
    posdata_loess <- loess_filter(posdata_argos, logger_filter)
    filtering$removed_loess <- nrow(posdata_argos) - nrow(posdata_loess)
    print(paste("Removed", filtering$removed_loess, "positions during loess filtering."))

    # Exclude midnight sun period at colony-----------
    if (logger_filter$midnightsun_removal) {
        posdata_ms <- aut_midnight_sun_removal(df = posdata_loess)
    } else {
        posdata_ms <- posdata_loess
    }

    filtering$removed_midnight_sun <- nrow(posdata_loess) - nrow(posdata_ms)
    print(paste("Removed", filtering$removed_midnight_sun, "positions during midnight sun removal."))

    # END OF FILTERING---------------------------------

    posdata_ds <- double_smoothing(df = posdata_ms, sun = get_sun_angle_seq(posdata_ms, light_data_calibration))
    print("Applied double smoothing to final positions.")
    filter_df <- logger_filter
    filter_df$months_breeding_start <- filter_df$months_breeding[1]
    filter_df$months_breeding_end <- filter_df$months_breeding[length(filter_df$months_breeding)]
    filter_df$months_breeding <- NULL
    boundary.box <- logger_filter$boundary.box
    filter_df$boundary.box <- NULL
    filter_df <- data.frame(filter_df)
    filter_df$boundary.box_xmin <- boundary.box["xmin"]
    filter_df$boundary.box_xmax <- boundary.box["xmax"]
    filter_df$boundary.box_ymin <- boundary.box["ymin"]
    filter_df$boundary.box_ymax <- boundary.box["ymax"]
    filter_df$species <- NULL
    filter_df$colony <- NULL
    filter_df$logger_id <- NULL
    filter_df$years_tracked <- NULL

    posdata_export <- data.frame(posdata_ds, light_data_calibration, logger_colony_info, filter_df)
    posdata_export$sun_angle <- get_sun_angle_seq(posdata_export, light_data_calibration)
    posdata_export$logger_id_year <- paste(posdata_export$logger_id[1], strsplit(posdata_export$total_years_tracked[1], "_")[[1]][2], sep = "_")
    posdata_export$script_version <- 3.0
    posdata_export <- dplyr::select(
        posdata_export,
        logger_id,
        logger_id_year,
        total_years_tracked,
        logger_model,
        start_datetime,
        end_datetime,
        year_tracked,
        species,
        date_time,
        sun_angle,
        eqfilter,
        lon_raw = lon,
        lat_raw = lat,
        lon_smooth1,
        lat_smooth1,
        lon = lon_smooth2,
        lat = lat_smooth2,
        tFirst,
        tSecond,
        type,
        colony,
        col_lat,
        col_lon,
        sun_angle_start,
        sun_angle_end,
        light_threshold,
        noon_filter,
        daylength_filter,
        speed,
        coast_to_land,
        coast_to_sea,
        loess_filter_k,
        months_breeding_start,
        months_breeding_end,
        boundary.box_xmin,
        boundary.box_xmax,
        boundary.box_ymin,
        boundary.box_ymax,
        analyzer
    )

    if (!is.null(logger_extra_metadata)) {
        posdata_export <- dplyr::left_join(posdata_export, logger_extra_metadata, by = "logger_id")
    }

    if (show_filter_plots) {
        print("Exporting position change plots...")
        pos_change_plot(
            filtering,
            posdata_export,
            twilight_data,
            twilight_data_tc,
            twilight_data_mt,
            twilight_data_nf,
            posdata_sf,
            posdata_bb,
            posdata_argos,
            posdata_loess,
            posdata_ms,
            light_data_calibration,
            logger_colony_info,
            logger_filter,
            logger_id_year,
            plotting_dir
        )
    }

    if (!calibration_mode) {
        if (type == "main") {
            seasonal_calibration_result <- tryCatch(
                {
                    handle_seasonal_calibration(
                        posdata_export,
                        light_data,
                        filtering,
                        light_data_calibration,
                        logger_filter,
                        logger_colony_info,
                        logger_extra_metadata
                    )
                },
                error = function(e) {
                    print(paste("Error during seasonal calibration:", e$message))
                    print("Proceeding without seasonal adjustments.")
                    return(list(
                        posdata_export = posdata_export,
                        filtering = filtering
                    ))
                }
            )

            posdata_export_ws_af <- seasonal_calibration_result$posdata_export
            filtering <- seasonal_calibration_result$filtering

            # Run land mask filter
            print("Applying land mask filter...")
            land_mask <- land_mask(
                lon = posdata_export_ws_af$lon, lat = posdata_export_ws_af$lat,
                coast_to_land = logger_filter$coast_to_land,
                coast_to_sea = logger_filter$coast_to_sea,
                eqfilter = posdata_export_ws_af$eqfilter
            )
            filtering$removed_landmask_seasonal <- sum(land_mask)
            print(paste("Removed", filtering$removed_landmask_seasonal, "positions during land mask filtering of positions."))
            posdata_export_final <- posdata_export_ws_af[!land_mask, ]

            posdata_export_final$lat_smooth2 <- NULL
            posdata_export_final$lon_smooth2 <- NULL

            return(list(
                twilight_estimates = twilight_data_nf,
                posdata_export = posdata_export_final,
                filtering = filtering
            ))
        } else {
            return(posdata_export)
        }
    } else {
        if (type != "main") {
            print("Comparing sun angles to main calibration...")
            new_sun_angles <- compare_sun_angle(prev_posdata_export, posdata_export, type)
            light_data_calibration$sun_angle_start <- new_sun_angles$sun_angle_start
            light_data_calibration$sun_angle_end <- new_sun_angles$sun_angle_end
        } else {
            print("Exporting calibration plots...")
            make_calibration_plots(
                posdata_export,
                sun_angle_seq = seatrackRgls::sun_angles$general,
                light_data_calibration = light_data_calibration,
                logger_filter = logger_filter,
                logger_id_year = logger_id_year,
                plotting_dir = plotting_dir
            )
            light_data_calibration$sun_angle_start <- NA
            light_data_calibration$sun_angle_end <- NA
        }
        return(light_data_calibration)
    }
}

handle_seasonal_calibration <- function(
    posdata_export, light_data, filtering, light_data_calibration, logger_filter, logger_colony_info, logger_extra_metadata = NULL) {
    print("Running seasonal calibration...")
    winter_calibration <- process_light_position(
        light_data, light_data_calibration, logger_filter, logger_colony_info, logger_extra_metadata,
        show_filter_plots = FALSE,
        plotting_dir = NULL,
        prev_posdata_export = posdata_export,
        type = "winter",
        calibration_mode = TRUE
    )
    summer_calibration <- process_light_position(
        light_data, light_data_calibration, logger_filter, logger_colony_info, logger_extra_metadata,
        show_filter_plots = FALSE,
        plotting_dir = NULL,
        prev_posdata_export = posdata_export,
        type = "summer",
        calibration_mode = TRUE
    )
    print(paste("Winter sun angles:", winter_calibration$sun_angle_start, winter_calibration$sun_angle_end))
    print(paste("Summer sun angles:", summer_calibration$sun_angle_start, summer_calibration$sun_angle_end))
    print("Recalculating positions with seasonal calibrations...")
    winter_pos <- process_light_position(
        light_data, winter_calibration, logger_filter, logger_colony_info, logger_extra_metadata,
        show_filter_plots = FALSE,
        plotting_dir = NULL,
        prev_posdata_export = posdata_export,
        type = "winter",
        calibration_mode = FALSE
    )
    summer_pos <- process_light_position(
        light_data, summer_calibration, logger_filter, logger_colony_info, logger_extra_metadata,
        show_filter_plots = FALSE,
        plotting_dir = NULL,
        prev_posdata_export = posdata_export,
        type = "summer",
        calibration_mode = FALSE
    )
    print(paste("Calculated", nrow(winter_pos), "winter positions."))
    print(paste("Calculated", nrow(summer_pos), "summer positions."))
    print("Combining seasonal positions...")
    # if (logger_colony_info$col_lat > 0) {
    #     summer_days <- c(98:247)
    #     winter_days <- c(290:366, 1:50)
    # } else {
    #     summer_days <- c(285:366, 1:60)
    #     winter_days <- c(110:210)
    # }
    # if (light_data_calibration$species %in% c("Arctic tern", "arctic tern", "Sterna paradisaea", "sterna paradisaea", "ARTE")) {
    #     summer_days <- c(285:366, 1:60, 110:247)
    #     winter_days <- c(285:366, 1:60, 110:247)
    # }
    # add_to_summer <- summer_pos[lubridate::yday(summer_pos$date_time) %in% summer_days, ]
    add_to_summer <- summer_pos[!(as.Date(summer_pos$date_time) %in% as.Date(posdata_export$date_time)), ]

    # add_to_winter <- winter_pos[lubridate::yday(winter_pos$date_time) %in% winter_days, ]
    add_to_winter <- winter_pos[!(as.Date(winter_pos$date_time) %in% as.Date(posdata_export$date_time)), ]
    posdata_export_w_s <- rbind(posdata_export, add_to_summer, add_to_winter)

    posdata_export_w_s <- posdata_export_w_s[order(posdata_export_w_s$date_time), ]
    filtering$added_seasonal_positions <- nrow(posdata_export_w_s) - nrow(posdata_export)
    print(paste("Added", filtering$added_seasonal_positions, "positions from seasonal calibrations."))
    print(paste("Combined seasonal positions, total positions now:", nrow(posdata_export_w_s)))

    # These functions expect a lat_smooth2 and lon_smooth2 columns
    posdata_export_w_s$lat_smooth2 <- posdata_export_w_s$lat
    posdata_export_w_s$lon_smooth2 <- posdata_export_w_s$lon

    # Run speed filter again
    print("Running speed filter on combined seasonal positions...")
    posdata_export_ws_sf <- speed_filter(posdata_export_w_s, logger_filter$speed)
    filtering$removed_speed_seasonal <- nrow(posdata_export_w_s) - nrow(posdata_export_ws_sf)
    print(paste("Removed", filtering$removed_speed_seasonal, "positions during speed filtering of combined seasonal positions."))

    # Keep this dataframe clean
    posdata_export_ws_sf$speed <- NULL
    posdata_export_ws_sf$keep <- NULL

    # Run argos filter again
    print("Running ARGOS filter on combined seasonal positions...")
    posdata_export_ws_af <- argos_filter(posdata_export_ws_sf, light_data_calibration, logger_colony_info, logger_filter)
    filtering$removed_argos_seasonal <- nrow(posdata_export_ws_sf) - nrow(posdata_export_ws_af)
    print(paste("Removed", filtering$removed_argos_seasonal, "positions during ARGOS filtering of combined seasonal positions."))

    # Keep this dataframe clean
    posdata_export_ws_af$lc <- NULL
    posdata_export_ws_af$argosfilter1 <- NULL
    posdata_export_ws_af$argosfilter2 <- NULL

    return(list(
        posdata_export = posdata_export_ws_af,
        filtering = filtering
    ))
}
