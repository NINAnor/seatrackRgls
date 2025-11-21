#' Process all lightposition files in a folder
#'
#' WIP
#'
#' @param import_directory
#' @param calibration_data
#' @param filter_list
#' @param all_colony_info
#' @param extra_metadata
process_folder <- function(import_directory, calibration_data, filter_list = seatrack_settings_list, all_colony_info, extra_metadata, show_filter_plots = FALSE, filter_plots_dir = NULL) {
    print("Scan import directory for files...")
    all_files <- list.files(import_directory)
    all_files_split <- strsplit(all_files, "_")
    file_info_list <- lapply(all_files_split, function(x) {
        data.frame(logger_id = x[1], year_downloaded = x[2], id_year = paste(x[1], x[2]))
    })
    file_info <- do.call(rbind, file_info_list)
    all_logger_id_year <- file_info[!duplicated(file_info$id_year), ]
    print(paste("Found", nrow(all_logger_id_year), "unique logger ID + year combinations."))

    for (logger_idx in seq_len(nrow(all_logger_id_year))) {
        logger_id <- all_logger_id_year$logger_id[logger_idx]
        year <- all_logger_id_year$year_downloaded[logger_idx]
        process_logger_year(logger_id, year, import_directory, all_files, calibration_data, filter_list, all_colony_info, extra_metadata, show_filter_plots, filter_plots_dir)
    }
}

process_logger_year <- function(logger_id, year, import_directory, all_files, calibration_data, filter_list = seatrack_settings_list, all_colony_info, extra_metadata,
    show_filter_plots = FALSE, filter_plots_dir = NULL){
        print(paste("Processing logger", logger_id, "for year", year))
        files <- all_files[file_info$logger_id == logger_id & file_info$year_downloaded == year]
        filepaths <- file.path(import_directory, files)

        calibration_year <- format(calibration_data$end_datetime, "%Y")
        extra_metadata_year <- format(extra_metadata$retrieval_date, "%Y")

        logger_calibration_data <- calibration_data[calibration_data$logger_id == logger_id & calibration_year == year, ]
        logger_extra_metadata <- extra_metadata[extra_metadata$logger_id == logger_id & extra_metadata_year == year, ]
        logger_colony_info <- all_colony_info[all_colony_info$colony == logger_calibration_data$colony[1], ]
        logger_filter <- filter_list[[tolower(logger_calibration_data$species)]]
        track <- process_light_positions(filepaths, logger_calibration_data, logger_filter, logger_colony_info, logger_extra_metadata, show_filter_plots, filter_plots_dir)
}

process_light_positions <- function(filepaths, logger_calibration_data, logger_filter, logger_colony_info, logger_extra_metadata, 
    show_filter_plots = FALSE, filter_plots_dir = NULL) {
    logger_calibration_data$sun_angle_start[is.na(logger_calibration_data$sun_angle_start)] <- -3.5
    logger_calibration_data$sun_angle_end[is.na(logger_calibration_data$sun_angle_end)] <- -logger_calibration_data$sun_angle_start[is.na(logger_calibration_data$sun_angle_end)]
    logger_calibration_data$light_threshold[is.na(logger_calibration_data$light_threshold)] <- 1
    
    if(is.null(logger_calibration_data$noon_filter)){
        logger_calibration_data$noon_filter <- TRUE
    }
    if(is.null(logger_calibration_data$daylength_filter)){
        logger_calibration_data$daylength_filter <- TRUE
    }


    if(!is.null(filter_plots_dir) && !dir.exists(filter_plots_dir)) {
        dir.create(filter_plots_dir, recursive = TRUE)
    }

    print("Load light data...")
    all_light_data <- get_light_data(filepaths)
    print("Limit light data to calibration time windows...")
    light_data_split <- limit_light_data(all_light_data, logger_calibration_data)

    all_filters <- data.frame()
    
    for (i in seq_along(light_data_split)) {
        print(paste("Processing calibration window", i, "of", nrow(logger_calibration_data)))
        light_data <- light_data_split[[i]]
        light_data_calibration <- logger_calibration_data[i, ]
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

        logger_id_year <- paste0(light_data_calibration$logger_id, "_", format(light_data_calibration$start_datetime, "%Y"))

        # Estimate twilights
        print("Estimating twilights...")
        twilight_data <- 
        export_filter_plot({
            twilight_estimation(light_data, light_data_calibration)
        }, 
        show_filter_plots, 
        plot_name = "1_twilight_estimation", 
        logger_id_year = logger_id_year, 
        filter_plots_dir)
        
        filtering$nrow_twilightCalc <- nrow(twilight_data)
        print(paste("Estimated", filtering$nrow_twilightCalc, "twilights."))

        # Add sun angle etc. to twilight data
        twilight_data <- data.frame(twilight_data, light_data_calibration[, c("sun_angle_start", "sun_angle_end", "light_threshold")])

        # Removes twilights of the same type less than 22 hours apart (tc)
        print("Cleaning twilight data...")
        twilight_data_tc <- export_filter_plot({
            twilight_cleanup(
            df = twilight_data,
            breedingloc_lon = logger_colony_info$col_lon,
            breedingloc_lat = logger_colony_info$col_lat,
            months_breeding = months_breeding,
            species = light_data_calibration$species,
            sun_angle_start = light_data_calibration$sun_angle_start,
            sun_angle_end = light_data_calibration$sun_angle_end,
            show_plot = show_filter_plots
        )},
        show_filter_plots, 
        plot_name = "2_twilight_cleanup", 
        logger_id_year = logger_id_year, 
        filter_plots_dir)

        # keep track of n edited twilights/filtered positions
        filtering$removed_twilight_cleanup <- (filtering$nrow_twilightCalc - nrow(twilight_data_tc))
        print(paste("Removed", filtering$removed_twilight_cleanup, "twilights during cleanup."))

        # detect outliers and move them back to mean of their neighbors
        twilight_data_mt <- 
        export_filter_plot({
            move_twilights(
            df = twilight_data_tc,
            speed = logger_filter$speed,
            sun = sun_angle_seq(twilight_data_tc, light_data_calibration),
            show_plot = show_filter_plots
        )},
        show_filter_plots, 
        plot_name = "3_twilight_move", 
        logger_id_year = logger_id_year, 
        filter_plots_dir)

        filtering$moved_twilight_positions <- nrow(twilight_data_mt[!(twilight_data_tc$tFirst %in% twilight_data_mt$tFirst), ])
        print(paste("Moved", filtering$moved_twilight_positions, "twilight positions."))

        # daylengthfilter
        if(light_data_calibration$daylength_filter){
            twilight_data_dl <- 
            export_filter_plot({
                daylengthfilter(df = twilight_data_mt, show_plot = show_filter_plots)
            },
            show_filter_plots, 
            plot_name = "4_daylength", 
            logger_id_year = logger_id_year, 
            filter_plots_dir)
        } else{
            twilight_data_dl <- twilight_data_mt
        }
        filtering$removed_daylengthfilter <- (nrow(twilight_data_mt) - nrow(twilight_data_dl))
        print(paste("Removed", filtering$removed_daylengthfilter, "twilights during daylength filtering."))

        # noonfilter
        if(light_data_calibration$noon_filter){
            twilight_data_nf <- 
            export_filter_plot({
                noonfilter(df = twilight_data_dl, show_plot = show_filter_plots)
            },
            show_filter_plots, 
            plot_name = "5_noonfilter", 
            logger_id_year = logger_id_year, 
            filter_plots_dir)
        }else{
            twilight_data_nf <- twilight_data_dl
        }
        filtering$removed_noonfilter <- (nrow(twilight_data_dl) - nrow(twilight_data_nf))
        print("Removed", filtering$removed_noonfilter, "twilights during noon filtering.")

        # CALCULATE POSITIONS---------------------------------
        latlon <- GeoLight::coord(twilight_data_nf$tFirst, 
                                    twilight_data_nf$tSecond, 
                                    twilight_data_nf$type, 
                                    degElevation = sun_angle_seq(twilight_data_nf, light_data_calibration), 
                                    note = F)
        
        postab <- data.frame(twilight_data_nf, latlon)
        print(paste("Calculated", nrow(postab), "positions."))

        # Temp smoothing x 2---------------------
        posdata <- double_smoothing(df = postab, sun = sun_angle_seq(postab, light_data_calibration))
        print("Applied double smoothing to positions.")
        # Equinox-filter ---------------------
        if(is.null(light_data_calibration$spring_eq_start)){
            posdata$eqfilter <- assign_equinox_periods(lats = posdata$lat_smooth2, 
            dates = posdata$tFirst, 
            breedingloc_lat = logger_colony_info$col_lat, 
            sun = sun_angle_seq(posdata, light_data_calibration))
        }else{
            posdata$eqfilter <- TRUE
            posdata$eqfilter[posdata$date_time >= light_data_calibration$spring_eq_start & posdata$date_time <= light_data_calibration$spring_eq_end] <- FALSE
            posdata$eqfilter[posdata$date_time >= light_data_calibration$aut_eq_start & posdata$date_time <= light_data_calibration$aut_eq_end] <- FALSE
        }

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

    }
}

sun_angle_seq <- function (data, light_data_calibration){
    sun <- seq(light_data_calibration$sun_angle_start, light_data_calibration$sun_angle_end, length.out = nrow(data))
    return(sun)
}




