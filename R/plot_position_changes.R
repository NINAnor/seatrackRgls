pos_change_plot <- function(
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
    plotting_dir = NULL) {
    if (!is.null(plotting_dir)) {
        filter_plots_dir <- file.path(plotting_dir, "filter_plots")
        if (!is.null(filter_plots_dir) && !dir.exists(filter_plots_dir)) {
            dir.create(filter_plots_dir, recursive = TRUE)
        }

        tiff(
            filename = file.path(filter_plots_dir, paste(logger_id_year, "_6_speed_bbox_angleanddist_loess_end", ".tiff", sep = "")),
            height = 22, width = 20, units = "cm", compression = "lzw", res = 600
        )
    }

    # Set up parameters
    par(mfrow = c(4, 2))
    par(mar = c(2, 3, 2, 2) + 0.1)
    par(oma = c(2, 2, 2, 2))

    map_ver <- get_map_ver(posdata_export)
    adjust_lon <- get_adjust_lon(posdata_export)

    boundary.box <- get_boundary_box(light_data_calibration, logger_filter)

    xlim <- c(boundary.box$xmin, boundary.box$xmax)
    ylim <- c(boundary.box$ymin, boundary.box$ymax)

    if (adjust_lon == 360 & as.data.frame(boundary.box)[2, ] <= 180) {
        xlim <- xlim + 180
    }

    # twilight_data
    twilight_data_coords <- GeoLight::coord(
        twilight_data$tFirst,
        twilight_data$tSecond,
        twilight_data$type,
        degElevation = get_sun_angle_seq(twilight_data, light_data_calibration),
        note = FALSE
    )
    twilight_data_coords <- data.frame(twilight_data, twilight_data_coords)
    twilight_data_coords <- equinox_filter(twilight_data_coords, twilight_data_coords$lat, light_data_calibration, logger_colony_info)
    twilight_data_coords <- apply_adjust_lon(twilight_data_coords, adjust_lon)

    # twilight_data_tc
    twilight_data_tc_coords <- GeoLight::coord(
        twilight_data_tc$tFirst,
        twilight_data_tc$tSecond,
        twilight_data_tc$type,
        degElevation = get_sun_angle_seq(twilight_data_tc, light_data_calibration),
        note = FALSE
    )
    twilight_data_tc_coords <- data.frame(twilight_data_tc, twilight_data_tc_coords)
    twilight_data_tc_coords <- equinox_filter(twilight_data_tc_coords, twilight_data_tc_coords$lat, light_data_calibration, logger_colony_info)
    twilight_data_tc_coords <- apply_adjust_lon(twilight_data_tc_coords, adjust_lon)

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = twilight_data_coords[twilight_data_coords$eqfilter, ],
        after_pos = twilight_data_tc_coords[twilight_data_tc_coords$eqfilter, ],
        title = "Twilight cleanup",
        n_pos_removed = filtering$removed_twilight_cleanup
    )

    # twilight_data_mt
    twilight_data_mt_coords <- GeoLight::coord(
        twilight_data_mt$tFirst,
        twilight_data_mt$tSecond,
        twilight_data_mt$type,
        degElevation = get_sun_angle_seq(twilight_data_mt, light_data_calibration),
        note = FALSE
    )
    twilight_data_mt_coords <- data.frame(twilight_data_mt, twilight_data_mt_coords)
    twilight_data_mt_coords <- equinox_filter(twilight_data_mt_coords, twilight_data_mt_coords$lat, light_data_calibration, logger_colony_info)
    twilight_data_mt_coords <- apply_adjust_lon(twilight_data_mt_coords, adjust_lon)

    # twilight_data_nf
    twilight_data_nf_coords <- GeoLight::coord(
        twilight_data_nf$tFirst,
        twilight_data_nf$tSecond,
        twilight_data_nf$type,
        degElevation = get_sun_angle_seq(twilight_data_nf, light_data_calibration),
        note = FALSE
    )
    twilight_data_nf_coords <- data.frame(twilight_data_nf, twilight_data_nf_coords)
    twilight_data_nf_coords <- equinox_filter(twilight_data_nf_coords, twilight_data_nf_coords$lat, light_data_calibration, logger_colony_info)
    twilight_data_nf_coords <- apply_adjust_lon(twilight_data_nf_coords, adjust_lon)

    before_modified_pos <- twilight_data_tc_coords[!(twilight_data_tc$tFirst %in% twilight_data_mt$tFirst), ]
    after_modified_pos <- twilight_data_mt_coords[!(twilight_data_tc$tFirst %in% twilight_data_mt$tFirst), ]

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = twilight_data_mt_coords[twilight_data_mt_coords$eqfilter, ],
        after_pos = twilight_data_nf_coords[twilight_data_nf_coords$eqfilter, ],
        before_modified_pos = before_modified_pos[before_modified_pos$eqfilter, ],
        after_modified_pos = after_modified_pos[after_modified_pos$eqfilter, ],
        title = "Edit and filter twilights",
        n_pos_removed = filtering$removed_daylengthfilter + filtering$removed_noonfilter,
        n_pos_changed = filtering$moved_twilight_positions
    )

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = twilight_data_nf_coords[twilight_data_nf_coords$eqfilter, ],
        after_pos = posdata_sf[posdata_sf$eqfilter, ],
        title = "Speed and twighlight mismatch filter",
        n_pos_removed = filtering$removed_speed + filtering$twilight_mismatch,
    )

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = posdata_sf[posdata_sf$eqfilter, ],
        after_pos = posdata_bb[posdata_bb$eqfilter, ],
        title = "Distribution filter",
        n_pos_removed = filtering$removed_boundbox,
        boundary.box = boundary.box
    )

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = posdata_bb[posdata_bb$eqfilter, ],
        after_pos = posdata_argos[posdata_argos$eqfilter, ],
        title = "ARGOS filter",
        n_pos_removed = filtering$removed_argos,
    )

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = posdata_argos[posdata_argos$eqfilter, ],
        after_pos = posdata_loess[posdata_loess$eqfilter, ],
        title = "Loess filter",
        n_pos_removed = filtering$removed_loess,
    )

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = posdata_loess[posdata_loess$eqfilter, ],
        after_pos = posdata_ms[posdata_ms$eqfilter, ],
        title = "Midnight sun filter",
        n_pos_removed = filtering$removed_midnight_sun,
    )

    pos_change_subplot(map_ver, xlim, ylim,
        before_pos = posdata_ms[posdata_ms$eqfilter, ],
        after_pos = posdata_export[posdata_export$eqfilter, ],
        title = "Smoothed positions"
    )

    if (!is.null(plotting_dir)) {
        dev.off()
    }
}



pos_change_subplot <- function(map_ver, xlim, ylim, before_pos, after_pos, before_col = "darkgrey", before_modified_pos = NULL, after_modified_pos = NULL, boundary.box = NULL, title = "", n_pos_removed = NA, n_pos_changed = NA) {
    # Set up background
    plot(NA, xlim = xlim, ylim = ylim, xaxt = "n", yaxt = "n", xlab = "", ylab = "")

    maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = xlim, ylim = ylim, add = T)
    maps::map.axes()

    # TRACK LINE
    lines(cbind(before_pos$lon, before_pos$lat), col = before_col, lwd = 0.5)

    if (!is.na(n_pos_removed)) {
        # PLOT ALL BEFORE POINTS
        points(cbind(before_pos$lon, before_pos$lat), cex = 1, pch = 20, col = before_col, lwd = 1)

        # PLOT AFTER POINTS
        points(cbind(after_pos$lon, after_pos$lat), cex = 1, pch = 20, col = "firebrick", lwd = 1)
    } else {
        lines(cbind(after_pos$lon, after_pos$lat), col = "firebrick", lwd = 0.5)
    }


    if (!is.null(before_modified_pos)) {
        points(cbind(before_modified_pos$lon, before_modified_pos$lat), cex = 1, pch = 13, col = "black", lwd = 1)
    }
    if (!is.null(after_modified_pos)) {
        points(cbind(after_modified_pos$lon, after_modified_pos$lat), cex = 1, pch = 21, col = "black", bg = "firebrick", lwd = 1)
    }

    if (!is.na(n_pos_removed)) {
        before_removed <- before_pos[!before_pos$tFirst %in% after_pos$tFirst, ]

        # PLOT BEFORE POINTS THAT ARE NOT PART OF AFTER POINTS
        points(cbind(before_removed$lon, before_removed$lat), cex = 1, pch = 20, col = "cornflowerblue", lwd = 1)
    }

    if (!is.null(boundary.box)) {
        rect(boundary.box$xmin, boundary.box$ymin, boundary.box$xmax, boundary.box$ymax, border = "orange")
    }

    if (!is.na(n_pos_removed)) {
        subtitle_string <- paste0("n pos removed: ", n_pos_removed)
    } else {
        subtitle_string <- paste0("n pos: ", nrow(after_modified_pos))
    }

    if (!is.na(n_pos_changed)) {
        subtitle_string <- paste0("n pos changed: ", n_pos_changed, ", ", subtitle_string)
    }

    mtext(title, side = 3, line = 1.3, cex = 0.7)
    mtext(subtitle_string, side = 3, line = 0.3, cex = 0.6, padj = 0)
}
