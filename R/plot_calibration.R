setup_dir <- function(plotting_dir) {
    if (!is.null(plotting_dir)) {
        sun_plot_dir <- file.path(plotting_dir, "sun_calib")
    } else {
        sun_plot_dir <- NULL
    }

    if (!is.null(sun_plot_dir) && !dir.exists(sun_plot_dir)) {
        dir.create(sun_plot_dir, recursive = TRUE)
    }
    return(sun_plot_dir)
}

lat_time_plot <- function(posdata_export, sun_angle_seq = sun_angles$general, light_data_calibration, logger_filter, logger_id_year, plotting_dir = NULL) {
    sun_plot_dir <- setup_dir(plotting_dir)

    adjust_lon <- get_adjust_lon(posdata_export)
    pos_plot <- apply_adjust_lon(posdata_export, adjust_lon)

    if (!is.null(sun_plot_dir)) {
        tiff(
            filename = file.path(sun_plot_dir, paste0(logger_id_year, "_lat_vs_time", ".tiff")), height = 20, width = 18, units = "cm",
            compression = "lzw", res = 600
        )
    }

    latmax <- max(pos_plot$lat[pos_plot$eqfilter]) + 5
    latmin <- min(pos_plot$lat[pos_plot$eqfilter]) - 5
    if (latmin %in% Inf) latmin <- boundary.box$ymin
    if (latmax %in% -Inf) latmax <- boundary.box$ymax

    par(mfrow = c(4, 3))
    par(mar = c(1.5, 2, 1.5, 1.5) + 0.1)

    for (i in 1:(length(sun_angle_seq))) {
        latlon.sun_coords <- GeoLight::coord(pos_plot$tFirst, pos_plot$tSecond, pos_plot$type, degElevation = sun_angle_seq[i], note = F)
        latlon.sun_coords <- data.frame(latlon.sun_coords)
        latlon.sun <- pos_plot
        latlon.sun$lat <- latlon.sun_coords$lat
        latlon.sun$lon <- latlon.sun_coords$lon


        latlon.sun$eqfilter <- NA
        latlon.sun$eqfilter[latlon.sun$tFirst %in% pos_plot$tFirst] <- pos_plot$eqfilter[latlon.sun$tFirst %in% pos_plot$tFirst]

        plot(latlon.sun$tFirst, latlon.sun$lat, col = "white", ylim = c(latmin, latmax), ylab = "lat", xlab = NULL, main = paste("sun", sun_angle_seq[i], sep = ""))
        abline(v = latlon.sun$tFirst[latlon.sun$eqfilter == 0], col = "light grey")
        # lines(latlon.sun$tFirst,latlon.sun[,5])
        latlon.sun$lat_roll4 <- NA
        latlon.sun$lat_roll4[4:(nrow(latlon.sun) - 4)] <- zoo::rollmean(latlon.sun$lat, 8)
        lines(latlon.sun$tFirst, latlon.sun$lat_roll4, lwd = 1.2)
        abline(h = posdata_export$col_lat[1], lty = 2)
    }

    plot(0, 0, ylim = c(25, 85), xlim = c(2, 10), axes = FALSE)
    text(6, 70, light_data_calibration$logger_id, cex = 1.5)
    text(6, 55, paste(light_data_calibration$logger_model, light_data_calibration$year_tracked, sep = " "), cex = 1.25)
    text(6, 45, light_data_calibration$colony, cex = 1.25)
    text(6, 35, light_data_calibration$species, cex = 1.25)

    if (!is.null(sun_plot_dir)) {
        dev.off()
    }
}

calibration_maps <- function(posdata_export, sun_angle_seq = sun_angles$general, light_data_calibration, logger_filter, logger_id_year, plotting_dir = NULL) {
    sun_plot_dir <- setup_dir(plotting_dir)

    map_ver <- get_map_ver(posdata_export)
    adjust_lon <- get_adjust_lon(posdata_export)
    pos_plot <- apply_adjust_lon(posdata_export, adjust_lon)

    boundary.box <- get_boundary_box(light_data_calibration, logger_filter)

    latmax <- max(pos_plot$lat[pos_plot$eqfilter == 1]) + 5
    latmin <- min(pos_plot$lat[pos_plot$eqfilter == 1]) - 5
    if (latmin %in% Inf) latmin <- boundary.box$ymin
    if (latmax %in% -Inf) latmax <- boundary.box$ymax

    lonmax <- max(pos_plot$lon) + 5
    lonmin <- min(pos_plot$lon) - 5
    if (lonmin %in% Inf) lonmin <- boundary.box$xmin + (adjust_lon / 2)
    if (lonmax %in% -Inf) lonmax <- boundary.box$xmax + (adjust_lon / 2)


    if (!is.null(sun_plot_dir)) {
        tiff(
            filename = file.path(sun_plot_dir, paste0(logger_id_year, "_sunmaps", ".tiff")), height = 30, width = 25.5, units = "cm",
            compression = "lzw", res = 700
        )
    }
    par(mfrow = c(4, 3))
    par(mar = c(2, 3, 2, 2) + 0.1)
    par(oma = c(2, 2, 2, 2))

    col_lon <- posdata_export$col_lon[1]
    col_lat <- posdata_export$col_lat[1]

    for (t in 1:(length(sun_angle_seq))) {
        for_calib <- pos_plot[, c("tFirst", "tSecond", "type", "eqfilter")]
        latlon.sun <- double_smoothing(df = for_calib, sun = sun_angle_seq[t])
        latlon.sun <- na.omit(latlon.sun)
        latlon.sun$lon_smooth2[latlon.sun$lon_smooth2 < 0] <- latlon.sun$lon_smooth2[latlon.sun$lon_smooth2 < 0] + adjust_lon
        plot(NA, xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), xaxt = "n", yaxt = "n", xlab = "", ylab = "", main = paste("sun", sun_angle_seq[t]))
        maps::map(map_ver, fill = TRUE, col = "grey90", bg = "white", xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), main = paste("sun", sun_angle_seq[t], sep = ""), add = T)
        maps::map.axes()
        lines(latlon.sun$lon_smooth2, latlon.sun$lat_smooth2, lwd = 0.8, col = "grey")
        lines(latlon.sun$lon_smooth2[latlon.sun$eqfilter], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1], lwd = 1, col = "red")
        points(latlon.sun$lon_smooth2, latlon.sun$lat_smooth2, cex = 0.4, pch = 16, col = "black")
        points(latlon.sun$lon_smooth2[latlon.sun$eqfilter & month(latlon.sun$date_time) %in% c(8, 9, 10)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(8, 9, 10)], cex = 0.6, pch = 16, col = "steelblue")
        points(latlon.sun$lon_smooth2[latlon.sun$eqfilter & month(latlon.sun$date_time) %in% c(11, 12, 1)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(11, 12, 1)], cex = 0.6, pch = 16, col = "firebrick")
        points(latlon.sun$lon_smooth2[latlon.sun$eqfilter & month(latlon.sun$date_time) %in% c(2, 3, 4)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(2, 3, 4)], cex = 0.6, pch = 16, col = "orange")
        points(latlon.sun$lon_smooth2[latlon.sun$eqfilter & month(latlon.sun$date_time) %in% c(5, 6, 7)], latlon.sun$lat_smooth2[latlon.sun$eqfilter == 1 & month(latlon.sun$date_time) %in% c(5, 6, 7)], cex = 0.6, pch = 16, col = "forestgreen")
        if (col_lon < 0) points(col_lon + adjust_lon, col_lat, cex = 2, pch = 19, col = "blue")
        if (col_lon > 0) points(col_lon, col_lat, cex = 2, pch = 19, col = "blue")
    }

    plot(0, 0, ylim = c(25, 85), xlim = c(2, 10), axes = FALSE)
    text(6, 48, light_data_calibration$logger_id, cex = 1.5)
    text(6, 40, paste(light_data_calibration$logger_model, light_data_calibration$year_tracked, sep = " "), cex = 1.25)
    text(6, 35, light_data_calibration$colony, cex = 1.25)
    text(6, 30, light_data_calibration$species, cex = 1.25)
    legend("top",
        title = "Time of the year",
        c("Equinox", "Aug to Oct", "Nov to Jan", "Feb to Apr", "May to Jul"), fill = c("black", "steelblue", "firebrick", "orange", "forestgreen"), horiz = FALSE, cex = 1.1
    )


    if (!is.null(sun_plot_dir)) {
        dev.off()
    }
}

make_calibration_plots <- function(posdata_export, sun_angle_seq = sun_angles$general, light_data_calibration, logger_filter, logger_id_year, plotting_dir = NULL) {
    lat_time_plot(posdata_export, sun_angle_seq, light_data_calibration, logger_filter, logger_id_year, plotting_dir)
    calibration_maps(posdata_export, sun_angle_seq, light_data_calibration, logger_filter, logger_id_year, plotting_dir)
}
