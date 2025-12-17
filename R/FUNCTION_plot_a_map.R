#' Produce a Quick Map
#'
#'
#' @param df data frame with lat, lon, eqfilter, date_time, col_lat, col_lon, colony
#' @return Plotted map.
#' @concept plotting
#' @export
plot_a_map <- function(df) {
   par(mfrow = c(1, 1))

   # center map on the Pacific or the Atlantic?
   map_ver <- "world"
   adjust_lon <- 0
   if ((nrow(df[lubridate::month(df$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4) & abs(df$lon) > 90, ]) / nrow(df[lubridate::month(df$date_time) %in% c(8, 9, 10, 11, 12, 1, 2, 3, 4), ])) > 0.5) adjust_lon <- 360
   if (adjust_lon == 360) map_ver <- "world2"

   df$lon[df$lon < 0] <- df$lon[df$lon < 0] + adjust_lon
   df$col_lon[df$col_lon < 0] <- df$col_lon[df$col_lon < 0] + adjust_lon

   lonmax <- max(df$lon) + 10
   lonmin <- min(df$lon) - 10

   latmax <- max(df$lat[df$eqfilter == 1]) + 10
   latmin <- min(df$lat[df$eqfilter == 1]) - 10

   plot(NA,
      xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), xaxt = "n", yaxt = "n", xlab = "",
      ylab = ""
   )
   maps::map(map_ver, fill = TRUE, col = "lightgray", bg = "white", xlim = c(lonmin, lonmax), ylim = c(latmin, latmax), add = T)
   mtext(ifelse(sum(names(args) %in% "xlab") == 1, args$xlab, "Longitude"), side = 1, line = 2.2, font = 3)
   mtext(ifelse(sum(names(args) %in% "ylab") == 1, args$ylab, "Latitude"), side = 2, line = 2.5, font = 3)
   maps::map.axes()
   mtext(ifelse(sum(names(args) %in% "main") == 1, args$main, ""), line = 0.6, cex = 1.2)
   lines(df$lon, df$lat, col = "grey", lwd = 0.6)
   points(df$lon[df$eqfilter == 1 & month(df$date_time) %in% c(5, 6, 7)], df$lat[df$eqfilter == 1 & month(df$date_time) %in% c(5, 6, 7)], cex = 0.6, pch = 16, col = "forestgreen")
   points(df$lon[df$eqfilter == 1 & month(df$date_time) %in% c(8, 9, 10)], df$lat[df$eqfilter == 1 & month(df$date_time) %in% c(8, 9, 10)], cex = 0.6, pch = 16, col = "steelblue")
   points(df$lon[df$eqfilter == 1 & month(df$date_time) %in% c(11, 12, 1)], df$lat[df$eqfilter == 1 & month(df$date_time) %in% c(11, 12, 1)], cex = 0.6, pch = 16, col = "firebrick")
   points(df$lon[df$eqfilter == 1 & month(df$date_time) %in% c(2, 3, 4)], df$lat[df$eqfilter == 1 & month(df$date_time) %in% c(2, 3, 4)], cex = 0.6, pch = 16, col = "orange")
   points(df$lon[df$eqfilter == 1 & df$light_threshold >= 20], df$lat[df$eqfilter == 1 & df$light_threshold >= 20], cex = 0.6, pch = 21, bg = "white", col = "black")
   points(df$lon[df$eqfilter == 1 & df$light_threshold < median(df$light_threshold)], df$lat[df$eqfilter == 1 & df$light_threshold < median(df$light_threshold)], cex = 0.6, pch = 21, col = "violet")

   points(df$lon[df$eqfilter == 0], df$lat[df$eqfilter == 0], cex = 0.3, pch = 16, col = "black")
   points(df$col_lon[1], df$col_lat[1], cex = 2, pch = 19, col = "red")
   legend("bottomright",
      inset = .01, title = "Time of the year",
      c("Equinox", "May to Jul", "Aug to Oct", "Nov to Jan", "Feb to Apr", "summer_threshold", "winter_threshold"), fill = c("black", "forestgreen", "steelblue", "firebrick", "orange", "white", "violet"), horiz = FALSE, cex = 0.5
   )
   mtext(paste(df$logger_id_year[1], df$year_tracked[1], df$species[1], df$colony[1]), line = 0.6)
}
