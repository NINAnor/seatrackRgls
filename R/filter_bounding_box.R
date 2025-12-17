#' Bounding Box Filter for Position Data
#'
#' Filters position data based on a defined bounding box, removing positions outside the specified latitude and longitude limits.
#' @param posdata A data frame containing position data with columns `lon`, `lat`, and `tFirst`.
#' @param light_data_calibration A list containing light data calibration parameters.
#' @param logger_filter A list containing logger filter parameters, including default boundary box limits.
#' @return A filtered data frame with positions outside the bounding box removed.
#' @concept filtering
#' @export
bounding_box_filter <- function(posdata, light_data_calibration, logger_filter){
    # latitudes only filtered outside the equinox

        # obsolete now we use custom filter settings.
        if(is.null(light_data_calibration$bbox_xmin)){
            boundary.box <- logger_filter$boundary.box
        }else{
            boundary.box <- sf::st_bbox(c(
                xmin = light_data_calibration$bbox_xmin,
                ymin = light_data_calibration$bbox_ymin,
                xmax = light_data_calibration$bbox_xmax,
                ymax = light_data_calibration$bbox_ymax
            ))
        }

        # adapt coordinates to Pacific birds
        if (boundary.box$xmin > 180 || boundary.box$xmax > 180) posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] <- posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] + 360

        posdata$lon_filter <- TRUE
        posdata$lat_filter <- TRUE

        posdata$lon_filter[posdata$lon > boundary.box$xmax] <- FALSE
        posdata$lon_filter[posdata$lon < boundary.box$xmin] <- FALSE
        posdata$lat_filter[posdata$lat < boundary.box$ymin] <- FALSE
        posdata$lat_filter[posdata$lat > boundary.box$ymax] <- FALSE

        posdata$tFirst[!posdata$lon_filter] <- NA
        posdata$tFirst[!posdata$lat_filter & posdata$eqfilter] <- NA

        posdata <- posdata[!is.na(posdata$tFirst),]
        posdata <- posdata[complete.cases(posdata),]

        posdata$lon_filter <- NULL
        posdata$lat_filter <- NULL

        if (boundary.box$xmin > 180 || boundary.box$xmax > 180) posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] <- posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] - 360

        return (posdata)

}