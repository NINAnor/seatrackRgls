bounding_box_filter <- function(posdata, light_data_calibration, logger_filter){
    # latitudes only filtered outside the equinox

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
        if (boundary.box[2] > 180 || boundary.box[1] > 180) posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] <- posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] + 360

        posdata$lon_filter <- TRUE
        posdata$lat_filter <- TRUE

        posdata$lon_filter[posdata$lon > boundary.box[2]] <- FALSE
        posdata$lon_filter[posdata$lon < boundary.box[1]] <- FALSE
        posdata$lat_filter[posdata$lat < boundary.box[3]] <- FALSE
        posdata$lat_filter[posdata$lat > boundary.box[4]] <- FALSE

        posdata$tFirst[!posdata$lon_filter] <- NA
        posdata$tFirst[!posdata$lat_filter & posdata$eqfilter] <- NA

        posdata <- posdata[!is.na(posdata$tFirst),]
        posdata <- posdata[complete.cases(posdata),]

        posdata$lon_filter <- NULL
        posdata$lat_filter <- NULL

        if (boundary.box[2] > 180 || boundary.box[1] > 180) posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] <- posdata$lon[!is.na(posdata$lon) & posdata$lon < 0] - 360

        return (posdata)

}