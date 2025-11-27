loess_filter <- function(posdata, logger_filter) {
    # Track must have a minimum length of 45 days
    if (difftime(posdata$tFirst[length(posdata$tFirst)], posdata$tFirst[1], units = "days") > 45) {
        print("Applying loess filter to position data")
        loess_filter <- GeoLight::loessFilter(posdata$tFirst, posdata$tSecond, posdata$type, k = logger_filter$loess_filter_k, plot = FALSE)
        posdata_2 <- posdata[loess_filter, ]
        return(posdata_2)
    } else {
        print("Track too short to apply loess filter")
        return(posdata)
    }
}
