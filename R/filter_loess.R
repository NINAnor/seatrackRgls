#' Loess Filter for Position Data
#'
#' Applies a loess filter to position data if the track length exceeds 45 days.
#'
#' @param posdata A data frame containing position data with columns `tFirst`, `tSecond`, and `type`.
#' @param logger_filter A list containing logger filter parameters, including `loess_filter_k
#' @return A data frame with loess-filtered positions if applicable; otherwise, the original data frame.
#' @concept filtering
#' @export
loess_filter <- function(posdata, logger_filter) {
    # Track must have a minimum length of 45 days
    if (difftime(posdata$tFirst[length(posdata$tFirst)], posdata$tFirst[1], units = "days") > 45) {
        print("Applying loess filter to position data")
        posdata_2 <- tryCatch(
            {
                loess_filter <- GeoLight::loessFilter(posdata$tFirst, posdata$tSecond, posdata$type, k = logger_filter$loess_filter_k, plot = FALSE)
                return(posdata[loess_filter, ])
            },
            error = function(e) {
                print(paste("Error in loess filter:", e))
                return(posdata)
            }
        )
        return(posdata_2)
    } else {
        print("Track too short to apply loess filter")
        return(posdata)
    }
}
