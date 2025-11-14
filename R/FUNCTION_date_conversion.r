#' convert dates for seatrackGLS
#'
#' @param date Date to be converted
#' @return Converted date in format "yyyy-mm-dd UTC"
#' @export
date_conversion <- function(date, tz = "UTC") {
   date <- as.character(date)
   output <- NULL
   # example 04.04.2013
   if (substr(date[1], 3, 3) == "." &
      substr(date[1], 6, 6) == "." &
      nchar(date[1]) == 10) {
      output <- as.POSIXct(strptime(date, "%d.%m.%Y"), tz = tz)
   }

   # example 04/04/2013
   if (is.null(output) &
      substr(date[1], 3, 3) == "/" &
      substr(date[1], 6, 6) == "/" &
      nchar(date[1]) == 10) {
      output <- as.POSIXct(strptime(date, "%d/%m/%Y"), tz = tz)
   }

   # example 04-04-2013
   if (is.null(output) &
      substr(date[1], 3, 3) == "-" &
      substr(date[1], 6, 6) == "-" &
      nchar(date[1]) == 10) {
      output <- as.POSIXct(strptime(date, "%d-%m-%Y"), tz = tz)
   }

   # example 04.04.13
   if (is.null(output) &
      substr(date[1], 3, 3) == "." &
      substr(date[1], 6, 6) == "." &
      nchar(date[1]) == 8) {
      output <- as.POSIXct(strptime(date, "%d.%m.%y"), tz = tz)
   }

   # example 04/04/13
   if (is.null(output) &
      substr(date[1], 3, 3) == "/" &
      substr(date[1], 6, 6) == "/" &
      nchar(date[1]) == 8) {
      output <- as.POSIXct(strptime(date, "%d/%m/%y"), tz = tz)
   }

   # example 04-04-13
   if (is.null(output) &
      substr(date[1], 3, 3) == "-" &
      substr(date[1], 6, 6) == "-" &
      nchar(date[1]) == 8) {
      output <- as.POSIXct(strptime(date, "%d-%m-%y"), tz = tz)
   }

   # example 2013.04.04
   if (substr(date[1], 5, 5) == "." &
      substr(date[1], 8, 8) == "." &
      nchar(date[1]) == 10) {
      output <- as.POSIXct(strptime(date, "%Y.%m.%d"), tz = tz)
   }

   # example 2013/04/04
   if (substr(date[1], 5, 5) == "/" &
      substr(date[1], 8, 8) == "/" &
      nchar(date[1]) == 10) {
      output <- as.POSIXct(strptime(date, "%Y/%m/%d"), tz = tz)
   }

   # example 2013-04-04
   if (substr(date[1], 5, 5) == "-" &
      substr(date[1], 8, 8) == "-" &
      nchar(date[1]) == 10) {
      output <- as.POSIXct(strptime(date, "%Y-%m-%d"), tz = tz)
   }


   return(output)
}
