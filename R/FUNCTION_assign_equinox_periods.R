#' Assign equinox periods
#'
#' Assign which days of the year estimated latitudes are unreliable due to equinox.
#' Limits are based on
#' 1) Which approximate latitude did the geolocator record light-data (objective; towards the poles changes in daylength is more dramatic around equinox than at equator = shorter equinox period).
#' 2) how much uncertainty allowed (subjective)	: uncertainty gets extreme right up to the equinox and less further away in time. ut off point of the equinox period aim to keep an average day-to-day variance in latitude below 1.5 degrees (165 km).
#' 3)	Results from sun angle calibrations (objective) which is a result of a) threshold (light-level) used to differentiate between night and daytime and b) the sensor's light sensitivity.
#'
#' Point 1-3 are compared to a reference table:
#'
#' Apparent equinox (period midpoint) is defined from how sun angles compare to calculations in Hill & Braun (2001).
#' Period start and end is based on estimations of variance in Hill & Braun (2001) and the mean day-to-day variance in latitudes observed in 16 000 tracked non-breeding seasons in SEATRACK.
#'
#' The function performs the following steps:
#' \enumerate{
#'  \item Calculate median sun angle for a 5-day period at yday 41:45 (>30 days before spring EQ), yday 113:117 (>30 days after spring EQ), yday 227:231 (>30 days before autumn EQ), yday 300:304 (>30 days after autumn EQ).
#'   \item If no sun angles available - use first sun angle
#'   \item Calculate median latitudes for a 5-day period at yday 41:45 (>30 days before spring EQ), yday 113:117 (>30 days after spring EQ), yday 227:231 (>30 days before autumn EQ), yday 300:304 (>30 days after autumn EQ).
#'   \item If no latitudes because of midnight sun - use breeding location
#'   \item Matches e.g yday 41:45 median sun angle and median latitude to the reference table within the R package to identify the start of spring equinox period an so on.
#' }
#'
#' @param lats Numeric value with latitudes from the tracked individual.
#' @param dates Dates (tfirst) when latitudes occur
#' @param breedingloc_lat Approximate latitude for the breeding locality.
#' @param sun Numeric  value showing the sun's angle to the horizon at each twilight. Is a result based on the chosen threshold of light used in twilight estimations and the light sensor sensitivity
#' @return A list saying if the equinox effect is present or not (1 = no equinox effect, 0 = equinox effect).
#' @export




### må legge til funksjonen en måte å forstå median sun angle for de samme periodene.
assign_equinox_periods <- function(lats, dates, breedingloc_lat, sun) {
    equinox_table <- read.table(system.file("equinox_table", "equinox_table.txt", package = "seatrackGLS"), header = TRUE)

    DOY <- as.numeric(strftime(dates, format = "%j"))
    feb_march1_sun <- NA
    feb_march1_sun <- median(sun[DOY %in% c(41:45)])
    march_april1_sun <- NA
    march_april1_sun <- median(sun[DOY %in% c(113:117)])
    august_september1_sun <- NA
    august_september1_sun <- median(sun[DOY %in% c(227:231)])
    september_october1_sun <- NA
    september_october1_sun <- median(sun[DOY %in% c(300:304)])

    if (is.na(feb_march1_sun)) feb_march1_sun <- march_april1_sun
    if (is.na(feb_march1_sun)) feb_march1_sun <- sun[1]
    if (is.na(march_april1_sun)) march_april1_sun <- feb_march1_sun
    if (is.na(march_april1_sun)) march_april1_sun <- sun[1]
    if (is.na(august_september1_sun)) august_september1_sun <- september_october1_sun
    if (is.na(august_september1_sun)) august_september1_sun <- sun[1]
    if (is.na(september_october1_sun)) september_october1_sun <- august_september1_sun
    if (is.na(september_october1_sun)) september_october1_sun <- sun[1]

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - feb_march1_sun) == min(abs(unique(equinox_table$sun_angle) - feb_march1_sun)))]
    new_start_DOY <- min(equinox_table$start_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    feb_march2_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(feb_march2_sun)) feb_march2_sun <- feb_march1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - feb_march2_sun) == min(abs(unique(equinox_table$sun_angle) - feb_march2_sun)))]
    new_start_DOY <- min(equinox_table$start_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    feb_march_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(feb_march_sun)) feb_march_sun <- feb_march1_sun
    feb_march_lat <- NA
    feb_march_lat <- median(lats[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(feb_march_lat)) feb_march_lat <- breedingloc_lat

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - march_april1_sun) == min(abs(unique(equinox_table$sun_angle) - march_april1_sun)))]
    new_start_DOY <- max(equinox_table$end_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    march_april2_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(march_april2_sun)) march_april2_sun <- feb_march1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - march_april2_sun) == min(abs(unique(equinox_table$sun_angle) - march_april2_sun)))]
    new_start_DOY <- max(equinox_table$end_spring_eq[equinox_table$sun_angle %in% sun_angle[1]])
    march_april_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(march_april_sun)) march_april_sun <- march_april1_sun
    march_april_lat <- NA
    march_april_lat <- median(lats[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(march_april_lat)) march_april_lat <- breedingloc_lat

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - august_september1_sun) == min(abs(unique(equinox_table$sun_angle) - august_september1_sun)))]
    new_start_DOY <- min(equinox_table$start_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    august_september2_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(august_september2_sun)) august_september2_sun <- august_september1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - august_september2_sun) == min(abs(unique(equinox_table$sun_angle) - august_september2_sun)))]
    new_start_DOY <- min(equinox_table$start_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    august_september_sun <- median(sun[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(august_september_sun)) august_september_sun <- august_september1_sun
    august_september_lat <- NA
    august_september_lat <- median(lats[DOY %in% c((new_start_DOY - 4):new_start_DOY)])
    if (is.na(august_september_lat)) august_september_lat <- breedingloc_lat

    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - september_october1_sun) == min(abs(unique(equinox_table$sun_angle) - september_october1_sun)))]
    new_start_DOY <- max(equinox_table$end_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    september_october2_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(september_october2_sun)) september_october2_sun <- september_october1_sun
    sun_angle <- unique(equinox_table$sun_angle)[which(abs(unique(equinox_table$sun_angle) - september_october2_sun) == min(abs(unique(equinox_table$sun_angle) - september_october2_sun)))]
    new_start_DOY <- max(equinox_table$end_aut_eq[equinox_table$sun_angle %in% sun_angle[1]])
    september_october_sun <- median(sun[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(september_october_sun)) september_october_sun <- september_october1_sun
    september_october_lat <- NA
    september_october_lat <- median(lats[DOY %in% c(new_start_DOY:(new_start_DOY + 4))])
    if (is.na(september_october_lat)) september_october_lat <- breedingloc_lat



    aut_start_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - august_september_lat))]
    aut_start_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - august_september_sun))]
    start_aut <- equinox_table$start_aut_eq[equinox_table$sun_angle %in% aut_start_sun & equinox_table$latitude %in% aut_start_lat]

    end_aut_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - september_october_lat))]
    end_aut_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - september_october_sun))]
    end_aut <- equinox_table$end_aut_eq[equinox_table$sun_angle %in% end_aut_sun & equinox_table$latitude %in% end_aut_lat]

    start_spring_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - feb_march_lat))]
    start_spring_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - feb_march_sun))]
    start_spring <- equinox_table$start_spring_eq[equinox_table$sun_angle %in% start_spring_sun & equinox_table$latitude %in% start_spring_lat]

    end_spring_lat <- equinox_table$latitude[which.min(abs(equinox_table$latitude - march_april_lat))]
    end_spring_sun <- equinox_table$sun_angle[which.min(abs(equinox_table$sun_angle - march_april_sun))]
    end_spring <- equinox_table$end_spring_eq[equinox_table$sun_angle %in% end_spring_sun & equinox_table$latitude %in% end_spring_lat]



    df <- as.data.frame(DOY)
    colnames(df) <- c("DOY")
    df$eqfilter <- 1
    df$eqfilter[df$DOY >= (start_aut + 1) & df$DOY <= (end_aut - 1)] <- 0
    df$eqfilter[df$DOY >= (start_spring + 1) & df$DOY <= (end_spring - 1)] <- 0

    output <- df$eqfilter
    return(output)
}
