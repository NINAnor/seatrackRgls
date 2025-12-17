#' Land mask
#'
#' The function measures the distance between any location and to a coastline, using the package ‘distancetocoast’ and a high-resolution coastline from Natural Earth ("10").
#' Land and ocean location can  be removed surpassing a specified distance in km.
#' To avoid this function, apply very far (e.g '10000' km) or infinite ('Inf') distance.
#'
#' @param lon longitude in decimal degrees
#' @param lat latitude in decimal degrees
#' @param coast_to_land locations filtered if X km inland from coastline
#' @param coast_to_sea locations filtered if X km offshore from the coastline
#' @param eqfilter if equinox, locations are not filtered
#' @return A data frame with longitude and latitude filtered.
#' @concept filtering
#' @export
land_mask <- function(lon, lat, coast_to_land, coast_to_sea, eqfilter) {
  set.seed(0)
  points <- cbind(lon, lat)
  points <- as.data.frame(na.omit(points))
  pts <- sf::st_as_sf(points, coords = 1:2, crs = 4326)
  points$over_land <- FALSE
  world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
  points$over_land <- !is.na(as.numeric(sf::st_intersects(pts, world)))
  points$eqfilter <- eqfilter

  # SHOULD REPLACE WITH TERRA
  points$dist_to_coastline <- terra::extract(distancetocoast::distance_to_coastline_10, cbind(lon, lat)) / 1000

  points$remove <- FALSE
  points$remove[points$dist_to_coastline > coast_to_land & points$over_land %in% TRUE & points$eqfilter == 1] <- TRUE
  points$remove[points$dist_to_coastline > coast_to_sea & points$over_land %in% FALSE & points$eqfilter == 1] <- TRUE

  output <- points$remove
  return(output)
}
