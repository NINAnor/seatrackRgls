#' Extract SEATRACK metadata to inform cleaning of GPS data
#'
#' Require login to the SEATRACK database. FUNCTION TO MOVE TO ANOTHER PACKAGE
#'
#' @param Colony name of colony, must correspond to the colony name in SEATRACK database. Required
#' @param Species name of species, must correspond to the species name in SEATRACK database. Required
#' @param Analyzer name of analyzer in order to keep some track of history. Optional
#' @return A data.frame with metadata extracted from the database. 'Sun' columns left blank.
#' @export
get_seatrack_metadata_GPS <- function(Colony, Species, Analyzer) {
  Individs <- getIndividInfo(colony = Colony)
  Individs <- Individs[Individs$species %in% Species, ]
  Individs <- as.data.frame(Individs)

  logger_info <- getLoggerInfo(asTibble = T)
  logger_info <- logger_info[logger_info$deployment_species %in% Species, ]
  logger_info <- logger_info[logger_info$colony %in% Colony, ]
  logger_info <- logger_info[logger_info$producer %in% "PathTrack", ]

  # NB! this code below should be reactivated in autumn 2025!!!
  # logger_info<-logger_info[logger_info$download_type%in%c("Successfully downloaded","Reconstructed"),]
  metadata_db <- logger_info[, c(1, 6, 10, 5, 17, 17, 21, 22, 22, 22, 10, 11, 2, 2, 2, 2, 2, 2, 2, 2, 8)]
  colnames(metadata_db) <- c(
    "logger_id", "logger_model", "year_tracked", "logger_producer", "ring_number", "country_code", "species", "colony", "col_lon", "col_lat",
    "date_deployed", "date_retrieved", "file_name", "analyzer", "data_responsible", "posdata_file", "age_deployed", "session_id"
  )


  i <- 1
  for (i in 1:length(unique(metadata_db$session_id))) {
    metadata_db$age_deployed[metadata_db$session_id %in% unique(metadata_db$session_id)[i]] <- Individs$status_age[Individs$session_id %in% unique(metadata_db$session_id)[i] & Individs$eventType %in% "Deployment"]
    metadata_db$ring_number[metadata_db$session_id %in% unique(metadata_db$session_id)[i]] <- Individs$ring_number[Individs$session_id %in% unique(metadata_db$session_id)[i] & Individs$eventType %in% "Deployment"]
    metadata_db$country_code[metadata_db$session_id %in% unique(metadata_db$session_id)[i]] <- Individs$euring_code[Individs$session_id %in% unique(metadata_db$session_id)[i] & Individs$eventType %in% "Deployment"]
    metadata_db$data_responsible[metadata_db$session_id %in% unique(metadata_db$session_id)[i]] <- Individs$data_responsible[Individs$session_id %in% unique(metadata_db$session_id)[i] & Individs$eventType %in% "Deployment"]
  }

  colony <- getColonies(loadGeometries = T)
  metadata_db$col_lat <- st_coordinates(colony[colony$colony_int_name %in% Colony, ])[2]
  metadata_db$col_lon <- st_coordinates(colony[colony$colony_int_name %in% Colony, ])[1]

  metadata_db$analyzer <- Analyzer
  metadata_db$posdata_file <- paste0("posdata_", Species, "_", Colony, "_GPS")
  metadata_db$session_id <- NULL

  # metadata_db$year_tracked<-paste0(year(metadata_db$date_deployed),"_",substr(year(metadata_db$date_retrieved),3,4))



  output <- metadata_db
  return(output)
}
