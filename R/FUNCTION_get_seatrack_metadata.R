#' Extract SEATRACK metadata to inform processing of light data into positions
#'
#' Require login to the SEATRACK database. FUNCTION TO MOVE TO ANOTHER PACKAGE
#'
#' @param Colony name of colony, must correspond to the colony name in SEATRACK database. Required
#' @param Species name of species, must correspond to the species name in SEATRACK database. Required
#' @param Analyzer name of analyzer in order to keep some track of history. Optional
#' @return A data.frame with metadata extracted from the database. 'Sun' columns left blank.
#' @export
get_seatrack_metadata <- function(Colony, Species, Analyzer) {
  Individs <- getIndividInfo(colony = Colony)
  Individs <- Individs[Individs$species %in% Species, ]
  Individs <- as.data.frame(Individs)

  logger_info <- getLoggerInfo(asTibble = T)
  logger_info <- logger_info[logger_info$deployment_species %in% Species, ]
  logger_info <- logger_info[logger_info$colony %in% Colony, ]

  logger_info <- logger_info[logger_info$download_type %in% c("Successfully downloaded", "Reconstructed"), ]
  metadata_db <- logger_info[, c(1, 6, 10, 5, 17, 17, 21, 22, 22, 22, 10, 11, 2, 2, 2, 2, 2, 2, 2, 2, 8)]
  colnames(metadata_db) <- c(
    "logger_id", "logger_model", "year_tracked", "logger_producer", "ring_number", "country_code", "species", "colony", "col_lon", "col_lat",
    "date_deployed", "date_retrieved", "light_threshold", "sun_angle_start", "sun_angle_end", "file_name", "analyzer", "data_responsible", "posdata_file", "age_deployed", "session_id"
  )

  metadata_db <- metadata_db[!(metadata_db$logger_model %in% "L250A"), ]

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

  metadata_db$light_threshold <- 1
  metadata_db$light_threshold[metadata_db$logger_model %in% "mk3006"] <- 9
  metadata_db$light_threshold[metadata_db$logger_producer %in% "Migrate Technology"] <- 11
  metadata_db$light_threshold[metadata_db$logger_model %in% "LAT"] <- 150

  metadata_db$sun_angle_start <- NA
  metadata_db$sun_angle_end <- NA
  metadata_db$file_name <- paste0(metadata_db$logger_id, "_", year(metadata_db$date_retrieved), "_", metadata_db$logger_model, ".txt")
  metadata_db$file_name[metadata_db$logger_producer %in% "Migrate Technology"] <- paste0(metadata_db$logger_id[metadata_db$logger_producer %in% "Migrate Technology"], "_", year(metadata_db$date_retrieved[metadata_db$logger_producer %in% "Migrate Technology"]), "_", metadata_db$logger_model[metadata_db$logger_producer %in% "Migrate Technology"], ".lux")
  metadata_db$file_name[metadata_db$logger_producer %in% c("BAS", "Lotek", "BAStrack", "Biotrack", "Bastrack")] <- paste0(metadata_db$logger_id[metadata_db$logger_producer %in% c("BAS", "Lotek", "BAStrack", "Biotrack", "Bastrack")], "_", year(metadata_db$date_retrieved[metadata_db$logger_producer %in% c("BAS", "Lotek", "BAStrack", "Biotrack", "Bastrack")]), "_", metadata_db$logger_model[metadata_db$logger_producer %in% c("BAS", "Lotek", "BAStrack", "Biotrack", "Bastrack")], ".lig")

  metadata_db$analyzer <- Analyzer
  metadata_db$posdata_file <- paste0("posdata_", Species, "_", Colony)
  metadata_db$session_id <- NULL

  metadata_db$year_tracked <- paste0(year(metadata_db$date_deployed), "_", substr(year(metadata_db$date_retrieved), 3, 4))
  metadata_db <- metadata_db[!is.na(metadata_db$date_retrieved), ]
  metadata_db <- metadata_db[!is.na(metadata_db$date_deployed), ]
  metadata_db$n_years <- (year(metadata_db$date_retrieved) - year(metadata_db$date_deployed))

  generate_year_covered <- function(df) {
    # Create an empty dataframe to store the new rows
    new_df <- data.frame()
    # Loop through each row in the dataframe
    for (i in 1:nrow(df)) {
      # Get the start year, end year, and n_years for the current row
      start_year <- df$start_year[i]
      end_year <- df$end_year[i]
      n_years <- df$n_years[i]
      # Generate the 'year_covered' values
      year_covered <- vector("character", length = n_years)
      # Generate the 'year_covered' values for the current row
      for (j in 1:n_years) {
        year_covered[j] <- paste0(start_year + j - 1, "_", start_year + j)
      }
      # Replicate the current row according to the n_years
      new_rows <- df[i, , drop = FALSE] # Keep it as a dataframe
      # Duplicate the rows and add the corresponding 'year_covered' values
      new_rows <- new_rows[rep(1, n_years), , drop = FALSE] # Replicate the row n_years times
      new_rows$year_covered <- year_covered # Assign the year_covered column
      # Bind these new rows to the new_df
      new_df <- rbind(new_df, new_rows)
    }
    return(new_df) # Make sure return is outside the loop
  }

  metadata_db$start_year <- year(metadata_db$date_deployed)
  metadata_db$end_year <- year(metadata_db$date_retrieved)
  # Apply the function
  metadata_db <- generate_year_covered(metadata_db)

  metadata_db$start_year <- NULL
  metadata_db$end_year <- NULL
  metadata_db$year_tracked <- paste0(substr(metadata_db$year_covered, 1, 5), substr(metadata_db$year_covered, 8, 9))
  metadata_db$year_covered <- NULL
  metadata_db$n_years <- NULL


  output <- metadata_db
  return(output)
}
