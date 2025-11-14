#' Update existing SEATRACK metadata to inform processing of light data into positions
#'
#' Require login to the SEATRACK database. FUNCTION TO MOVE TO ANOTHER PACKAGE
#'
#' @param Colony name of colony, must correspond to the colony name in SEATRACK database. Required
#' @param Species name of species, must correspond to the species name in SEATRACK database. Required
#' @param Analyzer name of analyzer in order to keep some track of history. Optional
#' @return A data.frame with metadata extracted from the database. 'Sun' columns left blank.
#' @export
update_seatrack_metadata <- function(df, Colony, Species, Analyzer) {
  df_old <- df
  df_new <- get_seatrack_metadata(Colony = Colony, Species = Species, Analyzer = Analyzer)


  df_new$id_column <- paste(df_new$logger_id, year(df_new$date_deployed), year(df_new$date_retrieved), sep = "_")
  df_old$id_column <- paste(df_old$logger_id, year(df_old$date_deployed), year(df_old$date_retrieved), sep = "_")

  i <- 1
  for (i in 1:length(unique(df_old$id_column))) {
    if (length(df_new$date_retrieved[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$date_retrieved[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$date_retrieved[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$date_deployed[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$date_deployed[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$date_deployed[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$logger_model[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$logger_model[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$logger_model[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$ring_number[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$ring_number[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$ring_number[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$logger_producer[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$logger_producer[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$logger_producer[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$country_code[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$country_code[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$country_code[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$species[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$species[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$species[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$colony[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$colony[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$colony[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$col_lon[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$col_lon[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$col_lon[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$col_lat[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$col_lat[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$col_lat[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$posdata_file[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$posdata_file[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$posdata_file[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$age_deployed[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$age_deployed[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$age_deployed[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
    if (length(df_new$data_responsible[df_new$id_column %in% unique(df_old$id_column)[i]]) != 0) {
      df_old$data_responsible[df_old$id_column %in% unique(df_old$id_column)[i]] <- df_new$data_responsible[df_new$id_column %in% unique(df_old$id_column)[i]]
    }
  }

  df_old$id_column <- NULL


  output <- df_old
  return(output)
}
