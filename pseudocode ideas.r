import_directory <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Logger data/Raw logger data/ALL/"
metadata_path <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Positions_seatrackGLS/Scripts/metadata.xlsx"

all_colony_info <- gls_seatrack_colony_info()
result <- prepare_seatrack_calibration(metadata_path)
# calibration_data <- result$calibration_data
extra_metadata <- result$extra_metadata

# Minimum columns in filter_list
# logger_id, logger_model, species, colony
# if start_datetime is not provided, deployment and retrieval must be provided

metadata <- openxlsx2::read_xlsx(metadata_path)
metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "logger_model", "date_deployed", "date_retrieved", "colony", "species")]
