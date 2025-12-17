import_directory <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Logger data/Raw logger data/ALL/"
metadata_path <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Positions_seatrackGLS/Scripts/metadata.xlsx"

all_colony_info <- gls_seatrack_colony_info()
result <- prepare_seatrack_calibration(metadata_path)
# calibration_data <- result$calibration_data
extra_metadata <- result$extra_metadata

# Minimum columns in filter_list
# logger_id, logger_model, species, colony
# if start_datetime is not provided, deployment and retrieval must be provided

all_colony_info <- data.frame(colony = "Halten", col_lat = 64.1716, col_lon = 9.4078)
metadata <- openxlsx2::read_xlsx(metadata_path)
metadata <- metadata[!is.na(metadata$logger_id) & !duplicated(metadata$logger_id), c("logger_id", "logger_model", "date_deployed", "date_retrieved", "colony", "species")]

# directories
dir_metadata <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Positions_seatrackGLS/example_LILA/"
dir_loggerdata <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Logger data/Raw logger data/ALL"
# naming convention of loggerdata = "LoggerSerialNumber_YearRetrieved_LoggerModel", i.e. "C23_2015_mk4083"
dir_output <- "C:/Users/julian.evans/Norsk Polarinstitutt/Benjamin Merkel - SEATRACK - shared/Database/Imports_Positions_seatrackGLS/example_LILA/output/"

# Prepare colony metadata, basic metadata for calibration. Basic metadata must have
# logger_id, logger_model, species, colony, deployment_date, retrieval_date.
# Extra metadata can also be provided, but isn't necessary during the calibration step.

# Example excel sheet to load:
metadata <- as.data.frame(readxl::read_excel(paste0(dir_metadata, "metadata.xlsx")))

filter_path <- file.path(dir_output, "filter_settings.xlsx")
create_filter_file(filter_path, "razorbill")

# help(prepare_calibration)
prepare_calibration(
    import_directory = dir_loggerdata,
    metadata = metadata,
    all_colony_info = metadata[1, c("colony", "col_lon", "col_lat")],
    output_dir = dir_output,
    show_filter_plots = TRUE,
    filter_list = filter_path
)

process_positions(
    import_directory = dir_loggerdata,
    calibration_data = file.path(dir_output, "calibration_data", "calibration.xlsx"),
    all_colony_info = metadata[1, c("colony", "col_lon", "col_lat")],
    output_directory = dir_output,
    filter_list = filter_path
)
