metadata <- read.csv(system.file("exdata", "metadata.csv", package = "seatrackRgls"), sep = ",", header = TRUE)
example_metadata <- metadata[1, c("logger_id", "logger_model", "date_deployed", "date_retrieved", "colony", "species")]
example_metadata$date_deployed <- as.Date(example_metadata$date_deployed, format = "%d/%m/%Y")
example_metadata$date_retrieved <- as.Date(example_metadata$date_retrieved, format = "%d/%m/%Y")
usethis::use_data(example_metadata, overwrite = TRUE)

example_colony_info <- metadata[, c("colony", "col_lat", "col_lon")]
example_colony_info <- example_colony_info[!duplicated(example_colony_info$colony), ]
usethis::use_data(example_colony_info, overwrite = TRUE)

example_extra_metadata <- metadata[1, c("logger_id", "date_retrieved", "logger_producer", "ring_number", "country_code")]
example_extra_metadata$date_retrieved <- as.Date(example_extra_metadata$date_retrieved, format = "%d/%m/%Y")
usethis::use_data(example_extra_metadata, overwrite = TRUE)
