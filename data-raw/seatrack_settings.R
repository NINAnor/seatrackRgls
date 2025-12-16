seatrack_settings <- read.csv(system.file("seatrack_settings", "seatrack_settings.csv", package = "seatrackRgls"), sep = ";", header = TRUE)
seatrack_settings$boundary.box <- lapply(seatrack_settings$boundary.box, function(x) {
    as.numeric(unlist(strsplit(gsub("[c() ]", "", x), ",")))
})
seatrack_settings$months_breeding <- lapply(seatrack_settings$months_breeding, function(x) {
    as.numeric(unlist(strsplit(gsub("[c() ]", "", x), ",")))
})
seatrack_filter_list <- lapply(seq_len(nrow(seatrack_settings)), function(i) {
    new_list <- as.list(seatrack_settings[i, ])
    new_list$months_breeding <- new_list$months_breeding[[1]]
    new_list$boundary.box <- new_list$boundary.box[[1]]
    bb <- sf::st_bbox(c(
        xmin = new_list$boundary.box[1],
        xmax = new_list$boundary.box[2],
        ymin = new_list$boundary.box[3],
        ymax = new_list$boundary.box[4]
    ))
    new_list$boundary.box <- bb

    return(new_list)
})
names(seatrack_filter_list) <- tolower(seatrack_settings$species)
GLS_settings_list <- lapply(seatrack_filter_list, function(x) {
    GLSsettings$new(
        logger_id = NULL,
        species = tolower(x$species),
        colony = NULL,
        years_tracked = NULL,
        settings = x
    )
})
seatrack_settings_list <- GLSfilterList$new(GLS_settings_list)

usethis::use_data(seatrack_settings_list, overwrite = TRUE)
