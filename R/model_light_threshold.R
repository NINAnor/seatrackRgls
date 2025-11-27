get_threshold <- function(model) {
    threshold_300 <- c(
        "c330",
        "f100",
        "c250",
        "c65",
        "w65",
        "Intigeo-P55B1-7",
        "c65_super",
        "Intigeo-P35A11-7-SGA-NOT",
        "W65A9-SEA",
        "W30A9-SEA",
        "W30A9-SEA-NOT",
        "c65_NOT",
        "c331"
    )
    if (model %in% threshold_300) {
        return(300)
    } else {
        return(50)
    }
}
