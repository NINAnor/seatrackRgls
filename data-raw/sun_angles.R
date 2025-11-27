sun_angles <-
    list(
        general = seq(-1.5, -6.5, -0.5),
        summer = seq(-3, 8, 0.25),
        winter = seq(-10, 0, 0.25)
    )
usethis::use_data(sun_angles, overwrite = TRUE)
