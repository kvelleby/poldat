vdem <- get_vdem(v2x_libdem, v2x_regime) |> area_weighted_synthetic_data(2019)
ucdp <- ucdpbrds |> dplyr::select(gwcode, year, best, low, high) |> area_weighted_synthetic_data(2019)


static_world <- dplyr::full_join(ucdp, vdem)
usethis::use_data(static_world, overwrite = TRUE)
