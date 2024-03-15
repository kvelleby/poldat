vdem <- get_vdem(v2x_libdem, v2x_regime) |> area_weighted_synthetic_data(2019)
ucdp <- ucdpbrds |> dplyr::select(gwcode, year, best, low, high) |> area_weighted_synthetic_data(2019)
#pwt <- get_pwt() |> dplyr::select(gwcode, year, rgdpe, rgdpo, pop, emp, cgdpe, cgdpo)

static_world <- dplyr::full_join(ucdp, vdem)
#static_world <- dplyr::left_join(static_world, pwt, by = c("gwcode", "year"))
usethis::use_data(static_world, overwrite = TRUE)
