#' Helper function to get V-Dem variables into the cShapes system used in poldat
#'
#'
#' @examples
#' df <- get_vdem(v2x_libdem)
#' df <- get_vdem(v2x_libdem, v2x_regime, .fun = min)
get_vdem <- function(..., .fun = mean, na.rm = TRUE){
  df <- vdemdata::vdem |> dplyr::select(country_name, country_text_id, country_id, year, ...) |> dplyr::filter(year >= 1946)
  df$country_name <- countrycode::countrycode(df$country_id, origin = "vdem", destination = "country.name",
                                              custom_match = c("128" = "Occupied Palestinian Territories", "178" = "Malta", "209" = "Palestine"))
  df$gwcode <- countrycode::countrycode(df$country_name, origin = "country.name", destination = "gwn",
                                        custom_match = c(custom_gwcode_matches, "Republic of Vietnam" = 817))
  df <- df |>
    group_by(gwcode, year) |>
    summarize_at(vars(...), .fun = .fun, na.rm = na.rm) |>
    dplyr::mutate_at(vars(...), .fun = function(x) if_else(is.infinite(x), NA, x)) |>
    dplyr::mutate_at(vars(...), .fun = function(x) if_else(is.nan(x), NA, x))
  return(df)
}

