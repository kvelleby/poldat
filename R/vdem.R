#' Helper function to get V-Dem variables
#'
#' Use `vdemdata::vdem`, but matches (as best as possible) to the cShapes system used in poldat.
#' The system used by V-Dem is not static, but does not match perfectly up with poldat.
#' For example, West Bank is not coded from 1950-1966 in V-Dem because of the Jordanian
#' occupation, and treats West Bank as both West Bank and Gaza from 1994-2007. cShapes
#' codes West Bank and Gaza independently until 1967, and then as part of Israel since,
#' whilst there is an option in poldat to treat West Bank and Gaza as a common unit after
#' 1967. This function merge states that are treated as a single unit in poldat, but multiple in
#' V-Dem. It likely misses many issues, but tries to accommodate at least political units currently
#' present (as coded by poldat/cShapes).
#'
#' Current states with > 250 000 population that are missing in V-Dem:
#'  Puerto Rico, Bahamas, Belize, Western Sahara, Brunei, New Caledonia and Dependencies, French Polynesia
#'
#' Filter data before 1946 for the moment (as there are likely many more inconsistencies before then).
#'
#'
#' @param ... <\code{\link[=dplyr::dplyr_tidy_select]{tidy-select}}> One or more unquoted
#'  expressions separated by commas. Variable names can be used as if they
#'  were positions in the data frame, so expressions like \code{x:y} can
#'  be used to select a range of variables. See `vdemdata::codebook$clean_tag` for valid expressions.
#' @param .fun The function used to summarize observations for mergers.
#' @param impute If true the following things are added:
#'   Uses Jordan coding for West Bank between 1950-67.
#'   Uses USA coding for Puerto Rico.
#'   Uses France coding for New Caledonia and French Polynesia.
#'   Belize, Bahamas, Western Sahara, and Brunei are kept missing.
#'
#'
#' @examples
#' df <- get_vdem(v2x_libdem)
#' df <- get_vdem(v2x_libdem, v2x_regime, .fun = min)
#'
#' @references Lührmann, Anna, Nils Düpont, Masaaki Higashijima, Yaman Berker Kavasoglu, Kyle L. Marquardt, Michael Bernhard, Holger Döring, Allen Hicken, Melis Laebens, Staffan I. Lindberg, Juraj Medzihorsky, Anja Neundorf, Ora John Reuter, Saskia Ruth-Lovell, Keith R. Weghorst, Nina Wiesehomeier, Joseph Wright, Nazifa Alizada, Paul Bederke, Lisa Gastaldi, Sandra Grahn, Garry Hindle, Nina Ilchenko, Johannes von Römer, Steven Wilson, Daniel Pemstein, Brigitte Seim. 2020. Varieties of Party Identity and Organization (V-Party) Dataset V1. Varieties of Democracy (V-Dem) Project. https://doi.org/10.23696/vpartydsv1
#' @source https://www.v-dem.net/
get_vdem <- function(..., .fun = mean, na.rm = TRUE, impute = TRUE){
  df <- vdemdata::vdem |> dplyr::select(country_name, country_text_id, country_id, year, ...) |> dplyr::filter(year >= 1946)

  if(impute){
    missing_west_bank <- df |> dplyr::filter(country_name == "Jordan", year >= 1950, year < 1967)
    missing_west_bank$country_name <- "Palestine/West Bank"
    missing_west_bank$country_text_id <- "PSE"
    missing_west_bank$country_id <- 128
    df <- dplyr::bind_rows(df, missing_west_bank)
  }


  df$country_name <- countrycode::countrycode(df$country_id, origin = "vdem", destination = "country.name",
                                              custom_match = c("128" = "Palestine/West Bank",
                                                               "209" = "Palestine/British Mandate",
                                                               "138" = "Palestine/Gaza"))
  df <- df |> dplyr::mutate(country_name = dplyr::if_else((grepl("West Bank|Gaza", country_name) & year>=1967), "Occupied Palestinian Territories", country_name))
  df$gwcode <- countrycode::countrycode(df$country_name, origin = "country.name", destination = "gwn",
                                        custom_match = c(custom_gwcode_matches, "Republic of Vietnam" = 817))

  if(impute){
    puerto_rico <- df |> dplyr::filter(country_name == "United States")
    puerto_rico$gwcode <- 6
    new_caledonia <- df |> dplyr::filter(country_name == "France")
    new_caledonia$gwcode <- 930
    french_polynesia <- df |> dplyr::filter(country_name == "France")
    french_polynesia$gwcode <- 960

    df <- dplyr::bind_rows(df, puerto_rico)
    df <- dplyr::bind_rows(df, new_caledonia)
    df <- dplyr::bind_rows(df, french_polynesia)
  }

  df <- df |>
    dplyr::group_by(gwcode, year) |>
    dplyr::summarize_at(dplyr::vars(...), .fun = .fun, na.rm = na.rm) |>
    dplyr::mutate_at(dplyr::vars(...), .fun = function(x) dplyr::if_else(is.infinite(x), NA, x)) |>
    dplyr::mutate_at(dplyr::vars(...), .fun = function(x) dplyr::if_else(is.nan(x), NA, x))
  return(df)
}
