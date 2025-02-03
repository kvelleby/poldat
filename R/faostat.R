#' Get the Suite of Food Security Indicators
#'
#' @return A data frame with Food Security Indicators
#'
#' @examples
#' fao <- get_fao_food_security()
get_fao_food_security_uncached <- function(gwcode = TRUE){
  url <- "https://bulks-faostat.fao.org/production/Food_Security_Data_E_All_Data.zip"

  tmp <- tempfile()
  url |>
    httr2::request() |>
    httr2::req_perform(path = tmp)
  unzip(tmp, list = T)
  unzip(tmp, files = "Food_Security_Data_E_All_Data.csv", exdir = dirname(tmp))
  datafile <- file.path(dirname(tmp), "Food_Security_Data_E_All_Data.csv")
  df <- readr::read_csv(datafile) |> janitor::clean_names()
  unlink(tmp)
  unlink(datafile)


  regions <- df |> dplyr::filter(item == "Prevalence of undernourishment (percent) (annual value)") |> dplyr::pull(area_code) |> unique()
  # f means flag
  # n means note
  years <- paste0("y", 2000:2023)
  keyvars <- c("area_code", "area_code_m49", "area", "item_code", "item", "element_code", "element", "unit")

  final <- df |> dplyr::select(all_of(c(keyvars, years))) |>
    dplyr::filter(!area_code %in% regions) |>
    dplyr::mutate(across(.cols = all_of(years), .fns = ~as.character(.))) |>
    tidyr::pivot_longer(cols = all_of(years), names_to = "year", values_to = "value") |>
    dplyr::mutate(value = stringr::str_replace_all(value, "<", "") |> as.numeric(value)) |> na.omit() |>
    dplyr::mutate(year = stringr::str_replace_all(year, "y", "") |> as.numeric(year))

  long_variable_names <- unique(final$item)
  var_names <- c("energy_supply", "gdp_per_capita", "food_variance", "safe_water_pct", "basic_water_pct", "basic_sanit_pct", "wasting_pct", "wasting_num", "stunting_pct", "stunting_num", "overweight_pct", "overweight_num", "obesity_pct", "obesity_num", "anemia_pct", "anemia_num", "breastfeed_pct", "min_energy_req", "avg_energy_req", "calorie_var", "retail_loss", "rail_density", "safe_sanit_pct", "low_birth_pct", "low_birth_num")
  name_match <- cbind(long_variable_names, var_names) |> dplyr::as_tibble()


  final <- dplyr::left_join(final, name_match, by = c("item" = "long_variable_names"))
  final <- final |> dplyr::select(all_of(c("area", "area_code", "year", "var_names", "value"))) |>
    tidyr::pivot_wider(id_cols = c("area", "area_code", "year"), names_from = "var_names")

  ccodes <- readr::read_csv("data-raw/FAOSTAT_data_2-3-2025.csv") |> janitor::clean_names()

  final <- final |> dplyr::left_join(ccodes, by = c("area_code" = "country_code"))



  if(gwcode){
    gwcodes <- countrycode::codelist_panel |> dplyr::select(year, iso3c, gwn)
    final <- final |> dplyr::left_join(gwcodes, by = c("year", "iso3_code" = "iso3c"))

    final$country_name <- countrycode::countrycode(final$iso3_code, origin = "iso3c", destination = "country.name")
    final$gwcode <- countrycode::countrycode(final$country_name, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)

    # final[final[c("gwcode", "year")] |> duplicated(),] |> dplyr::pull(gwcode)
    # final |> dplyr::filter(gwcode == 710) |> dplyr::pull(area) |> unique()
    # final |> dplyr::filter(gwcode == 390) |> dplyr::pull(area) |> unique()
    # final |> dplyr::filter(gwcode == 200) |> dplyr::pull(area) |> unique()
    # final |> dplyr::filter(gwcode == 2) |> dplyr::pull(area) |> unique()

    # The variables are not easy to aggregate, so drop the small entities instead.
    final <- final |> dplyr::filter(!(gwcode == 710 & area %in% c("China, Hong Kong SAR", "China, Macao SAR")))
    final <- final |> dplyr::filter(!(gwcode == 390 & area == "Greenland"))
    final <- final |> dplyr::filter(!(gwcode == 200 & area == "Bermuda"))
    final <- final |> dplyr::filter(!(gwcode == 2 & area == "American Samoa"))

    final <- final |> dplyr::filter(!is.na(gwcode)) |> dplyr::select(dplyr::all_of(c("gwcode", "year", "country_name", var_names)))
  }

  return(final)
}

#' @describeIn get_ggdc_uncached Get data from Grönigen Growth and Development Centre, cached version
#'
#' @export
get_fao_food_security <- memoise::memoise(get_fao_food_security_uncached, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))



#' Estimates prevalence of undernourishment
#'
#' See <https://www.fao.org/fileadmin/templates/ess/documents/food_security_statistics/metadata/undernourishment_methodology.pdf>
#'
#' @param DES Dietary energy supply per person
#' @param CV Coefficient of variation of the household daily per person dietary energy consumption
#' @param MDER Mean Dietary Energy Consumption
#' @param population Population (optional)
#'
#' @returns
#' @export
#'
#' @examples
#' fs <- get_fao_food_security()
#' pou <- estimate_undernourishment(fs$energy_supply, fs$calorie_var, fs$min_energy_req)
estimate_undernourishment <- function(DES, CV, MDER, population = NULL) {
  # Calculate parameters of lognormal distribution
  # σx = [log_e(CV² + 1)]^0.5
  sigma_x <- sqrt(log(CV^2 + 1))

  # μx = log_e(x̄) - σ²/2
  mu_x <- log(DES) - (sigma_x^2)/2

  # Calculate proportion undernourished using standard normal CDF
  # P(U) = Φ[(log_e(MDER) - μx)/σx]
  prop_undernourished <- pnorm((log(MDER) - mu_x)/sigma_x)

  # Calculate number of undernourished if population is provided
  n_undernourished <- if (!is.null(population)) {
    population * prop_undernourished
  } else {
    NULL
  }

  # Return results
  list(
    prevalence_of_undernourishment = prop_undernourished,
    percentage_undernourished = prop_undernourished * 100,
    number_undernourished = n_undernourished,
    parameters = list(
      mu = mu_x,
      sigma = sigma_x,
      DES = DES,
      CV = CV,
      MDER = MDER
    )
  )
}

#' Perform multiple imputation using Gaussian Copulas on FAO food security data
#'
#' See <https://journals.sagepub.com/doi/10.1177/0049124118799381> for example of this approach.
#'
#' @param nsamp number of iterations of the Markov chain
#' @param seed integer for random seed
#'
#' @returns
#' @export
#'
#' @examples
fs_multiple_imputation <- function(nsamp = 100, seed = 42){
  fs <- get_fao_food_security()
  pou <- estimate_undernourishment(fs$energy_supply, fs$calorie_var, fs$min_energy_req)

  fs$pou <- pou$prevalence_of_undernourishment

  imputation_matrix <- fs |> dplyr::select(-dplyr::all_of(c("country_name"))) |> as.matrix()
  fit <- sbgcop::sbgcop.mcmc(imputation_matrix, nsamp = 100, seed = seed)
  return(fit)
}
