#' Get Penn World Table data
#'
#' @param pwt_version A string with the version of PWT you want to downlaod.
#'
#' @return A data frame with PWT data
#'
#' @references Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at www.ggdc.net/pwt
#'
#' @examples
#' pwt <- get_pwt()
get_pwt_uncached <- function(pwt_version = "10.01", gwcode = TRUE){
  url <- dplyr::case_when(
    pwt_version == "10.01" ~ "https://dataverse.nl/api/access/datafile/354098",
    pwt_version == "10.0" ~ "https://www.rug.nl/ggdc/docs/pwt100.dta",
    pwt_version == "10" ~ "https://www.rug.nl/ggdc/docs/pwt100.dta",
    pwt_version == "9.1" ~ "https://www.rug.nl/ggdc/docs/pwt91.dta",
    pwt_version == "9.0" ~ "https://www.rug.nl/ggdc/docs/pwt90.dta",
    .default = paste("pwt_version", pwt_version, "not supported"))

  if(grepl("not supported", url)){
    stop(url)
  }

  tmp <- tempfile()
  url |>
    httr2::request() |>
    httr2::req_perform(path = tmp)
  df <- haven::read_dta(tmp)
  unlink(tmp)

  if(gwcode){
    df$country_name <- countrycode::countrycode(df$countrycode, origin = "iso3c", destination = "country.name")
    df$gwcode <- countrycode::countrycode(df$country_name, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)

    df <- df |> dplyr::group_by(gwcode, year) |>
      dplyr::summarize(dplyr::across(dplyr::where(is.double), .fns = hablar::sum_))
  }

  return(df)
}

#' @describeIn get_pwt_uncached Get Penn World Table data, cached version
#'
#' @export
get_pwt <- memoise::memoise(get_pwt_uncached, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))


