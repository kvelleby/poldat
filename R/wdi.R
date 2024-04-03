#' Get data from World Development Indicators
#'
#' @param indicators Character vector of indicators codes. See the WDI::WDIsearch() function.
#'  If you supply a named vector, the indicators will be automatically renamed:
#'  ‘c(’women_private_sector' = 'BI.PWK.PRVS.FE.ZS')'
#' @param start Start date in year integer format.
#' @param end End date in year integer format.
#' @param gwcode If true, use the gwcode identifier for countries
#' @param merge_fun If gwcode is true, some countries are merged. This sets the function used to merge data. See ?hablar::sum_ for default function.
#'
#' @return A data frame with WDI data
#'
#' @references http://arelbundock.com https://github.com/vincentarelbundock/WDI
#' @references See WDI indicator online for correct citation.
#'
#' @examples
#' indicators <- c("wdi_pop" = "SP.POP.TOTL",
#'                 "wdi_gdp_pp_con_us" = "NY.GDP.MKTP.PP.KD",
#'                 "wdi_gdp_pp_cur_us" = "NY.GDP.MKTP.PP.CD")
#' df <- download_wdi(indicators)
download_wdi_uncached <- function(indicators, start = 1960, end = lubridate::year(Sys.Date()), gwcode = TRUE, merge_fun = hablar::sum_){
  dat <- WDI::WDI(indicator=indicators, start = start, end = end)

  if(gwcode){
    dat <- dat |>
      dplyr::mutate(country_name = countrycode::countrycode(iso3c, origin = "wb", destination = "country.name")) |>
      dplyr::mutate(gwcode = countrycode::countrycode(country_name, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches))

    dat <- dat |> dplyr::group_by(gwcode, year) |>
      dplyr::summarize(dplyr::across(dplyr::where(is.double), .fns = merge_fun))
  }

  dat$version <- lubridate::quarter(Sys.Date(), with_year = TRUE)

  return(dat)
}


#' @describeIn download_wdi_uncached Get data from World Development Indicators, cached version
#'
#' @export
download_wdi <- memoise::memoise(download_wdi_uncached, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))


