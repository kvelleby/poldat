#' Get data from Grönigen Growth and Development Centre
#'
#' @param dataset A string "pwt" for Penn World Tables and "maddison" for Maddison Project data.
#' @param version A string with the version of PWT or Maddison you want to downlaod.
#'
#' @return A data frame with PWT or Maddison data
#'
#' @references Feenstra, Robert C., Robert Inklaar and Marcel P. Timmer (2015), "The Next Generation of the Penn World Table" American Economic Review, 105(10), 3150-3182, available for download at www.ggdc.net/pwt
#' @references Maddison Project Database, version 2020. Bolt, Jutta and Jan Luiten van Zanden (2020), “Maddison style estimates of the evolution of the world economy. A new 2020 update”.
#'
#' @examples
#' pwt <- get_penn(dataset = "pwt", version = "10.01")
get_ggdc_uncached <- function(dataset = "pwt", version = "10.01", gwcode = TRUE){
  url <- dplyr::case_when(
    (dataset == "pwt" & version == "10.01") ~ "https://dataverse.nl/api/access/datafile/354098",
    (dataset == "pwt" & version == "10.0") ~ "https://www.rug.nl/ggdc/docs/pwt100.dta",
    (dataset == "pwt" & version == "10") ~ "https://www.rug.nl/ggdc/docs/pwt100.dta",
    (dataset == "pwt" & version == "9.1") ~ "https://www.rug.nl/ggdc/docs/pwt91.dta",
    (dataset == "pwt" & version == "9.0") ~ "https://www.rug.nl/ggdc/docs/pwt90.dta",
    (dataset == "maddison" & version == "2020") ~ "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta",
    !(dataset %in% c("pwt", "maddison")) ~ "not supported",
    .default = paste("version", version, "not supported"))

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

#' @describeIn get_ggdc_uncached Get data from Grönigen Growth and Development Centre, cached version
#'
#' @export
get_ggdc <- memoise::memoise(get_ggdc_uncached, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))


