#' Test if a version number exists in the UCDP GED API
#'
#' See https://ucdp.uu.se/apidocs/ for more information
#'
#' @param version A character string
#'
#' @return A boolean
#' @export
#'
#' @examples
#' versions <- c("23.1", "26.2", "23.0.10")
#' versions <- sapply(versions, check_ucdp_version)
check_ucdp_version <- function(version){
  req <- httr2::request("https://ucdpapi.pcr.uu.se/api/") |>
    httr2::req_url_path_append("gedevents/") |>
    httr2::req_url_path_append(version) |>
    httr2::req_url_query("pagesize" = 1) |>
    httr2::req_throttle(rate = 1)

  try(httr2::req_perform(req), silent = TRUE)
  if(httr2::last_response() |> httr2::resp_status() == 200){
    return(TRUE)
  } else{
    return(FALSE)
  }
}

#' Find the latest version of the UCDP GED data
#'
#' See https://ucdp.uu.se/apidocs/ for more information
#'
#' @param type Either 'stable' or 'candidate'
#'
#' @return A character string representing the latest version of UCDP data.
#' @export
#'
#' @examples
#' stable_version <- latest_ucdp_version(type = "stable")
#' ucdp <- get_ucdp_ged(stable_version)
latest_ucdp_version <- function(type = "stable"){
  first_number <- function(d) d |> lubridate::year() - 2000
  last_number <- function(d) d |> lubridate::month()
  search_dates <- seq.Date(Sys.Date() %m-% months(6), Sys.Date(), by = "month")
  if(type == "stable"){
    versions <- paste(unique(first_number(search_dates)), 1, sep = ".")
  } else if(type == "candidate"){
    versions <- paste(first_number(search_dates), 0, last_number(search_dates), sep = ".")
  } else{
    stop("Type must be 'stable' or 'candidate'")
  }

  versions <- sapply(versions, check_ucdp_version)
  versions <- versions[versions==TRUE]
  return(names(versions[length(versions)]))
}

get_ucdp_ged_uncached <- function(version){
  req <- httr2::request("https://ucdpapi.pcr.uu.se/api/") |>
    httr2::req_url_path_append("gedevents/") |>
    httr2::req_url_path_append(version) |>
    httr2::req_url_query("pagesize" = 1000000) # UCDP GED 23.1 has 316 818 observations, so this should be good for a while
  httr2::req_perform(req)
  resp <- httr2::last_response() |> httr2::resp_body_json()
  dat <- resp$Result |> dplyr::bind_rows()
  return(dat)
}

cd <- cachem::cache_disk(rappdirs::user_cache_dir("R-poldat"))

#' Get the UCDP GED data from their API
#'
#' See https://ucdp.uu.se/apidocs/ for more information
#'
#' The function cache the results using memoise::memoise.
#'
#' @param version A character string denoting the version of UCDP GED you want.
#'
#' @return A data frame with UCDP GED data.
#' @export
#'
#' @examples
#' stable_version <- latest_ucdp_version(type = "stable")
#' ucdp <- get_ucdp_ged(stable_version)
get_ucdp_ged <- memoise::memoise(get_ucdp_ged_uncached, cache = cd)
