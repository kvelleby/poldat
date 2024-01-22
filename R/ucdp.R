#' Test if a version number exists in the UCDP API
#'
#' See https://ucdp.uu.se/apidocs/ for more information
#'
#' @param version A character string
#' @param dataset Which dataset to search. One of c("gedevents", "ucdpprioconflict", "dyadic", "nonstate", "onesided", "battledeaths")
#'
#' @return A boolean
#' @export
#'
#' @examples
#' versions <- c("23.1", "26.2", "23.0.10")
#' versions <- sapply(versions, check_ucdp_version)
check_ucdp_version <- function(version, dataset = "gedevents"){
  req <- httr2::request("https://ucdpapi.pcr.uu.se/api/") |>
    httr2::req_url_path_append(paste0(dataset, "/")) |>
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

#' Find the latest version of the UCDP data
#'
#' See https://ucdp.uu.se/apidocs/ for more information
#'
#' @param dataset Which dataset to search. One of c("gedevents", "ucdpprioconflict", "dyadic", "nonstate", "onesided", "battledeaths")
#' @param type Either 'stable' or 'candidate' (only relevant for "gedevents" dataset)
#'
#' @return A character string representing the latest version of UCDP data.
#' @export
#'
#' @examples
#' stable_version <- latest_ucdp_version(type = "stable")
#' ged <- get_ucdp(stable_version)
latest_ucdp_version <- function(dataset = "gedevents", type = "stable"){
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

  versions <- sapply(versions, check_ucdp_version, dataset = dataset)
  versions <- versions[versions==TRUE]
  return(names(versions[length(versions)]))
}

#' Get the UCDP data from their API
#'
#' See https://ucdp.uu.se/apidocs/ for more information
#'
#' The function cache the results using memoise::memoise.
#'
#' @param version A character string denoting the version of UCDP GED you want.
#' @param dataset Which dataset to search. One of c("gedevents", "ucdpprioconflict", "dyadic", "nonstate", "onesided", "battledeaths")
#'
#' @return A data frame with UCDP GED data.
#'
#' @examples
#' stable_version <- latest_ucdp_version(type = "stable")
#' ged <- get_ucdp(stable_version)
get_ucdp_uncached <- function(version, dataset = "gedevents"){
  req <- httr2::request("https://ucdpapi.pcr.uu.se/api/") |>
    httr2::req_url_path_append(paste0(dataset, "/")) |>
    httr2::req_url_path_append(version) |>
    httr2::req_url_query("pagesize" = 1000000) # UCDP GED 23.1 has 316 818 observations, so this should be good for a while
  httr2::req_perform(req)
  resp <- httr2::last_response() |> httr2::resp_body_json()
  dat <- resp$Result |> dplyr::bind_rows()
  return(dat)
}

#' @describeIn get_ucdp_uncached Get the UCDP GED data from their API
#'
#' @export
get_ucdp <- memoise::memoise(get_ucdp_uncached, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))
