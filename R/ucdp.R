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


#' Distributes a count over n periods as uniformly as possible
#'
#' Distributes from start to end, but ensures that there is at least one in the last observation if possible.
#'
#' @param count A positive integer of the count that should be distributed
#' @param n Number of periods to distribute over
#'
#' @return A vector of distributed counts
#' @export
#'
#' @examples
#' distribute_count(3, 4)
#' distribute_count(NA, 3)
#' distribute_count(8, 3)
distribute_count <- function(count, n){
  minimal_baseline <- rep(count %/% n, n)

  if(is.na(count)){
    return(rep(NA, n))
  }

  if(count != 0){
    residual <- rep(1, count %% n)
    lr <- length(residual)
    residual <- c(residual, rep(0, length(minimal_baseline) - length(residual)))
    result <- minimal_baseline + residual

    # Make sure the last observation includes a (minimal) positive count
    if(sum(minimal_baseline) == 0 & count != 1){
      idx <- 1:length(result)
      idx[lr] <- length(result)
      idx[length(result)] <- lr
      result <- result[idx]
    }

  } else{
    result <- minimal_baseline
  }

  return(result)
}

#' Distributes/aggregates UCDP GED at the country-level over a chosen time-interval
#'
#' @param version String denoting the UCDP GED version to use.
#' @param time_interval String denoting the temporal interval to use.
#' @param state_based Boolean if state_based events should be included
#' @param non_state Boolean if non_state events should be included
#' @param one_sided Boolean if one_sided events should be included
#' @param drop_poor_precision Boolean if events with temporal precision poorer than time_interval should be included
#' @param test Boolean to run code with smaller sample, used for testing only
#' @param ... Additional parameters
#'
#' @seealso \code{\link[=seq.Date]{seq.Date()}} for valid time_interval strings.
#'
#' @return Data frame with longitude, latitude, country_id, average date precision and sum of battle-deaths.
#' @export
#' @examples
#' ged <- ucdp_ged_panel(version = "23.1",
#'                       time_interval = "year",
#'                       state_based = TRUE,
#'                       non_state = TRUE,
#'                       one_sided = TRUE,
#'                       drop_poor_precision = TRUE,
#'                       test = TRUE)
ucdp_ged_panel <- function(version, time_interval, state_based, non_state, one_sided, drop_poor_precision, test = FALSE, ...){
  drop_poor_temporal_precision <- function(df, time_interval){
    if(time_interval == "year"){
      return(df)
    } else if(time_interval %in% c("quarter", "month")){
      return(df |> dplyr::filter(.data$date_prec < 5))
    } else if(time_interval == "week"){
      return(df |> dplyr::filter(.data$date_prec < 4))
    } else if(time_interval == "day"){
      return(df |> dplyr::filter(.data$date_prec < 2))
    } else{
      stop("Invalid time_interval")
    }
  }

  ucdp_ged <- get_ucdp(version = version)

  if(test){
    ucdp_ged <- ucdp_ged[1:100,]
  }

  ucdp_ged <- ucdp_ged |>
    dplyr::mutate(date_start = as.Date(.data$date_start),
           date_end = as.Date(.data$date_end))

  if(!state_based){ucdp_ged <- ucdp_ged |> dplyr::filter(.data$type_of_violence != 1)}
  if(!non_state){ucdp_ged <- ucdp_ged |> dplyr::filter(.data$type_of_violence != 2)}
  if(!one_sided){ucdp_ged <- ucdp_ged |> dplyr::filter(.data$type_of_violence != 3)}

  res <- ucdp_ged |>
    dplyr::select(.data$latitude,
                  .data$longitude,
                  .data$where_prec,
                  .data$date_start,
                  .data$date_end,
                  .data$best,
                  .data$low,
                  .data$high,
                  .data$deaths_a,
                  .data$deaths_b,
                  .data$deaths_civilians,
                  .data$deaths_unknown,
                  .data$date_prec,
                  .data$country_id) |>
    dplyr::rowwise() |>
    dplyr::mutate(mydate = list(seq(.data$date_start, .data$date_end, by = "day"))) |>
    dplyr::mutate(best = list(distribute_count(.data$best, length(.data$mydate))),
                  low = list(distribute_count(.data$low, length(.data$mydate))),
                  high = list(distribute_count(.data$high, length(.data$mydate))),
                  deaths_a = list(distribute_count(.data$deaths_a, length(.data$mydate))),
                  deaths_b = list(distribute_count(.data$deaths_b, length(.data$mydate))),
                  deaths_civilians = list(distribute_count(.data$deaths_civilians, length(.data$mydate))),
                  deaths_unknown = list(distribute_count(.data$deaths_unknown, length(.data$mydate)))) |>
    tidyr::unnest(cols = c(.data$mydate,
                           .data$best,
                           .data$low,
                           .data$high,
                           .data$deaths_a,
                           .data$deaths_b,
                           .data$deaths_civilians,
                           .data$deaths_unknown)) |>
    dplyr::ungroup() |>
    dplyr::mutate(mydate = lubridate::floor_date(.data$mydate, unit = time_interval)) |>
    dplyr::group_by(.data$latitude, .data$longitude, .data$country_id, .data$mydate)

  if(drop_poor_precision){
    res <- drop_poor_temporal_precision(res, time_interval)
  }

  res <- res |> dplyr::summarize(best = sum(.data$best),
                                 low = sum(.data$low),
                                 high = sum(.data$high),
                                 deaths_a = sum(.data$deaths_a),
                                 deaths_b = sum(.data$deaths_b),
                                 deaths_civilians = sum(.data$deaths_civilians),
                                 deaths_unknown = sum(.data$deaths_unknown),
                                 date_prec = mean(.data$date_prec)) |>
    dplyr::ungroup()
  return(res)
}

#' Complete country-time panel data with UCDP GED and modified cShapes
#'
#' This will override the country_id used by UCDP, and instead use the gwcode in cShapes.
#' For events that fall outside of cShapes polygons, this function falls back to the UCDP gazetteer coding.
#' The benefit of using cShapes is that this makes it possible to see events happening in e.g., Western Sahara or Palestine.
#' It also makes the country-code consistent across various datasets that is based on cShapes. While UCDP country_id
#' and cShapes mostly align, there are a few cases where they do not. E.g., UCDP codes events in Namibia in 1989 when it was a
#' South African colony with the gwcode of South Africa, whilst cShapes uses the Namibia code.
#'
#' @inheritDotParams cshp_gw_modifications
#' @inheritDotParams ucdp_ged_panel
#'
#' @return Data frame with country information, and average date precision (of UCDP GED events) and sum of battle-deaths.
#'
#' @examples
#' ged <- gw_ged(western_sahara = TRUE,
#'                morocco_protectorate = TRUE,
#'                palestine = TRUE,
#'                soviet_25dec = TRUE,
#'                version = "23.1",
#'                time_interval = "year",
#'                state_based = TRUE,
#'                non_state = TRUE,
#'                one_sided = TRUE,
#'                drop_poor_precision = TRUE,
#'                test = TRUE)
gw_ged_uncached <- function(...){
  find_country <- function(df, gw, time_interval, lat  = "latitude", lon = "longitude"){
    sf::sf_use_s2(FALSE)
    mydate <- df$mydate |> dplyr::first() # This assumes mydate is unique.
    df <- df |> sf::st_as_sf(coords = c(lon, lat), crs = 4326)
    gw <- gw |> dplyr::filter(mydate %within% .data$exist_interval) |> dplyr::select(.data$gwcode)
    df <- sf::st_join(df, gw)
    return(df)
  }

  dots <- rlang::list2(...)

  gw <- cshp_gw_modifications(...)
  ged <- ucdp_ged_panel(...)

  ged_list <- split(ged, ged$mydate)
  gw$exist_interval <- lubridate::interval(gw$start, gw$end)

  res <- lapply(ged_list, find_country, gw = gw, time_interval = dots$time_interval)
  res <- dplyr::bind_rows(res) |>
    dplyr::mutate(gwcode = dplyr::if_else(is.na(.data$gwcode), .data$country_id, .data$gwcode)) |> # Some events do not overlap with cshapes, use UCDP coding instead for those.
    dplyr::select(.data$gwcode,
                  .data$mydate,
                  .data$best,
                  .data$low,
                  .data$high,
                  .data$deaths_a,
                  .data$deaths_b,
                  .data$deaths_civilians,
                  .data$deaths_unknown,
                  .data$date_prec) |>
    dplyr::group_by(.data$gwcode, .data$mydate) |>
    dplyr::summarize(best = sum(.data$best),
                     low = sum(.data$low),
                     high = sum(.data$high),
                     deaths_a = sum(.data$deaths_a),
                     deaths_b = sum(.data$deaths_b),
                     deaths_civilians = sum(.data$deaths_civilians),
                     deaths_unknown = sum(.data$deaths_unknown),
                     date_prec = mean(.data$date_prec))

  df <- gw_panel(gw, time_interval = dots$time_interval, begin = min(res$mydate), stop = lubridate::ceiling_date(max(res$mydate), unit = dots$time_interval)) |>
    dplyr::mutate(mydate = lubridate::floor_date(.data$maxdate, unit = dots$time_interval)) |>
    dplyr::left_join(sf::st_drop_geometry(res), by = c("gwcode", "mydate")) |>
    dplyr::mutate(best = dplyr::if_else(is.na(.data$best), 0, .data$best),
                  low = dplyr::if_else(is.na(.data$low), 0, .data$low),
                  high = dplyr::if_else(is.na(.data$high), 0, .data$high),
                  deaths_a = dplyr::if_else(is.na(.data$deaths_a), 0, .data$deaths_a),
                  deaths_b = dplyr::if_else(is.na(.data$deaths_b), 0, .data$deaths_b),
                  deaths_civilians = dplyr::if_else(is.na(.data$deaths_civilians), 0, .data$deaths_civilians),
                  deaths_unknown = dplyr::if_else(is.na(.data$deaths_unknown), 0, .data$deaths_unknown)) |>
    dplyr::select(.data$gwcode,
                  .data$country_name,
                  .data$status,
                  .data$owner,
                  .data$capname,
                  .data$caplong,
                  .data$caplat,
                  .data$b_def,
                  .data$fid,
                  .data$mydate,
                  .data$best,
                  .data$low,
                  .data$high,
                  .data$deaths_a,
                  .data$deaths_b,
                  .data$deaths_civilians,
                  .data$deaths_unknown,
                  .data$date_prec)

  return(df)
}

#' @describeIn gw_ged_uncached Get the UCDP GED data from their API
#'
#' @export
gw_ged <- memoise::memoise(gw_ged_uncached, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))
