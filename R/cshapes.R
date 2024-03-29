#' Modifies certain codings of the cShapes dataset
#'
#' @description
#' See each parameter for the modifications. The point of these modification is not that they
#' are clearly better ways to code the state system. They just represent alternative ways
#' to impose a strict system on what is arguably much more amorphous and complex. Some
#' of these changes do represent what I believe are coding errors, however.
#'
#' These modifications are not part of or associated with the cShapes team at ETH Zürich.
#'
#' Notes (i.e., other potential modifications):
#' - This does not add microstates (< 250 000 population) to the system. http://ksgleditsch.com/data/microstates.txt.
#' - Chinese occupation of Tibet is not coded. Tibet becomes a part of China in 1950 according to cShapes.
#' - Occupations during WWII are generally not coded in GW. See the COW data instead.
#'
#' See https://icr.ethz.ch/data/cshapes/
#'
#' Please cite this article when using the data:
#' Schvitz, Guy, Seraina Rüegger, Luc Girardin, Lars-Erik Cederman, Nils Weidmann, and Kristian Skrede Gleditsch. 2022.
#' “Mapping The International System, 1886-2017: The CShapes 2.0 Dataset.” Journal of Conflict Resolution 66(1): 144–61.
#'
#' You can also cite this article:
#' Kristian S. Gleditsch and Michael D. Ward, “A Revised List of Independent States since the Congress of Vienna,”
#' International Interactions 25, no. 4 (December 1999): 393–413, https://doi.org/10.1080/03050629908434958.
#'
#' @param western_sahara Morocco is coded without Spanish Sahara, and Spanish Sahara becomes Western Sahara after 13 Nov 1975.
#'  cShapes codes the whole of Spanish Sahara as becoming a part of Morocco from Nov 1975.
#'  Although Morocco did occupy parts of Spanish Sahara in 1975, the fight against the Polisario
#'  Front (Western Sahara) ended in a cease-fire in 1991 and territorial claims have never
#'  been resolved. Fighting resumed in 2020. Morocco now controls most of Western Sahara,
#'  and importantly the costal areas ("Southern Provinces").
#' @param morocco_protectorate Recodes Morocco from "N/A" to "protectorate" from 1904-01-02.
#'  The only use of "N/A" in cshapes is Morocco from 1904-1912. The Entente Cordiale of 1904,
#'  the Algeciras Conference in 1906, the Agadir incidence in 1911, and the Treaty of Fez in 1912
#'  pulls Morocco into France's sphere of influence. Although not formally a protectorate
#'  until the Treaty of Fez, it is not unreasonable to code this as a protectorate instead
#'  of "N/A" from 1904.
#' @param palestine Israel is not granted Gaza, West Bank or Sinai due to length of occupation.
#'  After 6-day war, Israel becomes occupant ("owner") of West Bank and Gaza, instead of gaining territories as coded in cShapes.
#'  Gleditsch-Ward has a 10-year occupation rule saying that any territory occupied more than 10 years falls
#'  to the occupier. In cShapes, this means that Gaza, West Bank, and Sinai becomes part of Israel
#'  from 1967 until 1979 (when Egypt formally got Sinai back, although troops stayed until Jan 1982).
#'  Palestine is given the GW code 699.
#' @param soviet_25dec Moves the dissolution of Soviet from 21 Dec 1991 to 25 Dec 1991.
#'  A number of dates could be used to denote the dissolution of Soviet. 21 December was
#'  the date of the Alma-Ata protocol. 25 Dec was the date of Gorbachev's speech, whilst
#'  26 Dec was when Soviet recognized Ukraine, Armenia, Azerbaijan, and Kazakhstan. The issue
#'  with using 21 Dec as the date of Soviet dissolution is that cShapes codes the start of the
#'  new states from 26 Dec. So, there are five days where Ukraine, Armenia, Azerbaijan, and Kazakhstan
#'  territories have no state control according to cShapes. In certain applications, having full
#'  temporal contiguity is very useful. So this is a practical code change, more than anything.
#'  (E.g., Ukraine celebrates their independence day on 24 August, not 26 December.)
#' @param yugoslavia_4jun Moves the dissolution of Yugoslavia to 4 June 2006, the start of Montenegro and Serbia to 5 June.
#'  Montenegro declared independence on 3 June, whilst Serbia declared independence 5 June.
#'  cShapes 2.0 codes the end of Yugoslavia 2006-06-02, the start of Serbia 2006-06-06
#'  and the start of Montenegro 2006-06-12. GW codes the end of Yugoslavia 2006-06-04, the start of Serbia 2006-06-05,
#'  and the start of Montenegro 2006-06-03. Using the dissolution of the larger unit is consistent with how Soviet is treated.
#' @param kosovo_17feb Moves the start date of Kosovo to 2008-02-17. This is probably just a coding error (in cshapes 2.0
#'  it is 2008-02-20). Kosovo unilaterally declared independence 17 Feb, and that is also the start date in GW.
#' @param senegal_22sep Senegal withdrew from the Mali Federation 20 September 1960, not 21 June 1960. Could possibly have
#'  confused the date with when the Mali Federation became independent from France (20 June 1960). The Declaration of the
#'  Republic of Mali happened 22 September 1960, which is consistent with start dates of independent Mali in both cShapes
#'  and GW. To be consistent with other coding (e.g., Soviet Union), move start of Senegal from 20 September to 22 September.
#'  The geometry used to represent Mali Federation from June 1960 to September 1960 (fid = 236) appears to be wrong, and is
#'  swapped with fid = 235.
#' @param kuwait_protectorate Before independence, Kuwait was a British Protectorate. This is not present in cShapes 2.0. This
#'  adds this. Uqair Protocol of 1922 defined borders between Mandatory Iraq, Nejd, and Kuwait. Before then, the borders where
#'  disputed, with the Kuwait-Najd War breaking out after the Ottoman Empire was defeated in WW1 and the Anglo-Ottoman Convention of
#'  1913 was invalidated. Kuwait existed as a British protectorate from 23 January 1899, but this was disputed by the Ottomans. Note
#'  that this still leaves a stretch between 1914 - 1922 without any coding of this territory. But then, the Anglo-Ottoman Convention
#'  of 1913 does not make it easy for coders.
#' @param bahrain_protectorate Before independence, Bahrain was a British Protectorate. This is not present in cShapes 2.0. This
#'  adds this. On 22 December 1880, Britain signed a treaty with Bahrain that could be argued to be the start of a protectorate. A
#'  treaty in 1892 further limited Bahrain's power, and in the Anglo-Ottoman convention of 1913, the Ottomans renounced all claims
#'  they had to Bahrain. Use 22 December 1880 as the start date for the British Protectorate. Earlier dates could be
#'  possible start dates, e.g., May 1861 (first treaty with Britain) and 2 December 1869 (start of Isa Bin Ali Al Khalifa's reign).
#' @param france_overseas Makes overseas departments and regions a part of France. cShapes 2.0 codes these as colonies.
#'  Data compilations such as WDI, PWT, and WCDE, e.g., population and GDP, include overseas departments in the figures for
#'  France. Overseas departments have the same laws and rights as mainland France, and are part of the EU.
#'
#' @param ... Additional parameters
#' @returns sf tibble with all country borders over time
#' @export
#'
#' @examples
#' gw_mod <- cshp_gw_modifications()
#' gw <- cshapes::cshp(useGW = TRUE)
#' gw |> dplyr::filter(country_name %in% c("Morocco", "Spanish Sahara", "Western Sahara"))
#' gw_mod |> dplyr::filter(country_name %in% c("Morocco", "Spanish Sahara", "Western Sahara"))
#'
cshp_gw_modifications <- function(western_sahara = TRUE,
                               morocco_protectorate = TRUE,
                               palestine = TRUE,
                               soviet_25dec = TRUE,
                               yugoslavia_4jun = TRUE,
                               kosovo_17feb = TRUE,
                               senegal_22sep = TRUE,
                               kuwait_protectorate = TRUE,
                               bahrain_protectorate = TRUE,
                               france_overseas = TRUE,
                               ...){

  gw <- cshapes::cshp(useGW = TRUE, dependencies = TRUE)
  gw$owner <- as.numeric(gw$owner)

  if(morocco_protectorate){
    # gw |> dplyr::filter(status == "N/A")
    gw <- gw |> dplyr::mutate(status = dplyr::if_else(.data$status == "N/A", "protectorate", .data$status))
  }

  if(western_sahara){
    gw <- gw |>
      dplyr::mutate(end = dplyr::if_else(.data$country_name == "Morocco" & .data$fid == 441, lubridate::ymd("2019-12-31"), .data$end)) |>
      dplyr::filter(!(.data$gwcode == 600 & .data$start > lubridate::ymd("1975-11-13"))) # Remove subsequent coding (442, 443)

    # Spanish Sahara becomes Western Sahara and is occupied by Morocco
    western_sahara <- gw |> dplyr::filter(.data$gwcode == 609)
    western_sahara$start <- lubridate::ymd("1975-11-14")
    western_sahara$end <- lubridate::ymd("2019-12-31")
    western_sahara$country_name <- "Western Sahara"
    western_sahara$status <- "occupied"
    western_sahara$fid <- 449
    western_sahara$owner <- 600 # Morocco

    gw <- dplyr::bind_rows(gw, western_sahara)
  }

  if(palestine){
    #### Israel without acknowledging occupations after the Palestinian Civil War ####
    combined_area <- gw |> dplyr::filter(.data$country_name %in% c("West Bank", "Gaza")) |> sf::st_union()
    west_bank_and_gaza <- gw |> dplyr::filter(.data$country_name == "West Bank") # Use as template
    west_bank_and_gaza$country_name <- "Palestine"
    west_bank_and_gaza$start <- lubridate::ymd("1967-06-10")
    west_bank_and_gaza$end <- lubridate::ymd("2019-12-31")
    west_bank_and_gaza$owner <- 666 # occupied by Israel
    west_bank_and_gaza$gwcode <- 699
    west_bank_and_gaza$fid <- 700
    sf::st_geometry(west_bank_and_gaza) <- NULL
    west_bank_and_gaza <- sf::st_sf(west_bank_and_gaza, geometry = combined_area)

    gw <- dplyr::bind_rows(gw, west_bank_and_gaza)

    gw <- gw |> dplyr::mutate( end = dplyr::if_else(.data$country_name == "Israel" & .data$fid == 512, lubridate::ymd("2019-12-31"), .data$end)) |>
      dplyr::filter(!(.data$gwcode == 666 & .data$fid > 512)) # Not code Israels occupations as part of Israel
    gw <- gw |> dplyr::mutate( end = dplyr::if_else(.data$country_name == "Egypt" & .data$fid == 492, lubridate::ymd("2019-12-31"), .data$end)) |>
      dplyr::filter(!(.data$gwcode == 651 & .data$start > lubridate::ymd("1967-06-09"))) # Not code Israel as owner of Sinai during occupation

  }

  if(soviet_25dec){
    #gw |> dplyr::filter(start == as.Date("1991-12-26"))
    #gw |> dplyr::filter(end == as.Date("1991-12-20"))
    #gw |> dplyr::filter(start == as.Date("1991-12-21"))
    # Let Soviet survive 5 more days. (This deviates from GW list, but so do cShapes)
    gw <- gw |> dplyr::mutate(end = dplyr::if_else(.data$end == as.Date("1991-12-20"), as.Date("1991-12-25"), .data$end))
    gw <- gw |> dplyr::mutate(start = dplyr::if_else(.data$start == as.Date("1991-12-21"), as.Date("1991-12-26"), .data$start))
  }

  if(yugoslavia_4jun){
    #gw |> dplyr::filter(gwcode == 341)
    #gw |> dplyr::filter(gwcode == 345)
    #gw |> dplyr::filter(gwcode == 340)
    gw <- gw |> dplyr::mutate(end = dplyr::if_else((.data$gwcode == 345 &
                                                   .data$fid == 139), as.Date("2006-06-04"), .data$end))
    gw <- gw |> dplyr::mutate(start = dplyr::if_else((.data$gwcode == 340 &
                                                      .data$fid == 126), as.Date("2006-06-05"), .data$start))
    gw <- gw |> dplyr::mutate(start = dplyr::if_else((.data$gwcode == 341 &
                                                      .data$fid == 131), as.Date("2006-06-05"), .data$start))

  }

  if(kosovo_17feb){
    # gw |> dplyr::filter(gwcode == 347)
    # gw |> dplyr::filter(gwcode == 340)
    gw <- gw |> dplyr::mutate(start = dplyr::if_else((.data$gwcode == 347 &
                                                        .data$fid == 141), as.Date("2008-02-17"), .data$start))
  }

  if(senegal_22sep){
    # gw |> dplyr::filter(gwcode == 433)
    # gw |> dplyr::filter(gwcode == 432)
    new_geom <- gw |> dplyr::filter(gwcode == 432, fid == 235) |> sf::st_geometry()
    gw <- gw |> dplyr::mutate(start = dplyr::if_else((.data$gwcode == 433 &
                                                        .data$fid == 239), as.Date("1960-09-22"), .data$start),
                              geometry = dplyr::if_else((.data$gwcode == 432 &
                                                           .data$fid == 236), new_geom, .data$geometry))

  }

  if(kuwait_protectorate){
    # gw[sf::st_overlaps(gw |> dplyr::filter(gwcode == 690), gw, sparse = FALSE),]
    kuwait <- gw |> dplyr::filter(.data$gwcode == 690, .data$fid == 524)
    kuwait$start <- as.Date("1922-12-02")
    kuwait$end <- as.Date("1961-06-18")
    kuwait$status <- "protectorate"
    kuwait$owner <- 200
    kuwait$fid <- 523

    gw <- dplyr::bind_rows(gw, kuwait)
  }

  if(bahrain_protectorate){
    # gw[sf::st_overlaps(gw |> dplyr::filter(gwcode == 692), gw, sparse = FALSE),]
    bahrain <- gw |> dplyr::filter(.data$gwcode == 692, .data$fid == 525)
    bahrain$start <- as.Date("1880-12-22")
    bahrain$end <- as.Date("1971-08-14")
    bahrain$status <- "protectorate"
    bahrain$owner <- 200
    bahrain$fid <- 524

    gw <- dplyr::bind_rows(gw, bahrain)
  }

  if(france_overseas){
    # Mayotte (31 March 2011) are not part of cShapes.
    overseas <- gw |> dplyr::filter(.data$country_name %in% c("Reunion", "Martinique", "Guadeloupe"))
    french_guyana <- gw |> dplyr::filter(gwcode == 120, fid == 49)
    overseas <- dplyr::bind_rows(overseas, french_guyana)
    france <- gw |> dplyr::filter(.data$gwcode == 220, .data$fid == 80)
    combined_area <- dplyr::bind_rows(france, overseas) |> sf::st_union() # This returns multipolygon and a linestring...
    france <- sf::st_sf(france, geometry = combined_area[2])
    france$start <- as.Date("1946-03-19")
    france$fid <- 81

    gw <- gw |> dplyr::mutate(end = dplyr::if_else( (.data$gwcode == 220 & .data$fid == 80), as.Date("1946-03-18"), .data$end))
    gw <- gw |> dplyr::mutate(end = dplyr::if_else( (.data$gwcode == 65 & .data$fid == 26), as.Date("1946-03-18"), .data$end))
    gw <- gw |> dplyr::mutate(end = dplyr::if_else( (.data$gwcode == 66 & .data$fid == 27), as.Date("1946-03-18"), .data$end))
    gw <- gw |> dplyr::mutate(end = dplyr::if_else( (.data$gwcode == 585 & .data$fid == 434), as.Date("1946-03-18"), .data$end))

    gw <- gw |> dplyr::filter(!(gwcode == 120 & fid == 49)) # remove French Guyana as colony from 19 March 1946

    gw <- dplyr::bind_rows(gw, france)

  }

  gw$gwcode <- as.numeric(gw$gwcode)
  return(gw)
}

#' Creates a network graph of territorial dependencies from the cShapes Gleditsch-Ward data
#'
#' @description
#' Any country that contains or overlaps with another country in space, and is adjacent (1 day) in time
#' are defined as neighbors. From this list of neighbors, a network graph is built.
#'
#' @param gw The cShapes data based on Gleditsch-Ward.
#'  Using cshp_gw_modifications() with soviet_25dec = TRUE is recommended.
#' @param hashsum Hidden parameter that provides the hash digest of the data object. Used o ensure memoisation works correctly.
#' @returns An igraph network graph
#'
#' @examples
#' gw <- cshp_gw_modifications(soviet_25dec = TRUE)
#' g <- territorial_dependencies(gw)
#'
territorial_dependencies_base <- function(gw, hashsum){
  sf::sf_use_s2(FALSE)
  gw$uid <- paste(gw$gwcode, gw$fid, sep = "-")

  g <- igraph::make_empty_graph()
  g <- igraph::add_vertices(g,
                            nrow(gw),
                            name = gw$uid,
                            gwcode = gw$gwcode,
                            fid = gw$fid,
                            start = gw$start,
                            end = gw$end,
                            cname = gw$country_name)

  case_when_there_are_earlier_neighbors <- dplyr::tibble()
  case_when_there_are_later_neighbors <- dplyr::tibble()

  for(i in 1:nrow(gw)){
    obs <- gw[i,]
    res <- gw |> dplyr::filter(((.data$start - obs$end) == 1) | ((obs$start - .data$end) == 1))
    contains <- sf::st_contains(obs, res, sparse = F)
    overlaps <- sf::st_overlaps(obs, res, sparse = F)
    idx <- contains | overlaps
    res <- res[idx,] |> dplyr::filter( (.data$uid != obs$uid) )

    if(nrow(res) > 0){
      earlier <- res |> dplyr::filter(.data$start < obs$start)
      later <- res |> dplyr::filter(.data$start >= obs$end)

      if(nrow(earlier) > 0){
        intersection <- sf::st_intersection(earlier, obs)
        intersection_area <- sf::st_area(intersection)
        #intersection_share_of_origin <- (intersection_area / sf::st_area(earlier) ) |> as.numeric()
        intersection_area <- intersection_area |> as.numeric()

        destination <- igraph::V(g)[igraph::V(g)$name %in% obs$uid]
        origin <- igraph::V(g)[igraph::V(g)$name %in% earlier$uid]
        sg <- dplyr::tibble("from" = origin$name, "to" = destination$name, "end" = earlier$end |> as.character(), "a_i" = intersection_area)
        case_when_there_are_earlier_neighbors <- dplyr::bind_rows(case_when_there_are_earlier_neighbors, sg)
        rm(intersection_area)
      }
      if(nrow(later) > 0){
        intersection <- sf::st_intersection(later, obs)
        intersection_area <- sf::st_area(intersection)
        #intersection_share_of_origin <- (intersection_area / sf::st_area(obs)) |> as.numeric()
        intersection_area <- intersection_area |> as.numeric()

        destination <- igraph::V(g)[igraph::V(g)$name %in% later$uid]
        origin <- igraph::V(g)[igraph::V(g)$name %in% obs$uid]
        sg <- dplyr::tibble("from" = origin$name, "to" = destination$name, "end" = obs$end |> as.character(), "a_i" = intersection_area)
        case_when_there_are_later_neighbors <- dplyr::bind_rows(case_when_there_are_later_neighbors, sg)
        rm(intersection_area)
      }
    }
  }


  edges <- dplyr::bind_rows(case_when_there_are_later_neighbors,
                            dplyr::anti_join(case_when_there_are_earlier_neighbors, case_when_there_are_later_neighbors, by = c("from", "to", "end")))
  # An interesting case where Mauritania and Spanish Western Sahara overlaps but has 0 area intersection
  # Drop this relation as the spatial overlap is 0.
  # gw <- cshapes::cshp(dependencies = TRUE)
  # gw$uid <- paste(gw$gwcode, gw$fid, sep = "-")
  # gw |> filter(uid %in% c("435-251", "610-450")) |> sf::st_overlaps()
  # gw |> filter(uid %in% c("435-251", "610-450")) |> sf::st_intersection() |> filter(n.overlaps == 2) |> sf::st_area()
  edges <- edges |> dplyr::filter(.data$a_i > 0)

  sg <- edges |> igraph::graph_from_data_frame()

  g <- igraph::make_empty_graph()
  g <- igraph::add_vertices(g, nrow(gw),
                    name = gw$uid,
                    gwcode = gw$gwcode,
                    fid = gw$fid,
                    start = gw$start,
                    end = gw$end,
                    cname = gw$country_name,
                    owner = gw$owner,
                    area = sf::st_area(gw$geometry) |> as.numeric())
  g <- g + sg
  return(g)
}

#' @describeIn territorial_dependencies_base Creates a network graph of territorial dependencies from the cShapes Gleditsch-Ward data
#'
#' @export
territorial_dependencies <- function(gw){
  hashsum <- digest::digest(gw)
  territorial_dependencies_memoised <- memoise::memoise(territorial_dependencies_base, cache = cachem::cache_disk(rappdirs::user_cache_dir("R-poldat")))
  g <- territorial_dependencies_memoised(gw)
  return(g)
}


#' Find territorial dependencies of a specific gwcode given a cShapes like dataset
#'
#'
#' @param gwcode An integer or string with the gwcode of the country you want to find dependencies for
#' @param gw The cShapes data based on Gleditsch-Ward.
#' @returns An igraph graph
#' @export
#'
#' @examples
#' gw <- cshp_gw_modifications()
#' system_which_includes_russia <- find_territorial_dependencies(365, gw)
#' plot(system_which_includes_russia)
#'
find_territorial_dependencies <- function(gwcode, gw){
  g <- territorial_dependencies(gw)
  dg <- igraph::decompose(g, "weak")
  for(i in 1:length(dg)){
    uids <- (igraph::V(dg[[i]]) |> names())
    gwcodes <- stringr::str_extract(uids, "[0-9]*")
    system_found <- gwcode %in% gwcodes
    if(system_found){
      return(dg[[i]])
    }
  }
  return(paste(gwcode, "not found"))
}

#' Create a panel dataset from the cShapes Gleditsch-Ward data
#'
#' @description
#' A panel data (e.g., country-year) is created from the cShapes data. You can specify the time-interval
#' based on the seq.Date(by=) and a start and end time. If the end is after the end in cShapes, it is
#' assumed that borders have not changed since last update of cShapes. Only the entities that existed on the last day of the
#' interval (in each interval) are returned to avoid duplicates.
#'
#' @param gw The cShapes data based on Gleditsch-Ward.
#' @param time_interval String compatible with the seq.Date(by)-parameter
#' @param begin A date of the start of the panel
#' @param stop A date of the end of the panel. Defaults to Sys.Date(). It only returns full units, so if the interval is week,
#'  and it is Wednesday, the last week will be the last included.
#' @param static_date If not null, gw_panel will return a static/balanced time-series data using the world as it was at the
#'  static_date as template for all time-periods from begin to stop.
#' @returns A panel-date tibble
#' @export
#'
#' @examples
#' gw <- cshp_gw_modifications()
#' df <- gw_panel(gw, time_interval = "week", begin = as.Date("2024-01-01"), stop = Sys.Date())
#'
gw_panel <- function(gw, time_interval = "year", begin = NULL, stop = Sys.Date(), static_date = NULL){
  sf::st_geometry(gw) <- NULL
  gw <- gw |> dplyr::mutate(exist_interval = lubridate::interval(.data$start, .data$end))

  # Setting static_date freezes the world borders at the time of static_date over the begin-stop period.
  if(!is.null(static_date)){
    gw <- gw |> dplyr::filter(static_date %within% .data$exist_interval)

    # Pretend that states existed from begin to stop.
    if(is.null(begin)){
      gw <- gw |> dplyr::mutate(start = min(.data$start))
    } else{
      gw <- gw |> dplyr::mutate(start = begin)
    }
    if(is.null(stop)){
      gw <- gw <- dplyr::mutate(end = max(.data$stop))
    } else{
      gw <- gw |> dplyr::mutate(end = stop)
    }
  }

  if(stop > max(gw$end)){
    if(stop > max(gw$end)){
      gw <- gw |> dplyr::mutate(end = dplyr::if_else(.data$end == max(.data$end), stop, .data$end))
    }
    gw <- gw |> dplyr::filter(.data$start <= stop)
  }

  gw <- gw |> dplyr::mutate(exist_interval = lubridate::interval(.data$start, .data$end))
  if(!is.null(begin)){
    gw <- gw |> dplyr::mutate(start = dplyr::if_else(begin %within% .data$exist_interval, begin, .data$start))
    gw <- gw |> dplyr::filter(.data$start >= begin)
  }

  res <- gw |>
    dplyr::rowwise() |>
    dplyr::mutate(mydate = list(seq(.data$start, .data$end, by = time_interval))) |>
    tidyr::unnest(.data$mydate) |>
    dplyr::arrange(.data$gwcode, .data$mydate)

  if(time_interval == "year"){
    res <- res |> dplyr::mutate(year = lubridate::year(.data$mydate))
    res <- res |> dplyr::mutate(maxdate = ISOdate(.data$year, 12, 31) |> as.Date())
  }
  if(time_interval == "month"){
    res <- res |> dplyr::mutate(year = lubridate::year(.data$mydate),
                         month = lubridate::month(.data$mydate))
    res <- res |> dplyr::mutate(maxdate = zoo::as.Date(zoo::as.yearmon(paste(.data$year, .data$month, sep = "-")), frac = 1))
  }
  if(time_interval == "quarter"){
    res <- res |> dplyr::mutate(year = lubridate::year(.data$mydate),
                         quarter = lubridate::quarter(.data$mydate))
    res <- res |> dplyr::mutate(maxdate = zoo::as.Date(zoo::as.yearqtr(paste(.data$year, .data$quarter, sep = "-")), frac = 1))
  }
  if(time_interval == "week"){
    res <- res |> dplyr::mutate(year = lubridate::year(.data$mydate),
                         week = lubridate::week(.data$mydate))
    res <- res |> dplyr::mutate(maxdate = ISOdate(.data$year, 1, 1)  |> as.Date() + lubridate::days(.data$week*7-1))
  }
  if(time_interval == "day"){
    res <- res |> dplyr::mutate(year = lubridate::year(.data$mydate),
                         month = lubridate::month(.data$mydate),
                         day = lubridate::day(.data$mydate))
    res <- res |> dplyr::mutate(maxdate = ISOdate(.data$year, .data$month, .data$day)  |> as.Date())
  }

  res <- res |> dplyr::filter(.data$maxdate < Sys.Date(), .data$maxdate %within% exist_interval)
  return(res |> dplyr::select(-.data$mydate, -.data$exist_interval))
}
