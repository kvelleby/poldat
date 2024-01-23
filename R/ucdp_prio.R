#' Codes the gwcode of where majority of battles occurred in a conflict-dyad in the UCDP/PRIO ACD before 1989
#'
#' gwcodes are based on cShapes 2.0 (with modifications in `cshp_gw_modifications'). This coding is
#' using UCDP/PRIO ACD version 23.1. Supplement with data from UCDP GED after 1989.
#'
#' Coded by Jonas Vestby, 22.01.2024. All errors are mine.
#'
#' @return A data frame with UCDP/PRIO ACD data version 23.1 plus variable "battle_loc".
#'
#' @examples
#' ucdp_prio <- ucdp_prio_battle_locations_before_1989()
ucdp_prio_battle_locations_before_1989 <- function(){

  # gw <- cshp_gw_modifications()
  ucdp_prio <- get_ucdp(version = "23.1", dataset = "ucdpprioconflict")
  ucdp_prio$year <- ucdp_prio$year |> as.numeric()
  ucdp_prio$conflict_id <- ucdp_prio$conflict_id |> as.numeric()

  # Where did the fighting happen here?
  ## Interstate conflicts over government before 1989: Need manual coding
  # ucdp_prio |> filter(type_of_conflict == 2, incompatibility == 2, year < 1989) |> View()

  type22 <- dplyr::bind_rows(
    list(conflict_id = 250, battle_loc = "310"), # The Hungarian Uprising in 1956 was fought in Hungary
    list(conflict_id = 350, battle_loc = "55"), # The US invasion of Grenada in 1983 was fought in Grenada.
    list(conflict_id = 431, battle_loc = "700") # The Soviet - Afghan war was mainly fought in Afghanistan.
  )
  type22$type_of_conflict = 2
  ## Interstate conflicts over territory before 1989: Need manual coding but can be based on territory_name?
  # ucdp_prio |> filter(type_of_conflict == 2, incompatibility == 1, year < 1989) |> View()
  # ucdp_prio |> filter(type_of_conflict == 2, incompatibility == 1, year < 1989) |> select(conflict_id, location, start_date, ep_end_date, year, gwno_loc, territory_name) |> View()

  type2 <- dplyr::bind_rows(
    list(conflict_id = 11343, battle_loc = "651, 6511"), # The Six Day War. Egypt - Israel over Suez/Sinai. Gaza is also part here.
    list(conflict_id = 214, battle_loc = "811"), # Thailand - French Indochina battles over Northern Cambodia.
    list(conflict_id = 215, battle_loc = "339"), # Corfu Channel, battles at sea in the Corfu Channel.
    list(conflict_id = 218, battle_loc = "7506, 7708"), # Jammu and Kashmir and Kashmir (North)
    list(conflict_id = 226, battle_loc = "750, 751"), # gwcode 751 Hyderabad is only mentioned in the case descriptions for the GW list, but is not part of the list. 750 in cshapes includes Hyderabad.
    list(conflict_id = 228, battle_loc = "665, 666, 6511, 6631"), # Egypt, Iraq, Israel, Jordan, Lebanon, Syria are mentioned here, but figthing mainly happened in the Palestine Mandate (and after May 1948 in Gaza + West Bank). Israel did not exist at the start of conflict...
    list(conflict_id = 235, battle_loc = "731, 732"), # Korean war was fought all over the Korean Peninsula, with changing fronts over time.
    list(conflict_id = 334, battle_loc = "710, 816"), # Sino-Vietnamese skirmishes, both in Vietnam and China, as sea, and in Cambodia/Laos? Mostly along the Sino-Vietnamese border.
    list(conflict_id = 239, battle_loc = "651"), # The Suez Emergency, fought in Egypt.
    list(conflict_id = 252, battle_loc = "651"), # The Suez Crisis was an Israeli (+UK+France) invasion of Egypt.
    list(conflict_id = 255, battle_loc = "91, 93"), # Battles on both sides of a disputed border between Nicaragua and Honduras.
    list(conflict_id = 268, battle_loc = "520, 530"), # First Ogaden War. Was fought both in Ethiopia and in Somalia.
    list(conflict_id = 272, battle_loc = "616"), # The Bizerte Crisis happened in Tunisia, where Tunisia imposed a blockade on a French naval base in Bizerte.
    list(conflict_id = 274, battle_loc = "710, 750"), # The Sino-Indian War mainly occurred in disptuted territories Aksai Chin (cShapes codes this as part of China) and (what ended up being) Arunachal Pradesh (cShapes codes this as part of India).
    list(conflict_id = 276, battle_loc = "851"), # Operation Trikora aimed to seize West Irian (Duch New Guinea) gwcode 851, and is now part of Indonesia. The territory is not part of Indonesia in cShapes before 1969 (gwcode == 850, fid == 638) after the Act of Free Choice plebiscite.
    list(conflict_id = 278, battle_loc = "600, 615"), # The Sand War between Algeria and Morocco
    list(conflict_id = 281, battle_loc = "820, 850, 823, 824"), # Indonesia - Malaysia confrontation, happened on Borneo (Sarawak, Sabah)
    list(conflict_id = 293, battle_loc = "816, 817"), # Vietnam war in both south and north.
    list(conflict_id = 294, battle_loc = "800, 811"), # Thailand, Cambodia borders
    list(conflict_id = 301, battle_loc = "666, 663, 6631"), # The Six Day War, Israel, Jordan and West Bank
    list(conflict_id = 302, battle_loc = "666, 652"), # The Six Day War. Israel, Syria (Golan Heights)
    list(conflict_id = 304, battle_loc = "710, 775"), # China - Myanmar border
    list(conflict_id = 305, battle_loc = "365, 710"), # Sino - Soviet border along Ussuri river.
    list(conflict_id = 306, battle_loc = "91, 92"), # The "Soccer War" between El Salvador and Honduras
    list(conflict_id = 323, battle_loc = "352"), # Turkey invasion of Cyprus
    list(conflict_id = 328, battle_loc = "811, 816"), # Cambodia - Vietnam border
    list(conflict_id = 346, battle_loc = "160"), # Falkland islands, not included in cshapes, but Argentina is the closest.
    list(conflict_id = 349, battle_loc = "475, 483"), # Chad - Nigeria over Lake Chad. Fighting on both sides of the border, mostly in Borno State.
    list(conflict_id = 355, battle_loc = "432, 439"), # The Agacher Strip war between Burkina Faso and Mali.
    list(conflict_id = 356, battle_loc = "800, 812"), # Laos - Thailand border conflict
    list(conflict_id = 361, battle_loc = "483"), # The Chadian -Libyan War. A bit strange that it is only coded for 1987 in the UCDP/PRIO data (1978-1987?). Most fighting in Chad.
    list(conflict_id = 427, battle_loc = "500, 510") # Uganda - Tanzania war. Fighting in the Kagera region in Tanzania and in Uganda.
  )
  type2$type_of_conflict = 2

  # Incompatibility over both territory and government.
  # ucdp_prio |> filter(type_of_conflict == 2, incompatibility == 3, year < 1989) |> select(conflict_id, location, gwno_loc, year) |> View()

  type23 <- dplyr::bind_rows(
    list(conflict_id = 232, battle_loc = "710, 713"), # First Taiwan Strait Crisis. Shelling of islands I'm not sure even is part of cShapes (close to China mainland).
    list(conflict_id = 320, battle_loc = "678, 680"), # North and South Yemen
    list(conflict_id = 324, battle_loc = "630, 645") # Iran - Irak Shatt al-Arab and subsequent Iran - Irak war.
  )
  type23$type_of_conflict = 2
  ## Extrasystemic conflicts are coded with the location of the state that was created after end of conflict (and might be a reasonable assumption)
  ## Certain violent events in certain years might have happened in the colonizer state?
  # ucdp_prio |> filter(type_of_conflict == 1, year < 1989) |> select(conflict_id, location, side_a, side_b, start_date, ep_end_date, year, gwno_loc, territory_name) |> View()

  type1 <- dplyr::bind_rows(
    list(conflict_id = 201, battle_loc = "811"), #Cambodia
    list(conflict_id = 204, battle_loc = "850"), # Indonesia
    list(conflict_id = 207, battle_loc = "665"), # The Irgun/IZL operated in Mandate Palestine before Israel existed.
    list(conflict_id = 208, battle_loc = "812"), # Laos
    list(conflict_id = 216, battle_loc = "815"), # Vietnam (North Vietnam)
    list(conflict_id = 219, battle_loc = "580"), # Madagascar
    list(conflict_id = 229, battle_loc = "820"), # Malaysia (it seems fighting did not include Sarawak and Sabah?)
    list(conflict_id = 238, battle_loc = "6"), # Utuado uprising in Puerto Rico. Puerto Rico has gwcode 6 in cshapes 2.0.
    list(conflict_id = 241, battle_loc = "501"), # Kenya
    list(conflict_id = 244, battle_loc = "600"), # Morocco
    list(conflict_id = 245, battle_loc = "616"), # Tunisia
    list(conflict_id = 246, battle_loc = "615"), # Algeria - a department of France at the time. Is coded as a colony in cshapes 2.0 (as even the current overseas departments are)
    list(conflict_id = 248, battle_loc = "352"), # Cyprus
    list(conflict_id = 254, battle_loc = "471"), # Cameroon
    list(conflict_id = 256, battle_loc = "435, 600"), # Mauritania, Morocco
    list(conflict_id = 257, battle_loc = "435, 600"), # Mauritania, Morocco
    list(conflict_id = 263, battle_loc = "540"), # Angola
    list(conflict_id = 273, battle_loc = "835"), # Brunei revolt
    list(conflict_id = 279, battle_loc = "404"), # Guinea-Bissau
    list(conflict_id = 285, battle_loc = "541"), # Mozambique
    list(conflict_id = 286, battle_loc = "6801") # South Yemen was in the Federation of South Arabia from 1962-1967.
  )
  type1$type_of_conflict = 1
  ## The pure intrastate conflicts should be relatively easy
  # ucdp_prio |> filter(type_of_conflict == 3, year < 1989) |> select(conflict_id, location, side_a, side_b, start_date, ep_end_date, year, gwno_loc, territory_name) |> View()
  # ucdp_prio |> filter(type_of_conflict == 3, year < 1989, grepl("Israel", location)) |> select(conflict_id, location, side_a, side_b, start_date, ep_end_date, year, gwno_loc, territory_name) |> View()

  # Most intrastate conflicts fight in the same gwcode as in gwno_loc, with a couple exceptions
  type3 <- ucdp_prio |>
    dplyr::filter(.data$type_of_conflict == 3, .data$year < 1989) |>
    dplyr::select(.data$conflict_id, battle_loc = .data$gwno_loc) |>
    dplyr::distinct()

  type3_changes <- dplyr::bind_rows(
    list(conflict_id = 234, battle_loc = "6511, 6631, 666, 699"), # The "civil-war" in Israel-Palestine was fought in Israel, Gaza, West Bank, and Palestine.
    list(conflict_id = 270, battle_loc = "220, 615"), # OAS launched attacks both in France and in Algeria (which was part of France...)
    list(conflict_id = 311, battle_loc = "600, 609"), # Fighting between Morocco and Polisario happened in both Western Sahara and in Morocco.
    list(conflict_id = 428, battle_loc = "435, 609") # Fighting between Mauritania and Polisario happened in both Western Sahara and in Mauritania
  )

  type3 <- dplyr::anti_join(type3, type3_changes, by = "conflict_id") |>
    dplyr::bind_rows(type3_changes)
  type3$type_of_conflict = 3
  ## The internationalized intrastate conflicts are likely to mainly have happened in the "intrastate" state.
  ## However, here you might find certain large events in other countries (e.g., al Qaida attack on the US)
  # ucdp_prio |> filter(type_of_conflict == 4, year < 1989) |> View() |> select(conflict_id, location, side_a, side_b, start_date, ep_end_date, year, gwno_loc, territory_name) |> View()

  # Most internationalized intrastate conflicts fight in the same gwcode as in gwno_loc, with a couple exceptions
  type4 <- ucdp_prio |>
    dplyr::filter(.data$type_of_conflict == 4, .data$year < 1989) |>
    dplyr::select(.data$conflict_id, battle_loc = .data$gwno_loc) |>
    dplyr::distinct()

  type4_changes <- dplyr::bind_rows(
    list(conflict_id = 234, battle_loc = "6511, 6631, 666, 699"), # Also the internationalized civil-war in Israel-Palestine was fought in Israel, Gaza, West Bank, and Palestine.
    list(conflict_id = 311, battle_loc = "600, 609") # Fighting between Morocco and Polisario happened in both Western Sahara and in Morocco.
  )
  type4 <- dplyr::anti_join(type4, type4_changes, by = "conflict_id") |>
    dplyr::bind_rows(type4_changes)
  type4$type_of_conflict = 4

  battle_loc <- dplyr::bind_rows(type1, type2, type22, type23, type3, type4)
  battle_loc$type_of_conflict <- battle_loc$type_of_conflict |> as.character()


  ucdp_prio_before_1989 <- battle_loc |>
    dplyr::right_join(ucdp_prio |> dplyr::filter(.data$year < 1989),
                      by = c("conflict_id", "type_of_conflict"),
                      relationship = "one-to-many")

  return(ucdp_prio_before_1989)
}

#' A country-year panel data with battle-locations and intensity from 1946-present
#'
#' Is based on cShapes 2.0, UCDP/PRIO ACD (1946-88) and UCDP GED (1989-present).
#'
#' Using modifications to cshapes using `cshp_gw_modifications()`.
#' Using re-coded battle-locations data from `ucdp_prio_battle_locations_before_1989()`
#'
#' Use UCDP GED point locations to determine country battle location based on the modified cShapes data
#' (instead of UCDP GED's own country_id code).
#'
#' Please cite:
#'
#' Gleditsch, Nils Petter; Peter Wallensteen, Mikael Eriksson, Margareta Sollenberg & Håvard Strand
#' (2002) Armed Conflict 1946–2001: A New Dataset. Journal of Peace Research 39(5): 615–637.
#'
#' Sundberg, Ralph, and Erik Melander, 2013, “Introducing the UCDP Georeferenced
#' Event Dataset”, Journal of Peace Research, vol.50, no.4, 523-532
#'
#' Schvitz, Guy, Seraina Rüegger, Luc Girardin, Lars-Erik Cederman, Nils Weidmann, and Kristian Skrede Gleditsch. 2022.
#' “Mapping The International System, 1886-2017: The CShapes 2.0 Dataset.” Journal of Conflict Resolution 66(1): 144–61.
#'
#' @param version A string denoting the version of UCDP GED to use to generate dataset.
#'
#' @return A data frame with country-year panel data with battle-locations and intensity from 1946-present
#'
#' @examples
#' df <- ucdp_long_cy_panel()
ucdp_long_cy_panel <- function(version = "23.1"){
  ucdp_prio <- ucdp_prio_battle_locations_before_1989()
  start_year <- as.Date(paste(min(ucdp_prio$year), "01-01", sep = "-"))

  ucdp_ged <- get_ucdp(version = version)
  end_year <- as.Date(paste(max(ucdp_ged$year)+1, "01-01", sep = "-"))

  # Not clear how to divide brds across locations...
  # old_brds <- get_prio_brd() |>
  #  dplyr::select(new_id, year, low = bdeadlow, best = bdeadbes, high = bdeadbes)

  gw <- cshp_gw_modifications()
  df <- gw_panel(gw, time_interval = "year", begin = start_year, stop = end_year)
  df <- df |> dplyr::left_join(gw |> dplyr::select(gwcode, fid, geometry), by = c("gwcode", "fid")) |> sf::st_as_sf(crs = 4326)

  #ucdp_prio <- ucdp_prio |> dplyr::left_join(old_brds, by = c("conflict_id" = "new_id", "year"))

  intensity_panel <- ucdp_prio |>
    dplyr::select(.data$battle_loc, .data$year, .data$intensity_level) |>
    tidyr::separate_rows(.data$battle_loc) |>
    dplyr::mutate(battle_loc = as.integer(.data$battle_loc),
           year = as.integer(.data$year),
           intensity_level = as.integer(.data$intensity_level)) |>
    dplyr::rename(gwcode = .data$battle_loc) |>
    dplyr::group_by(.data$gwcode, .data$year) |>
    dplyr::summarize(intensity_level = max(.data$intensity_level, na.rm = T)) |>
    dplyr::ungroup()

  # The country_id assignment is not correct (to cShapes 2.0 at least)

  gw <- cshp_gw_modifications()
  ucdp_ged <- ucdp_ged |> sf::st_as_sf(coords = c("longitude", "latitude"), crs = 4326)

  ucdp_ged_l <- split(ucdp_ged, ucdp_ged$year)
  df_l <- split(df, df$year)

  ged_years <- list()
  sf::sf_use_s2(FALSE)
  for(year in names(ucdp_ged_l)){
    gw_year <- df_l[[year]]
    ged_year <- ucdp_ged_l[[year]]

    ged_year <- sf::st_join(ged_year, gw_year |> dplyr::select(correct_gwcode = .data$gwcode))
    ged_years[[year]] <- ged_year
  }
  new_ucdp_ged <- dplyr::bind_rows(ged_years)
  sf::st_geometry(new_ucdp_ged) <- NULL


  battle_deaths <- new_ucdp_ged |>
    dplyr::select(gwcode = .data$correct_gwcode,
                  .data$year,
                  .data$type_of_violence,
                  .data$deaths_a,
                  .data$deaths_b,
                  .data$deaths_civilians,
                  .data$deaths_unknown,
                  .data$best,
                  .data$high,
                  .data$low) |>
    dplyr::filter(.data$type_of_violence == 1) |> #state-based violence
    dplyr::select(-.data$type_of_violence) |>
    dplyr::group_by(.data$gwcode, .data$year) |>
    dplyr::summarize_all(.funs = sum) |>
    dplyr::ungroup() |>
    dplyr::mutate(intensity_level = dplyr::if_else(.data$best < 25, 0,
                                                   dplyr::if_else(.data$best < 1000, 1, 2)))



  brd_panel <- dplyr::bind_rows(intensity_panel,
                                      battle_deaths |>
                                        dplyr::select(.data$gwcode,
                                                      .data$year,
                                                      .data$intensity_level,
                                                      .data$best,
                                                      .data$low,
                                                      .data$high))

  intensity_panel <- dplyr::bind_rows(intensity_panel,
                                      battle_deaths |>
                                        dplyr::select(.data$gwcode,
                                                      .data$year,
                                                      .data$intensity_level,
                                                      .data$best,
                                                      .data$low,
                                                      .data$high))


  df <- dplyr::left_join(df, intensity_panel, by = c("gwcode", "year"))

  df <- df |> dplyr::mutate(
    intensity_level = dplyr::if_else(is.na(.data$intensity_level), 0, .data$intensity_level),
    best = dplyr::if_else(is.na(.data$best) & .data$year >= 1989, 0, .data$best),
    low = dplyr::if_else(is.na(.data$low) & .data$year >= 1989, 0, .data$low),
    high = dplyr::if_else(is.na(.data$high) & .data$year >= 1989, 0, .data$high)
  )

  return(df)
}
