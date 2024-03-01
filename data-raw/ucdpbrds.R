## code to prepare `ucdpbrds` dataset goes here

many_blcks <- battlelocationsfatalityshares |>
  tidyr::separate_longer_delim(battle_loc, delim = ", ") |>
  dplyr::mutate(battle_loc = as.integer(battle_loc)) |>
  dplyr::group_by(conflict_id, old_id, year) |>
  dplyr::mutate(pos_idx = dplyr::row_number()) |>
  dplyr::mutate(brd_share = dplyr::case_when(
    pos_idx == 1 ~ share_a,
    pos_idx == 2 ~ share_b,
    pos_idx == 3 ~ share_c,
    pos_idx == 4 ~ share_d
  )) |>
  dplyr::select(conflict_id, old_id, year, battle_loc, brd_share) |>
  na.omit()

brd <- get_prio_brd() |>
  dplyr::rename(conflict_id = new_id, old_id = id) |>
  dplyr::select(conflict_id, old_id, year, bdeadlow, bdeadhig, bdeadbes) |>
  dplyr::mutate(conflict_id = as.integer(conflict_id),
         old_id = as.integer(old_id),
         year = as.integer(year))

blcs <- ucdp_prio_battle_locations_before_1989()

single_blcks <- blcs |>
  dplyr::filter(!grepl(", ", battle_loc)) |>
  dplyr::select(conflict_id, year, battle_loc) |>
  dplyr::mutate(brd_share = 1,
         conflict_id = as.integer(conflict_id),
         year = as.integer(year),
         battle_loc = as.integer(battle_loc))

brd_single <- dplyr::left_join(single_blcks, brd, by = c("conflict_id", "year"))
brd_many <- dplyr::left_join(many_blcks, brd, by = c("conflict_id", "old_id", "year"))

res <- dplyr::bind_rows(brd_single, brd_many) |>
  dplyr::mutate(
    bdeadlow = dplyr::if_else(bdeadlow == -999, NA_real_, bdeadlow),
    bdeadhig = dplyr::if_else(bdeadhig == -999, NA_real_, bdeadhig),
    bdeadbes = dplyr::if_else(bdeadbes == -999, NA_real_, bdeadbes)) |>
  dplyr::mutate(bdeadbes = dplyr::if_else(is.na(bdeadbes),
                            (bdeadhig + bdeadlow)/2, bdeadbes)) |>
  dplyr::mutate(bdeadlow = as.integer(bdeadlow * brd_share),
         bdeadhig = as.integer(bdeadhig * brd_share),
         bdeadbes = as.integer(bdeadbes * brd_share))|>
  dplyr::group_by(battle_loc, year) |>
  dplyr::summarize(bdeadlow = sum(bdeadlow, na.rm = T),
            bdeadhig = sum(bdeadhig, na.rm = T),
            bdeadbes = sum(bdeadbes, na.rm = T))

gw <- cshp_gw_modifications(western_sahara = TRUE, morocco_protectorate = TRUE, palestine = TRUE, soviet_25dec = TRUE)
ged <- gw_ged(version = "23.1", time_interval = "year", state_base = TRUE, non_state = FALSE, one_sided = FALSE,
              drop_poor_precision = FALSE, test = FALSE)

ged <- ged |>
  dplyr::mutate(year = lubridate::year(mydate)) |>
  dplyr::select(gwcode, country_name, status, owner, capname, caplong, caplat, b_def, fid, year, low, high, best)

df <- gw_panel(gw, time_interval = "year", begin = as.Date("1946-01-01")) |>
  dplyr::filter(year < 1989) |>
  dplyr::left_join(res, by = c("gwcode" = "battle_loc", "year")) |>
  dplyr::mutate(bdeadlow = dplyr::if_else(is.na(bdeadlow), 0, bdeadlow),
         bdeadhig = dplyr::if_else(is.na(bdeadhig), 0, bdeadhig),
         bdeadbes = dplyr::if_else(is.na(bdeadbes), 0, bdeadbes)) |>
  dplyr::select(gwcode, country_name, status, owner, capname, caplong, caplat, b_def, fid, year, low = bdeadlow, high = bdeadhig, best = bdeadbes)

ucdpbrds <- dplyr::bind_rows(df, ged) |>
  dplyr::mutate(source = dplyr::if_else(year < 1989, "PRIO Battle-deaths 3.1", "UCDP GED 23.1"))


usethis::use_data(ucdpbrds, overwrite = TRUE)
