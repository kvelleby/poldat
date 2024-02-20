## code to prepare `battlelocationsandfatalityshares` dataset goes here
battlelocationsfatalityshares <- readr::read_csv2("data-raw/battlelocationsfatalityshares.csv") |>
  dplyr::rename(old_id = id) |>
  dplyr::mutate(conflict_id = as.integer(conflict_id),
                old_id = as.integer(old_id),
                year = as.integer(year),
                intensity_level = as.integer(intensity_level),
                cumulative_intensity = as.integer(cumulative_intensity),
                start_date = as.Date(start_date, format = "%d.%m.%y"),
                ep_end_date = as.Date(ep_end_date, format = "%d.%m.%y"),
                bdeadlow = as.integer(bdeadlow),
                bdeadhig = as.integer(bdeadhig),
                bdeadbes = as.integer(bdeadbes),
                share_a = as.numeric(share_a),
                share_b = as.numeric(share_b),
                share_c = as.numeric(share_c),
                share_d = as.numeric(share_d))


usethis::use_data(battlelocationsfatalityshares, overwrite = TRUE)
