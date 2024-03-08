vdem_notes <- readr::read_csv2("data-raw/vdem_notes.csv")

vdat <- vdemdata::vdem |> dplyr::select(country_name, country_text_id, country_id) |> dplyr::distinct()

# Double check that names and id's are coded correctly.
dplyr::anti_join(vdem_notes, vdat, by = c("vdem_id" = "country_id"))
dplyr::anti_join(vdem_notes |> dplyr::filter(!is.na(include_id)), vdat, by = c("include_id" = "country_id"))
dplyr::anti_join(vdem_notes |> dplyr::filter(!is.na(not_include_id)), vdat, by = c("not_include_id" = "country_id"))
dplyr::anti_join(vdem_notes, vdat, by = c("vdem_name" = "country_name"))
dplyr::anti_join(vdem_notes |> dplyr::filter(!is.na(include_id)), vdat, by = c("includes" = "country_name")) # These are ok, not perfect fit to country_id, but good enough
dplyr::anti_join(vdem_notes |> dplyr::filter(!is.na(not_include_id)), vdat, by = c("not_include" = "country_name"))

usethis::use_data(vdem_notes, overwrite = TRUE)
