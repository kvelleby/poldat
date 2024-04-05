# Windows users might need to install Curl to install RCurl (used by wcde): see https://stackoverflow.com/a/71048160
# 1. Launch RTools Bash from the Windows Menu
# 2. pacman -Syuv
# 3. pacman -S mingw-w64-x86_64-curl

#### Download SSP2

wl <- wcde::wic_locations
wl <- wl |> dplyr::filter(dim == "country")
wl$country_name <- countrycode::countrycode(wl$isono, origin = "iso3n", destination = "country.name")
# 736 (Old Sudan Version), 830 (Channel Islands) and 530 (Netherlands Antilles) are not in WCDE.
wl$gwcode <- countrycode::countrycode(wl$country_name, origin = "country.name", destination = "gwn",
                                      custom_match = custom_gwcode_matches)
# Guam, Micronesia (Federated States of), Samoa, U.S. Virgin Islands are not in gwcode (so 7 obs are dropped)
wl <- wl |> dplyr::select(name, isono, country_name, gwcode) |> na.omit()

ssp2 <- dplyr::tibble() # Need for-loop here due to issue https://github.com/guyabel/wcde/issues/5
for(countries in wl$isono |> split(1:10)){
  initial_connections <- showConnections(all = TRUE)
  tmp <- wcde::get_wcde(indicator = "pop", scenario = 2, country_code = countries, pop_age = "all", pop_edu = "six")
  new_connections <- showConnections(all = TRUE)
  remove_these <- nrow(initial_connections)+1:(nrow(new_connections) - nrow(initial_connections))-1
  connections <- remove_these |> purrr::map(getConnection)
  connections |> purrr::walk(close)
  ssp2 <- dplyr::bind_rows(ssp2, tmp)
}

ssp2$country_name <- countrycode::countrycode(ssp2$country_code, origin = "iso3n", destination = "country.name")
ssp2$gwcode <- countrycode::countrycode(ssp2$country_name, origin = "country.name", destination = "gwn",
                                        custom_match = custom_gwcode_matches)

ssp2$broad_age <- broad_age(ssp2$age)
ssp2$education <- ordered(ssp2$education)

#### Functions

broad_age <- function(age){
  forcats::fct_collapse(age,
                        "youth" = c("0--4", "5--9", "10--14"),
                        "working" = c("15--19", "20--24", "25--29", "30--34", "35--39", "40--44", "45--49", "50--54", "55--59", "60--64"),
                        "elderly" = c("65--69", "70--74", "75--79", "80--84", "85--89", "90--94", "95--99", "100+")
  )
}

total_population <- function(df){
  df |> dplyr::group_by(gwcode, year) |>
    dplyr::summarize(tot_pop = sum(pop))
}

dependency_ratios <- function(df){
  df |> dplyr::group_by(gwcode, year, broad_age) |>
    dplyr::summarize(tot_pop = sum(pop)) |>
    tidyr::pivot_wider(names_from = "broad_age", values_from = "tot_pop") |>
    dplyr::mutate(tdr = (youth + elderly) / working,
                  ydr = youth / working,
                  odr = elderly / working) |>
    dplyr::select(gwcode, year, tdr, ydr, odr, youth, working, elderly)
}

proportion_secondary_educational_attainment_in_workpop <- function(df){
  df |> dplyr::group_by(gwcode, year, broad_age) |>
    dplyr::mutate(tot_pop = sum(pop)) |>
    dplyr::filter(education >= "Upper Secondary") |>
    dplyr::group_by(gwcode, year, broad_age, tot_pop) |>
    dplyr::summarize(sec_pop = sum(pop)) |>
    dplyr::mutate(secprop = sec_pop / tot_pop) |>
    dplyr::filter(broad_age == "working") |>
    dplyr::ungroup() |>
    dplyr::select(gwcode, year, secprop)
}

proportion_primary_educational_attainment_in_workpop <- function(df){
  df |> dplyr::group_by(gwcode, year, broad_age) |>
    dplyr::mutate(tot_pop = sum(pop)) |>
    dplyr::filter(education >= "Primary") |>
    dplyr::group_by(gwcode, year, broad_age, tot_pop) |>
    dplyr::summarize(pri_pop = sum(pop)) |>
    dplyr::mutate(priprop = pri_pop / tot_pop) |>
    dplyr::filter(broad_age == "working") |>
    dplyr::ungroup() |>
    dplyr::select(gwcode, year, priprop)
}

proportion_post_secondary_educational_attainment_in_workpop <- function(df){
  df |> dplyr::group_by(gwcode, year, broad_age) |>
    dplyr::mutate(tot_pop = sum(pop)) |>
    dplyr::filter(education >= "Post Secondary") |>
    dplyr::group_by(gwcode, year, broad_age, tot_pop) |>
    dplyr::summarize(psec_pop = sum(pop)) |>
    dplyr::mutate(psecprop = psec_pop / tot_pop) |>
    dplyr::filter(broad_age == "working") |>
    dplyr::ungroup() |>
    dplyr::select(gwcode, year, psecprop)
}

#### Historical data

edu <- wcde::past_epop

edu$country_name <- countrycode::countrycode(edu$country_code, origin = "iso3n", destination = "country.name")
edu$gwcode <- countrycode::countrycode(edu$country_name, origin = "country.name", destination = "gwn",
                                       custom_match = custom_gwcode_matches)


edu$broad_age <- broad_age(edu$age)
edu$education <- ordered(edu$education)
edu <- edu |> dplyr::rename(pop = epop)


#### Put things together

df <- proportion_secondary_educational_attainment_in_workpop(edu)
df <- dplyr::left_join(df, proportion_primary_educational_attainment_in_workpop(edu), by = c("gwcode", "year"))
df <- dplyr::left_join(df, proportion_post_secondary_educational_attainment_in_workpop(edu), by = c("gwcode", "year"))
df <- dplyr::left_join(df, dependency_ratios(edu), by = c("gwcode", "year"))
df <- dplyr::left_join(df, total_population(edu), by = c("gwcode", "year"))

fut <- proportion_secondary_educational_attainment_in_workpop(ssp2)
fut <- dplyr::left_join(fut, proportion_primary_educational_attainment_in_workpop(ssp2), by = c("gwcode", "year"))
fut <- dplyr::left_join(fut, proportion_post_secondary_educational_attainment_in_workpop(ssp2), by = c("gwcode", "year"))
fut <- dplyr::left_join(fut, dependency_ratios(ssp2), by = c("gwcode", "year"))
fut <- dplyr::left_join(fut, total_population(ssp2), by = c("gwcode", "year"))

# Extrapolate the years after 2020 using SSP2 (this does not look right?)
#df <- dplyr::bind_rows(df, fut |> dplyr::filter(year == 2025))

library(ggplot2)
ggplot(df, aes(x = year, y = priprop, group = gwcode)) +
  geom_line(color = "red") + geom_line(aes(x = year, y = secprop, group = gwcode), color = "blue") +
  geom_line(aes(x = year, y = psecprop, group = gwcode), color = "green") +
  ggplot2::geom_vline(xintercept = 2023)

ggplot(df, aes(x = year, y = odr, group = gwcode)) +
  geom_line() +
  ggplot2::geom_vline(xintercept = 2023)

ggplot(df, aes(x = year, y = ydr, group = gwcode)) +
  geom_line() +
  ggplot2::geom_vline(xintercept = 2023)

ggplot(df, aes(x = year, y = tdr, group = gwcode)) +
  geom_line() +
  ggplot2::geom_vline(xintercept = 2023)

wcde_gwcode <- df
usethis::use_data(wcde_gwcode, overwrite = TRUE)

