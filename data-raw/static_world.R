vdem <- get_vdem(v2x_libdem, v2x_regime, v2x_accountability, v2x_corr, v2xeg_eqdr, v2x_egal, v2x_polyarchy, v2pepwrgen, e_wbgi_gee, e_wbgi_vae, v2regdur) |> area_weighted_synthetic_data(2019)
vdem2 <- get_vdem(v2regendtype, .fun = min) |> area_weighted_synthetic_data(2019)
ucdp <- ucdpbrds |> dplyr::select(gwcode, year, best, low, high) |> area_weighted_synthetic_data(2019)
pwt <- get_ggdc(dataset = "pwt", version = "10.01") |>
  dplyr::select(gwcode, year, rgdpna, rgdpe, rgdpo, pop, emp, cgdpe, cgdpo) |>
  dplyr::rename(pwt_pop = pop)
maddison <- get_ggdc(dataset = "maddison", version = "2020") |>
  dplyr::mutate(maddison_gdp = gdppc * pop) |>
  dplyr::rename(maddison_pop = pop) |>
  dplyr::select(gwcode, year, maddison_gdp, maddison_pop)
wcde <- wcde_gwcode |>
  dplyr::rename(wcde_pop = tot_pop)
fs <- get_fao_food_security() |> dplyr::select(-"country_name")



indicators_mean <- c(
                 "wdi_undernourishment" = "SN.ITK.DEFC.ZS",
                 "wdi_imr" = "SP.DYN.IMRT.IN",
                 "wdi_nmr" = "SH.DYN.NMRT",
                 "wdi_gini" = "SI.POV.GINI")
indicators_sum <- c("wdi_pop" = "SP.POP.TOTL",
                     "wdi_gdp_pp_con_us" = "NY.GDP.MKTP.PP.KD",
                     "wdi_gdp_pp_cur_us" = "NY.GDP.MKTP.PP.CD")
wdi <- download_wdi(indicators_sum) |>
  dplyr::select(-version)
wdi2 <- download_wdi(indicators_mean, merge_fun = hablar::mean_) |>
  dplyr::select(-version)


df <- dplyr::full_join(ucdp, vdem)
df <- dplyr::left_join(df, vdem2)
df <- dplyr::left_join(df, maddison, by = c("gwcode", "year"))
df <- dplyr::left_join(df, pwt, by = c("gwcode", "year"))
df <- dplyr::left_join(df, wdi, by = c("gwcode", "year"))
df <- dplyr::left_join(df, wdi2, by = c("gwcode", "year"))
df <- dplyr::left_join(df, wcde, by = c("gwcode", "year"))
df <- dplyr::left_join(df, fs, by = c("gwcode", "year"))


# Linear interpolation between 5-year intervals
df <- df |>
  dplyr::group_by(gwcode) |>
  dplyr::arrange(year) |>
  dplyr::mutate(dplyr::across(dplyr::all_of(c("priprop", "secprop", "psecprop", "tdr", "ydr", "odr", "youth", "working", "elderly", "wcde_pop")), ~ zoo::na.approx(.x, na.rm = FALSE)))

# Ensure similar numeric representation
df <- df |> dplyr::mutate(
  wdi_pop = wdi_pop / 10^6,
  wcde_pop = wcde_pop / 10^3,
  maddison_pop = maddison_pop / 10^3,
  wdi_gdp_pp_con_us = wdi_gdp_pp_con_us / 10^6,
  wdi_gdp_pp_cur_us = wdi_gdp_pp_cur_us / 10^6,
  maddison_gdp = maddison_gdp / 10^3
)

growth <- function(x) (x - dplyr::lag(x)) / dplyr::lag(x)
ln_growth <- function(x) (log(x) - log(dplyr::lag(x)))

df <- tsibble::as_tsibble(df, key = gwcode, index = year)

df <- df |>
  dplyr::mutate(pwt_grwt_na = ln_growth(rgdpna),
         wdi_grwt_con = ln_growth(wdi_gdp_pp_con_us),
         maddison_grwt = ln_growth(maddison_gdp))

df <- df |>
  dplyr::mutate(pwt_pop_grwt = ln_growth(pwt_pop),
         wdi_pop_grwt = ln_growth(wdi_pop),
         wcde_pop_grwt = ln_growth(wcde_pop),
         maddison_pop_grwt = ln_growth(maddison_pop))


df <- df |> dplyr::mutate(
  grwt = dplyr::if_else(!is.na(pwt_grwt_na) & !is.infinite(pwt_grwt_na), pwt_grwt_na,
                         dplyr::if_else(!is.na(wdi_grwt_con) & !is.infinite(wdi_grwt_con), wdi_grwt_con, maddison_grwt)),
  pop_grwt = dplyr::if_else(!is.na(wdi_pop_grwt) & !is.infinite(wdi_pop_grwt), wdi_pop_grwt,
                             dplyr::if_else(!is.na(wcde_pop_grwt) & !is.infinite(wcde_pop_grwt), wcde_pop_grwt,
                                            dplyr::if_else(!is.na(pwt_pop_grwt) & !is.infinite(pwt_pop_grwt), pwt_pop_grwt, maddison_pop_grwt)))
)

grow <- function(x, growth) exp(log(dplyr::lag(x)) + growth)
grow_back <- function(x, growth) exp(log(dplyr::lead(x)) - dplyr::lead(growth))

df$rgdp <- NULL
df$population <- NULL

df <- df |>
  dplyr::mutate(rgdp = dplyr::if_else(year != 2017, NA_real_,
                                      dplyr::if_else(!is.na(cgdpo), cgdpo,
                                                     dplyr::if_else(!is.na(wdi_gdp_pp_con_us), wdi_gdp_pp_con_us, maddison_gdp))),
         population = dplyr::if_else(year != 2017, NA_real_,
                                     dplyr::if_else(!is.na(wdi_pop), wdi_pop,
                                                    dplyr::if_else(!is.na(wcde_pop), wcde_pop, pwt_pop))))

for(i in 1:15){
  df <- df |> dplyr::mutate(rgdp = dplyr::if_else(is.na(rgdp), grow(rgdp, grwt), rgdp),
                     population = dplyr::if_else(is.na(population), grow(population, pop_grwt), population))
}
for(i in 1:100){
  df <- df |> dplyr::mutate(rgdp = dplyr::if_else(is.na(rgdp), grow_back(rgdp, grwt), rgdp),
                     population = dplyr::if_else(is.na(population), grow_back(population, pop_grwt), population))
}

df <- df |> dplyr::mutate(rgdp = dplyr::if_else(rgdp == 0, NA_real_, rgdp))
df <- df |> dplyr::mutate(population = dplyr::if_else(population == 0, NA_real_, population))
df <- df |> dplyr::mutate(pwt_pop = dplyr::if_else(pwt_pop == 0, NA_real_, pwt_pop))
df <- df |> dplyr::mutate(wdi_gdp_pp_con_us  = dplyr::if_else(wdi_gdp_pp_con_us  == 0, NA_real_, wdi_gdp_pp_con_us ))
df <- df |> dplyr::mutate(maddison_gdp  = dplyr::if_else(maddison_gdp  == 0, NA_real_, maddison_gdp ))
df <- df |> dplyr::mutate(maddison_pop  = dplyr::if_else(maddison_pop  == 0, NA_real_, maddison_pop ))

df <- df |> dplyr::mutate(gdppc = rgdp / population) |>
  dplyr::mutate(gdppc_grwt = ln_growth(gdppc)) |>
  dplyr::rename(gdp_grwt = grwt)

# Order variables more sensibly
df <- df |> dplyr::select(
  gwcode, year,
  rgdp, gdp_grwt, gdppc, gdppc_grwt, population, pop_grwt,
  best, low, high,
  v2x_polyarchy, v2x_libdem, v2x_regime, v2x_accountability, v2x_corr, v2regdur, v2xeg_eqdr, v2x_egal, v2pepwrgen, v2regendtype, e_wbgi_gee, e_wbgi_vae,
  priprop, secprop, psecprop, tdr, ydr, odr, youth, working, elderly,
  wdi_undernourishment, wdi_imr, wdi_nmr, wdi_gini,
  energy_supply, min_energy_req, calorie_var, food_variance, safe_water_pct, basic_water_pct, basic_sanit_pct, wasting_pct, wasting_num, stunting_pct,
  stunting_num, overweight_pct, overweight_num, obesity_pct, obesity_num, anemia_pct, anemia_num, breastfeed_pct, avg_energy_req, retail_loss, rail_density,
  safe_sanit_pct, low_birth_pct, low_birth_num, "fao_gdp_per_capita" = gdp_per_capita,
  wcde_pop, pwt_pop, wdi_pop, maddison_pop,
  wcde_pop_grwt, pwt_pop_grwt, wdi_pop_grwt, maddison_pop_grwt,
  rgdpna, wdi_gdp_pp_con_us, maddison_gdp,
  pwt_grwt_na, wdi_grwt_con, maddison_grwt,
  rgdpe, rgdpo, emp, cgdpe, cgdpo, wdi_gdp_pp_cur_us
)

static_world <- df
usethis::use_data(static_world, overwrite = TRUE)
