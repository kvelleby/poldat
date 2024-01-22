# Installation

NOTE: This package is under development. Expect this to break.

```R
devtools::install_github("kvelleby/poldat")
```

# Useage

Pull the latest version of UCDP GED from their API. `get_ucdp_ged()' caches the results on disk so next time you run with  the same settings, you will get the cached version.
```R
library(poldat)
stable_version <- latest_ucdp_version()
candidate_version <- latest_ucdp_version(type="candidate")

ged <- get_ucdp_ged(stable_version)
ged_candidate <- get_ucdp_ged(candidate_version)

memoise::has_cache(get_ucdp_ged)(stable_version)
memoise::drop_cache(get_ucdp_ged)(stable_version)
```

Create a network graph of all territorial dependencies (states that share territory across time). Neighbors own the same territory with 1-day difference.
```R
gw <- cshp_gw_modifications(soviet_25dec = TRUE)
g <- territorial_dependencies(gw)

memoise::has_cache(territorial_dependencies)(gw)
memoise::drop_cache(territorial_dependencies)(gw)

usa <- find_territorial_dependencies(2, gw)
usa |> plot()
```

Create a panel dataset from cShapes. 
```R
gw <- cshp_gw_modifications()
df <- gw_panel(gw, time_interval = "week", begin = as.Date("2024-01-01"), stop = Sys.Date())
```

Create a country-year panel dataset with battle-locations and conflict intensity from 1946-present. This includes a novel recoding of battle-locations (at the country-level) for the whole UCDP/PRIO ACD dataset, and using the modified cShapes 2.0 data as a consistent dataset for determining location. E.g., the country id's of events from UCDP GED are coded using cShapes, instead of the country_id variable supplied by UCDP.
```R
recoded_battle_locations <- ucdp_prio_battle_locations_before_1989()
df <- ucdp_long_cy_panel()

# Here  is an issue where overlapping countries are part of the gw_panel during the same year.
df |> dplyr::filter(gwcode %in% c(666, 699, 655, 651, 6511, 6631), year == 1967)
df |> dplyr::filter(year == 1948) |> dplyr::select(intensity_level) |> plot()
df |> dplyr::filter(year == 2022) |> dplyr::select(intensity_level) |> plot()
```


