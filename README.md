# Installation

```R
devtools::install_github("kvelleby/poldat")
```

# Useage

Pull the latest version of UCDP GED from their API. `get_ucdp_ged()' caches the results on disk so next time you run with  the same settings, you will get the cached version.
```R
library(poldat)
stable_version <- latest_ucdp_version()
candidate_version <- latest_ucdp_version(type="candidate")

ucdp <- get_ucdp_ged(stable_version)
ucdp_candidate <- get_ucdp_ged(candidate_version)

memoise::has_cache(get_ucdp_ged)(stable_version)
memoise::drop_cache(get_ucdp_ged)(stable_version)
```

```R
gw <- cshp_gw_modifications(soviet_25dec = TRUE)
g <- territorial_dependencies(gw)

memoise::has_cache(territorial_dependencies)(gw)
memoise::drop_cache(territorial_dependencies)(gw)
```
