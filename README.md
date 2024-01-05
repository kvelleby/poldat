# Installation

```R
devtools::install_github("kvelleby/poldat")
```

# Useage

```R
library(poldat)
stable_version <- latest_ucdp_version()
candidate_version <- latest_ucdp_version(type="candidate")

ucdp <- get_ucdp_ged(stable_version)
ucdp_candidate <- get_ucdp_ged(candidate_version)
```
