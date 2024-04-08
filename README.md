# Installation

NOTE: This package is under development. Expect this to break.

```R
devtools::install_github("vdeminstitute/vdemdata")
devtools::install_github("kvelleby/poldat")
```

Curl is needed to install RCurl required by wcde. If you do not have the curl development library, see https://stackoverflow.com/a/71048160 for how to install.

For Windows users (also in the stackoverflow):
1. Install [RTools](https://cran.r-project.org/bin/windows/Rtools)
2. Launch RTools Bash from the Windows Menu
3. pacman -Syuv
4. pacman -S mingw-w64-x86_64-curl

igraph is another package that might be difficult for some to install. If installation fails, try:
1. Update R and RTools to at least 4.3 (or any supported R versions for the CRAN binary of igraph).
2. You might need glpk and libxml2 (see https://r.igraph.org/).
  2.1. E.g., (for Windows, in RTools Bash): pacman -S mingw-w64-x86_64-glpk mingw-w64-x86_64-libxml2
3. Try devtools::install_github("kvelleby/poldat")
4. Try devtools::install_github("igraph/rigraph") to install igraph from the github repo

If any other dependencies fail to install, please see their respective installation pages.

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

"Static world" dataset with units using territorial boundaries as in 2019, support for data from UCDP, V-Dem, WDI, WCDE, Penn World Table, Maddison Project, etc. See the data_raw/static_world.R file for how it is built.
df <- static_world
```


