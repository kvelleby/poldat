% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data.R
\docType{data}
\name{ucdpbrds}
\alias{ucdpbrds}
\title{A battle-deaths country-year panel data from 1946-2022}
\format{
A data frame with 13170 rows and 14 columns:
\describe{
  \item{gwcode}{Slightly modified cShapes 2.0 gwcodes}
  \item{country_name}{Country name from cShapes 2.0}
  \item{status}{Status of territory from cShapes 2.0}
  \item{owner}{The state holding sovereignity over territory, from cShapes 2.0}
  \item{capname}{Name of capital, from cShapes 2.0}
  \item{caplong}{Longitude of the capital from cShapes 2.0}
  \item{caplat}{Latitude of the capital from cShapes 2.0}
  \item{b_def}{Dummy if country borders are clearly defined from cShapes 2.0}
  \item{fid}{Feature id, used to match GIS borders from cShapes 2.0}
  \item{year}{The year of the events}
  \item{low}{Low estimate of battle-deaths}
  \item{high}{High estimate of battle-deaths}
  \item{best}{Best estimate of battle-deaths}
  \item{source}{Source of battle-deaths data}
}
}
\source{
[PRIO Battle-deaths 3.1](https://www.prio.org/data/1)

[UCDP GED 23.1](https://ucdp.uu.se)

[cShapes 2.0](https://icr.ethz.ch/data/cshapes/)
}
\usage{
ucdpbrds
}
\description{
PRIO Battle-deaths 3.1 and UCDP GED 23.1 with cShapes 2.0 and manual coding of battle-locations and fatality shares.
The cut-off point is between 1988-89. This data does not reconcile the fact that battle-deaths are counted in very
different manner in PRIO Battle-deaths and UCDP GED. My guess is that low intensity conflicts are better covered by
UCDP GED, but that it under-counts battle-deaths compared to PRIO Battle-deaths. Hopefully, the coverage has increased
over the years. The big difference in battle-deaths from 1988 to 1989 could be explained by the end of the Iran-Irak war,
but is also likely to be due to different measurement approaches.
}
\details{
Main innovations:
* UCDP GED provides the geographic location of the battle, whilst UCDP/PRIO and PRIO Battle-deaths does not.
Here, I've coded that, as well as the shares of fatalities in different countries if multiple countries are involved. The
coding assumes that intrastate conflicts only had fighting within that country, although we know from UCDP GED that events
also happen in other countries.
* I consistently use my modified version of cShapes 2.0 gwcodes, overriding "country_id" in UCDP GED. This means that events in Western Sahara and
Palestine are coded as such. Also, events in colonies have separate gwcodes. E.g., events in Namibia in 1989 are coded in UCDP GED as happening in
South Africa, as Namibia was a colony of South Africa at the time.
}
\keyword{datasets}
