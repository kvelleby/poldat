#' Battle-locations fatality shares data
#'
#' Manually coded yearly fatality shares for conflicts in UCDP/PRIO ACD version 23.1
#' with more than one battle-location as coded in \code{\link[=ucdp_prio_battle_locations_before_1989]{ucdp_prio_battle_locations_before_1989()}}
#' The shares are based on reading Wikipedia and the documentation for the PRIO Battledeaths dataset, and amounts to a qualified impression of where most fighting occurred.
#' This is not an assessment of fatalities broken down in events. E.g., .5/.5 is used when fighting occurred on both sides of a contested border.
#' .1/.9 is used if most fighting happened on one side of the border, but with documented fighting on the other side. The code is made on a year
#' by year basis for each conflict_id, and the shares can change over years.
#'
#' @format A data frame with 134 rows and 22 columns:
#' \describe{
#'   \item{conflict_id}{UCDP/PRIO conflict id}
#'   \item{old_id}{Old conflict id as used in PRIO Battle-deaths data}
#'   \item{year}{Year}
#'   \item{battle_loc}{cShapes 2.0 gwcodes of countries with any battles within a conflict_id}
#'   \item{side_a}{Actor on side A}
#'   \item{side_a_2nd}{States that enter conflict with troops in support of Side A}
#'   \item{side_b}{Actor on side B}
#'   \item{side_b_2nd}{States that enter conflict with troops in support of Side B}
#'   \item{intensity_level}{1: Between 25 and 999 battle-deaths in a given year in the conflict, 2: > 1000 BRDs}
#'   \item{cumulative_intensity}{Whether conflict since onset has exceeded 1000 BRDs}
#'   \item{start_date}{Date of first battle-death in conflict}
#'   \item{ep_end_date}{Date when conflict activity ended}
#'   \item{bdeadlow}{Battle-deaths, low estimate from PRIO Battle-deaths}
#'   \item{bdeadhig}{Battle-deaths, high estimate from PRIO Battle-deaths}
#'   \item{bdeadbes}{Battle-deaths, best estimate from PRIO Battle-deaths}
#'   \item{territory_name}{The name of the territory over which the conflict is fought}
#'   \item{gwnoloc}{Country codes of the incompatibility}
#'   \item{share_a, share_b, share_c, share_d}{The coded share of battle-deaths occuring in 1st, 2nd, 3rd, and 4th gwcode in the battle_loc variable}
#'   \item{note}{My notes for the coding. These are not replicated for each year if the coding relates to the whole conflict_id.}
#' }
#' @source [PRIO Battle-deaths 3.1](https://www.prio.org/data/1)
#' @source [UCDP GED 23.1](https://ucdp.uu.se)
"battlelocationsfatalityshares"

#' A battle-deaths country-year panel data from 1946-2022
#'
#' PRIO Battle-deaths 3.1 and UCDP GED 23.1 with cShapes 2.0 and manual coding of battle-locations and fatality shares.
#' The cut-off point is between 1988-89. This data does not reconcile the fact that battle-deaths are counted in very
#' different manner in PRIO Battle-deaths and UCDP GED. My guess is that low intensity conflicts are better covered by
#' UCDP GED, but that it under-counts battle-deaths compared to PRIO Battle-deaths. Hopefully, the coverage has increased
#' over the years. The big difference in battle-deaths from 1988 to 1989 could be explained by the end of the Iran-Irak war,
#' but is also likely to be due to different measurement approaches.
#'
#' Main innovations:
#' * UCDP GED provides the geographic location of the battle, whilst UCDP/PRIO and PRIO Battle-deaths does not.
#' Here, I've coded that, as well as the shares of fatalities in different countries if multiple countries are involved. The
#' coding assumes that intrastate conflicts only had fighting within that country, although we know from UCDP GED that events
#' also happen in other countries.
#' * I consistently use my modified version of cShapes 2.0 gwcodes, overriding "country_id" in UCDP GED. This means that events in Western Sahara and
#' Palestine are coded as such. Also, events in colonies have separate gwcodes. E.g., events in Namibia in 1989 are coded in UCDP GED as happening in
#' South Africa, as Namibia was a colony of South Africa at the time.
#'
#'
#' @format A data frame with 13170 rows and 14 columns:
#' \describe{
#'   \item{gwcode}{Slightly modified cShapes 2.0 gwcodes}
#'   \item{country_name}{Country name from cShapes 2.0}
#'   \item{status}{Status of territory from cShapes 2.0}
#'   \item{owner}{The state holding sovereignity over territory, from cShapes 2.0}
#'   \item{capname}{Name of capital, from cShapes 2.0}
#'   \item{caplong}{Longitude of the capital from cShapes 2.0}
#'   \item{caplat}{Latitude of the capital from cShapes 2.0}
#'   \item{b_def}{Dummy if country borders are clearly defined from cShapes 2.0}
#'   \item{fid}{Feature id, used to match GIS borders from cShapes 2.0}
#'   \item{year}{The year of the events}
#'   \item{low}{Low estimate of battle-deaths}
#'   \item{high}{High estimate of battle-deaths}
#'   \item{best}{Best estimate of battle-deaths}
#'   \item{source}{Source of battle-deaths data}
#' }
#' @source [PRIO Battle-deaths 3.1](https://www.prio.org/data/1)
#' @source [UCDP GED 23.1](https://ucdp.uu.se)
#' @source [cShapes 2.0](https://icr.ethz.ch/data/cshapes/)
"ucdpbrds"

#' A battle-deaths static-country-year panel data from 1946-2022
#'
#'
#' PRIO Battle-deaths 3.1 and UCDP GED 23.1 with cShapes 2.0 and manual coding of battle-locations and fatality shares.
#' The data base itself of static country borders (as they were 2019-01-01), and estimate "synthetic" versions of these countries
#' before they existed. For the period 1989-2022, when using UCDP GED, it is simple to identify country-id based on the event location. For the period 1946-1988, I base myself off the manual coding of battle-locations and fatality shares.
#' Then, I distribute battle-deaths according to the share of area at the time that
#' intersects with the country-borders as they existed in 2019-01-01. Clearly, this can introduce coding errors, but until
#' we have sub-national data of battle-locations, coding this in other ways is difficult.
#'
#' See `ucdpbrds` for more information.
#'
#' @format A data frame with 14037 rows and 5 columns:
#' \describe{
#'   \item{gwcode}{Slightly modified cShapes 2.0 gwcodes}
#'   \item{year}{The year of the events}
#'   \item{low}{Low estimate of battle-deaths}
#'   \item{high}{High estimate of battle-deaths}
#'   \item{best}{Best estimate of battle-deaths}
#' }
#' @source [PRIO Battle-deaths 3.1](https://www.prio.org/data/1)
#' @source [UCDP GED 23.1](https://ucdp.uu.se)
#' @source [cShapes 2.0](https://icr.ethz.ch/data/cshapes/)
"ucdpbrds_static"

#' Custom country to gwcode matches
#'
#' A set of matches that fits well with `cshp_gw_modifications()` and `countrycode(custom_match = custom_gwcode_matches)`
#' Depending on whether you match from gwcode to something else, or from something to gwcode, you will need to swap
#' names and values of this vector
#'
#' @format A named vector
"custom_gwcode_matches"
