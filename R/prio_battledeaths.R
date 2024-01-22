#' Downloads PRIO Battle Deaths Dataset 1946-2008 version 3
#'
#' Please cite:
#' Bethany Lacina and Nils Petter Gleditsch, 2005. "Monitoring Trends in Global Combat: A New Dataset of Battle Deaths." European Journal of
#' Population: 21(2–3): 145–166. The data are available at http://www.prio.no/CSCW/Datasets/Armed-Conflict/Battle-Deaths/
#'
#' Updated UCDP/PRIO conflict id's are merged in for convenience.
#'
#' @return A data frame with the Battle Deaths data.
#'
#' @examples
#' brds <- get_prio_brd()
get_prio_brd <- function(){
  brd_url <- "https://cdn.cloud.prio.org/files/d21ef702-a546-45a8-b3c9-5b520dcc1239/PRIO%20Battle%20Deaths%20Dataset%2031.xls?inline=true"

  id_translation_table_url <- "https://ucdp.uu.se/downloads/actor/translate_conf.csv"


  tmp <- tempfile()
  id_translation_table_url |>
    httr2::request() |>
    httr2::req_perform(path = tmp)
  ids <- readr::read_csv(tmp) |>
    janitor::clean_names() |>
    dplyr::mutate(old_id = as.numeric(.data$old_id))
  unlink(tmp)

  tmp <- tempfile()
  brd_url |>
    httr2::request() |>
    httr2::req_perform(path = tmp)
  brd <- readxl::read_excel(tmp) |>
    janitor::clean_names()
  unlink(tmp)


  brd <-brd |> dplyr::left_join(ids, by = c("id" = "old_id"))

  return(brd)
}

