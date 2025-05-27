excluded_share <- function(){
  library(priogrid)
  excluded <- read_epr() |>
    dplyr::filter(status %in% c("DISCRIMINATED", "POWERLESS", "SELF-EXCLUSION"))

  agg <- excluded |>
    dplyr::group_by(gwid, from, to) |>
    dplyr::summarize(size = sum(size, na.rm = T))

  expanded_data <- list()
  for(i in 1:nrow(agg)){
    r <- agg[i,]
    expanded_data[[i]] <- data.frame(gwcode = r$gwid, year = r$from:r$to, size = r$size)
  }

  df <- dplyr::bind_rows(expanded_data) |>
    dplyr::group_by(gwcode, year) |>
    dplyr::summarize(size = sum(size, na.rm = T)) |>
    tsibble::tsibble(key = "gwcode", index = "year") |>
    tsibble::fill_gaps(.full = TRUE, .start = 1946, .end = 2023)
  df
}
