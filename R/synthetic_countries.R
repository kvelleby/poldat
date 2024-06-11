get_parents <- function(node, g){
  return(igraph::as_edgelist(g) |> dplyr::as_tibble() |> dplyr::filter(V2 == node) |> dplyr::pull(V1))
}

lookup_edge_values <- function(parent, variable, g){
  res <- g |> igraph::as_data_frame(what = "edges") |> dplyr::filter(from == parent)
  if(nrow(res) > 0){
    return(res |> dplyr::select(from, to, !!variable))
  }
}

edge_weight_as_childrens_share_of_variable <- function(node, variable, g){
  parents <- get_parents(node, g)
  if(length(parents) == 0){return()}

  edge_values <- lapply(parents, lookup_edge_values, variable = variable, g = g)
  edge_values <- edge_values |> dplyr::bind_rows()
  edge_values$node <- node
  if(is.null(edge_values)){return()}

  edge_values |>
    dplyr::group_by(from) |>
    dplyr::mutate(total = sum(!!rlang::data_sym(variable))) |>
    dplyr::filter(.data$to == .data$node) |>
    dplyr::mutate(weight = !!rlang::data_sym(variable) / .data$total) |>
    dplyr::select(.data$from, .data$to, .data$weight)
}

edge_weights_as_childrens_share_of_variable <- function(g, variable){
  nodes <- g |> igraph::as_data_frame(what = "vertices") |> row.names()
  return(lapply(nodes, edge_weight_as_childrens_share_of_variable, variable = variable, g = g) |> dplyr::bind_rows())
}

#' Calculate area weighted synthetic static panel data
#'
#' This function creates a static-world panel data.frame from dynamic gwcode-year panel-data by weighing
#' the data from the ancestors of countries that existed at the end of `static_year` using the shares of
#' ancestor's territories that can be traced to the current countries.
#'
#' @param df A dynamic panel-data with gwcode and year plus any additional variables.
#' @param static_year An integer of the year you want to create a static world template based on.
#' @param ... Additional parameters, please not use yet as they might not work properly.
#' @return A static panel-data data.frame
#' @export
#'
#' @examples
#' vdem_dynamic <- get_vdem(v2x_libdem, v2x_regime)
#' vdem_static <- area_weighted_synthetic_data(vdem_dynamic, 2019)
#'
area_weighted_synthetic_data <- function(df, static_year, ...){
  # Returns 0 when should return NA.
  df |> dplyr::select(dplyr::all_of(c("gwcode", "year"))) # Simple way to test expectations of data.frame
  min_year <- min(df$year)

  start_date <- as.Date(paste0(min(df$year),"-01-01"))
  end_date <- as.Date(paste0(max(df$year),"-12-31"))

  gw <- cshp_gw_modifications(...)
  g <- territorial_dependencies(gw)
  dynamic_skeleton <- gw_panel(gw, time_interval = "year") |>
    dplyr::mutate(uuid = paste(.data$gwcode, .data$fid, sep = "-")) |>
    dplyr::select(.data$gwcode, .data$year, .data$uuid)
  df <- dplyr::left_join(dynamic_skeleton, df, by = c("gwcode", "year"))

  ew <- edge_weights_as_childrens_share_of_variable(g, "a_i")

  # https://stackoverflow.com/questions/50003449/multiplicative-distance-between-graph-nodes
  gew <- ew |>
    dplyr::mutate(weight = dplyr::if_else(.data$weight != 0, log(.data$weight), .data$weight)) |>
    igraph::graph_from_data_frame() # Log weigths to get multiplicative distances
  igraph::E(gew)$weight <- -1*igraph::E(gew)$weight
  weight_matrix <- exp(igraph::shortest.paths(gew, mode = "out")* -1)
  weight_matrix <- weight_matrix / apply(weight_matrix, 1, sum) # row standardize weights

  uuid_strings <- df |>
    dplyr::filter(.data$year == static_year) |>
    dplyr::pull(.data$uuid) |> unique()

  res <- dplyr::tibble()
  pb = txtProgressBar(min = 0, max = length(uuid_strings), initial = 0)
  for(j in 1:length(uuid_strings)){
    setTxtProgressBar(pb,j)
    uuid_str <- uuid_strings[j]
    ancestor_uuids <- igraph::neighborhood(g, uuid_str, order = 100, mindist = 1, mode = "in")[[1]] |> names()
    if(length(ancestor_uuids) == 0){
      country_res <- df |>
        dplyr::filter(.data$uuid == uuid_str) |>
        dplyr::rename(node = .data$uuid) |>
        dplyr::mutate(gwcode = stringr::str_remove(.data$node, "-[0-9]*") |> as.integer())
    } else{
      weights <- weight_matrix[rownames(weight_matrix) %in% c(uuid_str, ancestor_uuids), colnames(weight_matrix) == uuid_str]
      country_res <- dplyr::tibble()
      for(i in 1:length(weights)){
        country <- names(weights)[i]
        sdf <- df |> dplyr::filter(.data$uuid == country) |>
          dplyr::mutate(dplyr::across(-dplyr::any_of(c("gwcode", "year", "fid", "uuid")), function(x) x*weights[i])) |>
          dplyr::mutate(node = uuid_str)

        country_res <- dplyr::bind_rows(country_res, sdf)
      }
      country_res <- country_res |>
        dplyr::group_by(.data$node, .data$year) |>
        dplyr::summarize(dplyr::across(-dplyr::any_of(c("gwcode", "year", "fid", "uuid")), .fns = ~ hablar::sum_(.x)), .groups = "drop_last") # hablar::sum_ returns NA instead of 0 if all are NA

    }
    res <- dplyr::bind_rows(res, country_res) |> dplyr::mutate(gwcode = stringr::str_remove(.data$node, "-[0-9]*") |> as.integer())
  }
  res <- res |>
    dplyr::select(-dplyr::any_of(c("node", "fid", "uuid"))) |>
    dplyr::group_by(.data$gwcode, .data$year) |>
    dplyr::summarize(dplyr::across(dplyr::everything(), .fns = ~ hablar::sum_(.x)), .groups = "drop_last") # hablar::mean_ returns NA instead of NaN if all are NA



  static_skeleton <- gw_panel(gw, time_interval = "year", begin = start_date, stop = end_date, static_date = as.Date(paste0(static_year, "-01-01")))
  res <- dplyr::left_join(static_skeleton |> dplyr::select(.data$gwcode, .data$year), res, by = c("gwcode", "year")) |>
    dplyr::filter(.data$year >= min_year)
  close(pb)

  return(res)
}
