get_parents <- function(node, g){
  return(igraph::as_edgelist(g) |> dplyr::as_tibble(.name_repair = NULL) |> dplyr::filter(V2 == node) |> dplyr::pull(V1))
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

area_weighted_synthetic_brds <- function(df, static_year, ...){
  if(!("year" %in% names(df))){
    stop("Variable 'year' must be present in df.")
  }
  if(!("gwcode" %in% names(df))){
    stop("Variable 'gwcode' must be present in df.")
  }

  gw <- cshp_gw_modifications(...)
  g <- territorial_dependencies(gw)
  dynamic_skeleton <- gw_panel(gw, time_interval = "year") |>
    dplyr::mutate(duuid = paste(gwcode, fid, sep = "-")) |>
    dplyr::select(gwcode, year, duuid)
  static_skeleton <- gw_panel(gw, time_interval = "year", static_date = as.Date("2019-01-01"))
  static_skeleton <- dplyr::left_join(static_skeleton, dynamic_skeleton, by = c("gwcode", "year"))

  df_names <- names(df)
  df <- dplyr::left_join(static_skeleton, df, by = c("gwcode", "year")) |>
    dplyr::select(dplyr::all_of(c(df_names, "fid", "duuid"))) |>
    dplyr::mutate(uuid = paste(gwcode, fid, sep = "-"))

  ew <- edge_weights_as_childrens_share_of_variable(g, "a_i")

  # https://stackoverflow.com/questions/50003449/multiplicative-distance-between-graph-nodes
  gew <- ew |>
    dplyr::mutate(weight = dplyr::if_else(weight != 0, log(weight), weight)) |>
    igraph::graph_from_data_frame() # Log weigths to get multiplicative distances
  igraph::E(gew)$weight <- -1*igraph::E(gew)$weight
  weight_matrix <- exp(igraph::shortest.paths(gew, mode = "out")* -1)

  uuid_strings <- df |>
    dplyr::filter(year == static_year) |>
    dplyr::pull(uuid) |> unique()

  res <- dplyr::tibble()
  pb = txtProgressBar(min = 0, max = length(uuid_strings), initial = 0)
  for(j in 1:length(uuid_strings)){
    setTxtProgressBar(pb,j)
    uuid_str <- uuid_strings[j]
    ancestor_uuids <- igraph::neighborhood(g, uuid_str, order = 100, mindist = 1, mode = "in")[[1]] |> names()
    if(length(ancestor_uuids) == 0){
      country_res <- df |>
        dplyr::filter(uuid == uuid_str) |>
        dplyr::rename(node = uuid) |>
        mutate(gwcode = stringr::str_remove(node, "-[0-9]*") |> as.integer())
    } else{
      weights <- weight_matrix[rownames(weight_matrix) %in% c(uuid_str, ancestor_uuids), colnames(weight_matrix) == uuid_str]
      country_res <- dplyr::tibble()
      for(i in 1:length(weights)){
        country <- names(weights)[i]
        sdf <- df |> dplyr::filter(duuid == country) |>
          dplyr::mutate(dplyr::across(-dplyr::any_of(c("gwcode", "year", "fid", "duuid", "uuid")), function(x) x*weights[i])) |>
          dplyr::mutate(node = uuid_str)

        country_res <- dplyr::bind_rows(country_res, sdf)
      }
      country_res <- country_res |>
        dplyr::group_by(node, year) |>
        dplyr::summarize(dplyr::across(-dplyr::any_of(c("gwcode", "year", "fid", "duuid", "uuid")), .fns = sum), .groups = "drop_last")

    }
    res <- dplyr::bind_rows(res, country_res)
    close(pb)
  }
  return(res)
}
