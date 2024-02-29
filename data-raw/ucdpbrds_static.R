library(igraph)
library(dplyr)
library(lubridate)
library(rlang)

get_parents <- function(node, g){
  return(as_edgelist(g) |> as_tibble(.name_repair = NULL) |> dplyr::filter(V2 == node) |> dplyr::pull(V1))
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
  edge_values <- edge_values |> bind_rows()
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

area_weighted_synthetic_brds <- function(static_year){
  df <- ucdpbrds |> mutate(uuid = paste(gwcode, fid, sep = "-"))

  uuid_strings <- df |> dplyr::filter(year == static_year) |>
    pull(uuid) |> unique()

  res <- tibble()
  pb = txtProgressBar(min = 0, max = length(uuid_strings), initial = 0)
  for(j in 1:length(uuid_strings)){
    setTxtProgressBar(pb,j)
    uuid_str <- uuid_strings[j]
    ancestor_uuids <- igraph::neighborhood(g, uuid_str, order = 100, mindist = 1, mode = "in")[[1]] |> names()
    if(length(ancestor_uuids) == 0){
      country_res <- df |>
        dplyr::filter(uuid == uuid_str) |>
        rename(node = uuid) |>
        select(node, year, low, high, best) |>
        mutate(low = as.integer(low),
               high = as.integer(high),
               best = as.integer(best)) |>
        mutate(gwcode = stringr::str_remove(node, "-[0-9]*") |> as.integer())
    } else{
      weights <- weight_matrix[rownames(weight_matrix) %in% c(uuid_str, ancestor_uuids), colnames(weight_matrix) == uuid_str]
      country_res <- tibble()
      for(i in 1:length(weights)){
        country <- names(weights)[i]
        sdf <- df |> dplyr::filter(uuid == country)
        sdf <- sdf |> mutate(high = high * weights[i],
                             low = low * weights[i],
                             best = best * weights[i],
                             node = uuid_str)
        country_res <- bind_rows(country_res, sdf) |>
          group_by(node, year) |>
          summarize(low = sum(low) |> as.integer(),
                    high = sum(high) |> as.integer(),
                    best = sum(best) |> as.integer(), .groups = "drop_last") |>
          mutate(gwcode = stringr::str_remove(node, "-[0-9]*") |> as.integer())
      }
    }
    res <- dplyr::bind_rows(res, country_res)
    close(pb)
  }
  return(res)
}


gw <- cshp_gw_modifications()
g <- territorial_dependencies(gw)
ew <- edge_weights_as_childrens_share_of_variable(g, "a_i")

# https://stackoverflow.com/questions/50003449/multiplicative-distance-between-graph-nodes
gew <- ew |> mutate(weight = if_else(weight != 0, log(weight), weight)) |> graph_from_data_frame() # Log weigths to get multiplicative distances
E(gew)$weight <- -1*E(gew)$weight
weight_matrix <- exp(shortest.paths(gew, mode = "out")* -1)


ged <- gw_ged(western_sahara = TRUE, morocco_protectorate = TRUE, palestine = TRUE, soviet_25dec = TRUE,
              version = "23.1", time_interval = "year", state_base = TRUE, non_state = FALSE, one_sided = FALSE,
              drop_poor_precision = FALSE, test = FALSE, static_date = as.Date("2019-01-01")) |>
  dplyr::mutate(year = lubridate::year(.data$mydate)) |>
  dplyr::select(gwcode, year, best, low, high)

ucdp <- area_weighted_synthetic_brds(2019) |>
  dplyr::filter(year < 1989) |>
  dplyr::select(gwcode, year, best, low, high)

ucdpbrds_static <- dplyr::bind_rows(ucdp, ged) |> arrange(gwcode, year)
usethis::use_data(ucdpbrds_static, overwrite = TRUE)
