#' Default path of the poldat DuckDB research database
#'
#' @return A file path inside \code{rappdirs::user_data_dir("R-poldat")}.
#' @export
poldat_db_path <- function() file.path(rappdirs::user_data_dir("R-poldat"), "poldat.duckdb")

#' Connect to the poldat DuckDB research database
#'
#' Only one read-write connection (process) may hold the file at a time; any number of
#' read-only processes may connect concurrently. Close with \code{DBI::dbDisconnect(con)}.
#'
#' @param path Path to the DuckDB file.
#' @param read_only If \code{TRUE}, open the file read-only.
#' @return A DBI connection.
#' @export
poldat_db_connect <- function(path = poldat_db_path(), read_only = FALSE) {
  if (!read_only) dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  DBI::dbConnect(duckdb::duckdb(dbdir = path, read_only = read_only))
}

#' Build a self-contained DuckDB research database with static_world
#'
#' Downloads every source, stores it as a pristine raw table, derives harmonised
#' gwcode-year input tables (\code{sw_*}) and computes \code{static_world} in SQL from them.
#' The database is meant as the user's end-to-end store: connect with \code{\link{poldat_db_connect}}
#' and write own analysis sets and simulation results with plain DBI.
#'
#' Tables:
#' \describe{
#'   \item{\code{wdi_raw}}{World Bank WDI bulk CSV (Data360 \code{WB_WDI_WIDEF.csv}), all columns, years wide.}
#'   \item{\code{wdi_long}}{\code{wdi_raw} unpivoted to one row per series and year (missing values dropped).}
#'   \item{\code{vdem_raw}}{\code{vdemdata::vdem}, all columns.}
#'   \item{\code{ucdpbrds}}{Package data \code{ucdpbrds}: battle deaths by gwcode-year, derived from GED 25.1 (from 1989) and PRIO BRD 3.1 with hand-coded battle-location shares (before 1989).}
#'   \item{\code{ucdp_ged_raw}}{UCDP GED 25.1 event CSV (\code{GEDEvent_v25_1.csv}), all columns.}
#'   \item{\code{prio_brd_raw}}{PRIO Battle Deaths 3.1 xls (sheet \code{bdonly}), original column names and codes.}
#'   \item{\code{ucdp_prio_acd_raw}}{UCDP/PRIO Armed Conflict Dataset 23.1 CSV (\code{UcdpPrioConflict_v23_1.csv}), all columns.}
#'   \item{\code{ucdp_translate_conf_raw}}{UCDP old-to-new conflict id translation table (\code{translate_conf.csv}).}
#'   \item{\code{battlelocationsfatalityshares_raw}}{Hand-coded battle-location fatality shares CSV shipped in \code{inst/extdata}, all columns as text.}
#'   \item{\code{pwt_raw}}{Penn World Table 11.0 (Stata value labels dropped).}
#'   \item{\code{maddison_raw}}{Maddison Project Database 2020 (Stata value labels dropped).}
#'   \item{\code{wcde_past_epop_raw}}{\code{wcde::past_epop}.}
#'   \item{\code{fao_food_security_raw}}{FAOSTAT Suite of Food Security Indicators bulk CSV, all columns as text.}
#'   \item{\code{epr_raw}}{ETH ICR EPR Core 2023 CSV.}
#'   \item{\code{gwcode_lookup}}{Source code to Gleditsch-Ward code mapping (\code{source}, \code{code}, \code{gwcode}); unmatched codes have NULL \code{gwcode}.}
#'   \item{\code{sw_wdi}, \code{sw_vdem}, \code{sw_vdem_min}, \code{sw_ucdp}, \code{sw_pwt}, \code{sw_maddison}, \code{sw_wcde}, \code{sw_fao}, \code{sw_epr}}{Harmonised gwcode-year inputs to \code{static_world}.}
#'   \item{\code{static_world}}{The static-world panel, same columns as the \code{static_world} package data.}
#'   \item{\code{poldat_sources}}{Provenance log: \code{table_name}, \code{source}, \code{origin}, \code{retrieved_at}.}
#' }
#'
#' \code{static_world} is rebuilt after every call and requires all \code{sw_*} tables. A partial refresh
#' (e.g. \code{sources = "wdi"}) therefore works once every source has been loaded at least once.
#'
#' DuckDB allows only one read-write process per file. Parallel simulation workers must not
#' write to the database; return results to the parent session and write them there, e.g. with
#' \code{DBI::dbWriteTable(poldat_db_connect(), "my_results", results)}.
#'
#' @param path Path to the DuckDB file. Created if missing.
#' @param sources Sources to (re)load, in the given order.
#' @param static_year Year of the static world template passed to \code{\link{area_weighted_synthetic_data}}.
#' @return \code{path}, invisibly.
#' @export
#'
#' @examples
#' \dontrun{
#' static_world_duckdb()
#' con <- poldat_db_connect(read_only = TRUE)
#' DBI::dbGetQuery(con, "SELECT * FROM static_world WHERE year = 2019")
#' DBI::dbDisconnect(con)
#' }
static_world_duckdb <- function(path = poldat_db_path(),
                                sources = c("wdi", "vdem", "ucdp", "pwt", "maddison", "wcde", "fao", "epr"),
                                static_year = 2019) {
  sources <- match.arg(sources, several.ok = TRUE)
  con <- poldat_db_connect(path)
  on.exit(DBI::dbDisconnect(con), add = TRUE)

  for (src in sources) {
    message("poldat: loading ", src)
    switch(src,
      wdi = .db_load_wdi(con, static_year),
      vdem = .db_load_vdem(con, static_year),
      ucdp = .db_load_ucdp(con, static_year),
      pwt = .db_load_pwt(con, static_year),
      maddison = .db_load_maddison(con, static_year),
      wcde = .db_load_wcde(con, static_year),
      fao = .db_load_fao(con, static_year),
      epr = .db_load_epr(con, static_year)
    )
  }

  .db_build_static_world(con)
  invisible(path)
}

# ---- write helpers ---------------------------------------------------------

.db_exec <- function(con, sql) DBI::dbExecute(con, sql)

.db_write_df <- function(con, name, df) {
  df <- as.data.frame(dplyr::ungroup(df))
  duckdb::duckdb_register(con, "poldat_tmp_df", df)
  on.exit(duckdb::duckdb_unregister(con, "poldat_tmp_df"), add = TRUE)
  .db_exec(con, paste("CREATE OR REPLACE TABLE", DBI::dbQuoteIdentifier(con, name), "AS SELECT * FROM poldat_tmp_df"))
}

.db_write_sw <- function(con, name, df) {
  df <- df |>
    dplyr::ungroup() |>
    dplyr::filter(!is.na(gwcode)) |>
    dplyr::mutate(gwcode = as.integer(gwcode), year = as.integer(year))
  .db_write_df(con, name, df)
}

.db_record <- function(con, table_name, source, origin) {
  .db_exec(con, "CREATE TABLE IF NOT EXISTS poldat_sources (table_name VARCHAR PRIMARY KEY, source VARCHAR, origin VARCHAR, retrieved_at TIMESTAMP)")
  DBI::dbExecute(con, "INSERT OR REPLACE INTO poldat_sources VALUES (?, ?, ?, current_timestamp)",
                 params = list(table_name, source, origin))
}

.db_gwcode_lookup <- function(con, source, codes, origin) {
  country_name <- countrycode::countrycode(codes, origin = origin, destination = "country.name")
  gwcode <- countrycode::countrycode(country_name, origin = "country.name", destination = "gwn", custom_match = custom_gwcode_matches)

  .db_exec(con, "CREATE TABLE IF NOT EXISTS gwcode_lookup (source VARCHAR, code VARCHAR, gwcode INTEGER)")
  DBI::dbExecute(con, "DELETE FROM gwcode_lookup WHERE source = ?", params = list(source))
  duckdb::duckdb_register(con, "poldat_tmp_df", data.frame(source = source, code = codes, gwcode = as.integer(gwcode)))
  on.exit(duckdb::duckdb_unregister(con, "poldat_tmp_df"), add = TRUE)
  .db_exec(con, "INSERT INTO gwcode_lookup SELECT * FROM poldat_tmp_df")
}

.db_download <- function(url, fileext) {
  tmp <- tempfile(fileext = fileext)
  httr2::request(url) |> httr2::req_perform(path = tmp)
  tmp
}

# ---- source loaders --------------------------------------------------------

.db_load_wdi <- function(con, static_year) {
  url <- "https://data360files.worldbank.org/data360-data/data/WB_WDI/WB_WDI_WIDEF.csv"
  tmp <- .db_download(url, ".csv")
  on.exit(unlink(tmp), add = TRUE)

  header <- names(utils::read.csv(tmp, nrows = 0, check.names = FALSE))
  years <- grep("^[0-9]{4}$", header, value = TRUE)
  if (length(years) == 0) stop("WDI CSV has no year columns")
  types <- paste0("{", paste(sprintf("'%s': 'DOUBLE'", years), collapse = ", "), "}")
  year_cols <- paste(DBI::dbQuoteIdentifier(con, years), collapse = ", ")

  wdi_sum <- c(wdi_pop = "SP.POP.TOTL", wdi_gdp_pp_con_us = "NY.GDP.MKTP.PP.KD", wdi_gdp_pp_cur_us = "NY.GDP.MKTP.PP.CD")
  wdi_mean <- c(wdi_undernourishment = "SN.ITK.DEFC.ZS", wdi_imr = "SP.DYN.IMRT.IN", wdi_nmr = "SH.DYN.NMRT", wdi_gini = "SI.POV.GINI")
  to_code <- function(x) paste0("WB_WDI_", gsub(".", "_", x, fixed = TRUE))
  items <- c(
    sprintf("sum(value) FILTER (WHERE INDICATOR = '%s') AS %s", to_code(wdi_sum), names(wdi_sum)),
    sprintf("avg(value) FILTER (WHERE INDICATOR = '%s') AS %s", to_code(wdi_mean), names(wdi_mean))
  )
  codes <- paste(sprintf("'%s'", to_code(c(wdi_sum, wdi_mean))), collapse = ", ")

  DBI::dbWithTransaction(con, {
    .db_exec(con, paste0("CREATE OR REPLACE TABLE wdi_raw AS SELECT * FROM read_csv(", DBI::dbQuoteString(con, tmp),
                         ", header = true, sample_size = -1, types = ", types, ")"))
    .db_exec(con, paste0("CREATE OR REPLACE TABLE wdi_long AS ",
                         "WITH u AS (UNPIVOT wdi_raw ON ", year_cols, " INTO NAME year VALUE value) ",
                         "SELECT * REPLACE (CAST(year AS INTEGER) AS year) FROM u"))
    .db_gwcode_lookup(con, "wdi", DBI::dbGetQuery(con, "SELECT DISTINCT REF_AREA FROM wdi_raw")$REF_AREA, origin = "wb")
    .db_exec(con, paste0(
      "CREATE OR REPLACE TABLE sw_wdi AS SELECT m.gwcode, w.year, ", paste(items, collapse = ", "), " ",
      "FROM wdi_long w JOIN gwcode_lookup m ON m.source = 'wdi' AND m.code = w.REF_AREA ",
      "WHERE m.gwcode IS NOT NULL AND w.INDICATOR IN (", codes, ") ",
      "AND w.SEX = '_T' AND w.AGE = '_T' AND w.URBANISATION = '_T' ",
      "GROUP BY m.gwcode, w.year"))
    .db_record(con, "wdi_raw", "wdi", url)
    .db_record(con, "wdi_long", "wdi", "UNPIVOT of wdi_raw")
    .db_record(con, "sw_wdi", "wdi", "wdi_long aggregated to gwcode-year (sum: pop/GDP; mean: rates)")
  })
}

.db_load_vdem <- function(con, static_year) {
  vdem <- get_vdem(v2x_libdem, v2x_regime, v2x_rule, v2x_accountability, v2x_corr, v2xeg_eqdr, v2x_egal, v2x_polyarchy,
                   v2pepwrgen, e_peedgini, e_wbgi_gee, e_wbgi_vae, v2regdur) |>
    area_weighted_synthetic_data(static_year)
  vdem_min <- get_vdem(v2regendtype, .fun = min) |> area_weighted_synthetic_data(static_year)

  DBI::dbWithTransaction(con, {
    .db_write_df(con, "vdem_raw", vdemdata::vdem)
    .db_write_sw(con, "sw_vdem", vdem)
    .db_write_sw(con, "sw_vdem_min", vdem_min)
    .db_record(con, "vdem_raw", "vdem", paste0("vdemdata::vdem (vdemdata ", utils::packageVersion("vdemdata"), ")"))
    .db_record(con, "sw_vdem", "vdem", sprintf(
      "get_vdem(v2x_libdem, v2x_regime, v2x_rule, v2x_accountability, v2x_corr, v2xeg_eqdr, v2x_egal, v2x_polyarchy, v2pepwrgen, e_peedgini, e_wbgi_gee, e_wbgi_vae, v2regdur) |> area_weighted_synthetic_data(%s)",
      static_year))
    .db_record(con, "sw_vdem_min", "vdem", sprintf("get_vdem(v2regendtype, .fun = min) |> area_weighted_synthetic_data(%s)", static_year))
  })
}

.db_load_ucdp <- function(con, static_year) {
  ged_url <- "https://ucdp.uu.se/downloads/ged/ged251-csv.zip"
  acd_url <- "https://ucdp.uu.se/downloads/ucdpprio/ucdp-prio-acd-231-csv.zip"
  translate_url <- "https://ucdp.uu.se/downloads/actor/translate_conf.csv"
  brd_url <- "https://cdn.cloud.prio.org/files/d21ef702-a546-45a8-b3c9-5b520dcc1239/PRIO%20Battle%20Deaths%20Dataset%2031.xls?inline=true"

  exdir <- tempfile("ucdp")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  ged_zip <- .db_download(ged_url, ".zip")
  on.exit(unlink(ged_zip), add = TRUE)
  utils::unzip(ged_zip, files = "GEDEvent_v25_1.csv", exdir = exdir)
  acd_zip <- .db_download(acd_url, ".zip")
  on.exit(unlink(acd_zip), add = TRUE)
  utils::unzip(acd_zip, files = "UcdpPrioConflict_v23_1.csv", exdir = exdir)
  translate_csv <- .db_download(translate_url, ".csv")
  on.exit(unlink(translate_csv), add = TRUE)
  xls <- .db_download(brd_url, ".xls")
  on.exit(unlink(xls), add = TRUE)
  brd <- readxl::read_excel(xls, sheet = "bdonly", guess_max = 1e6)
  shares_csv <- system.file("extdata", "battlelocationsfatalityshares.csv", package = "poldat", mustWork = TRUE)

  ucdp <- ucdpbrds |> dplyr::select(gwcode, year, best, low, high) |> area_weighted_synthetic_data(static_year)

  DBI::dbWithTransaction(con, {
    .db_exec(con, paste0("CREATE OR REPLACE TABLE ucdp_ged_raw AS SELECT * FROM read_csv(",
                         DBI::dbQuoteString(con, file.path(exdir, "GEDEvent_v25_1.csv")), ", header = true, sample_size = -1)"))
    .db_exec(con, paste0("CREATE OR REPLACE TABLE ucdp_prio_acd_raw AS SELECT * FROM read_csv(",
                         DBI::dbQuoteString(con, file.path(exdir, "UcdpPrioConflict_v23_1.csv")), ", header = true, sample_size = -1)"))
    .db_exec(con, paste0("CREATE OR REPLACE TABLE ucdp_translate_conf_raw AS SELECT * FROM read_csv(",
                         DBI::dbQuoteString(con, translate_csv), ", header = true, sample_size = -1)"))
    .db_exec(con, paste0("CREATE OR REPLACE TABLE battlelocationsfatalityshares_raw AS SELECT * FROM read_csv(",
                         DBI::dbQuoteString(con, shares_csv), ", header = true, delim = ';', all_varchar = true)"))
    .db_write_df(con, "prio_brd_raw", brd)
    .db_write_df(con, "ucdpbrds", ucdpbrds)
    .db_write_sw(con, "sw_ucdp", ucdp)
    .db_record(con, "ucdp_ged_raw", "ucdp", paste0(ged_url, " (UCDP GED 25.1)"))
    .db_record(con, "ucdp_prio_acd_raw", "ucdp", paste0(acd_url, " (UCDP/PRIO ACD 23.1)"))
    .db_record(con, "ucdp_translate_conf_raw", "ucdp", translate_url)
    .db_record(con, "prio_brd_raw", "ucdp", paste0(brd_url, " (PRIO Battle Deaths 3.1, sheet bdonly)"))
    .db_record(con, "battlelocationsfatalityshares_raw", "ucdp", "poldat inst/extdata/battlelocationsfatalityshares.csv (hand-coded battle-location fatality shares)")
    .db_record(con, "ucdpbrds", "ucdp", "poldat::ucdpbrds (PRIO BRD 3.1 <1989, UCDP GED 25.1 >=1989)")
    .db_record(con, "sw_ucdp", "ucdp", sprintf(
      "ucdpbrds |> dplyr::select(gwcode, year, best, low, high) |> area_weighted_synthetic_data(%s)", static_year))
  })
}

.db_zap <- function(df) df |> haven::zap_labels() |> haven::zap_label() |> haven::zap_formats()

.db_load_pwt <- function(con, static_year) {
  pwt <- get_ggdc(dataset = "pwt", version = "11.0", gwcode = FALSE) |> .db_zap()

  DBI::dbWithTransaction(con, {
    .db_write_df(con, "pwt_raw", pwt)
    .db_gwcode_lookup(con, "pwt", unique(pwt$countrycode), origin = "iso3c")
    .db_exec(con, paste(
      "CREATE OR REPLACE TABLE sw_pwt AS",
      "SELECT m.gwcode, CAST(p.year AS INTEGER) AS year,",
      "sum(rgdpna) AS rgdpna, sum(rgdpe) AS rgdpe, sum(rgdpo) AS rgdpo, sum(pop) AS pwt_pop,",
      "sum(emp) AS emp, sum(cgdpe) AS cgdpe, sum(cgdpo) AS cgdpo",
      "FROM pwt_raw p JOIN gwcode_lookup m ON m.source = 'pwt' AND m.code = p.countrycode",
      "WHERE m.gwcode IS NOT NULL GROUP BY ALL"))
    .db_record(con, "pwt_raw", "pwt", "https://dataverse.nl/api/access/datafile/554030 (PWT 11.0)")
    .db_record(con, "sw_pwt", "pwt", "pwt_raw summed to gwcode-year via gwcode_lookup")
  })
}

.db_load_maddison <- function(con, static_year) {
  maddison <- get_ggdc(dataset = "maddison", version = "2020", gwcode = FALSE) |> .db_zap()

  DBI::dbWithTransaction(con, {
    .db_write_df(con, "maddison_raw", maddison)
    .db_gwcode_lookup(con, "maddison", unique(maddison$countrycode), origin = "iso3c")
    .db_exec(con, paste(
      "CREATE OR REPLACE TABLE sw_maddison AS",
      "SELECT m.gwcode, CAST(d.year AS INTEGER) AS year, sum(gdppc * pop) AS maddison_gdp, sum(pop) AS maddison_pop",
      "FROM maddison_raw d JOIN gwcode_lookup m ON m.source = 'maddison' AND m.code = d.countrycode",
      "WHERE m.gwcode IS NOT NULL GROUP BY ALL"))
    .db_record(con, "maddison_raw", "maddison", "https://www.rug.nl/ggdc/historicaldevelopment/maddison/data/mpd2020.dta (Maddison 2020)")
    .db_record(con, "sw_maddison", "maddison", "maddison_raw summed to gwcode-year via gwcode_lookup (gdp = sum(gdppc * pop))")
  })
}

.db_load_wcde <- function(con, static_year) {
  wcde <- wcde_gwcode |> dplyr::rename(wcde_pop = tot_pop)

  DBI::dbWithTransaction(con, {
    .db_write_df(con, "wcde_past_epop_raw", wcde::past_epop)
    .db_write_sw(con, "sw_wcde", wcde)
    .db_record(con, "wcde_past_epop_raw", "wcde", paste0("wcde::past_epop (wcde ", utils::packageVersion("wcde"), ")"))
    .db_record(con, "sw_wcde", "wcde", "poldat::wcde_gwcode (data-raw/wcde-gwcode.R)")
  })
}

.db_load_fao <- function(con, static_year) {
  url <- "https://bulks-faostat.fao.org/production/Food_Security_Data_E_All_Data.zip"
  zip <- .db_download(url, ".zip")
  on.exit(unlink(zip), add = TRUE)
  # Private directory: get_fao_food_security_uncached() extracts and deletes the same file name in tempdir().
  exdir <- tempfile("fao")
  on.exit(unlink(exdir, recursive = TRUE), add = TRUE)
  utils::unzip(zip, files = "Food_Security_Data_E_All_Data.csv", exdir = exdir)
  csv <- file.path(exdir, "Food_Security_Data_E_All_Data.csv")
  enc <- if (all(validUTF8(readLines(csv, warn = FALSE)))) "utf-8" else "latin-1"

  fs <- get_fao_food_security() |> dplyr::select(-"country_name")

  DBI::dbWithTransaction(con, {
    .db_exec(con, paste0("CREATE OR REPLACE TABLE fao_food_security_raw AS SELECT * FROM read_csv(", DBI::dbQuoteString(con, csv),
                         ", header = true, all_varchar = true, encoding = '", enc, "')"))
    .db_write_sw(con, "sw_fao", fs)
    .db_record(con, "fao_food_security_raw", "fao", url)
    .db_record(con, "sw_fao", "fao", "get_fao_food_security() |> dplyr::select(-\"country_name\")")
  })
}

.db_load_epr <- function(con, static_year) {
  epr <- epr_excluded_share() |>
    area_weighted_synthetic_data(static_year) |>
    dplyr::mutate(epr_excluded_share = dplyr::if_else(is.na(epr_excluded_share), 0, epr_excluded_share))
  # get_pgfile() reads priogrid's `pgsources` data unqualified, so priogrid must be attached;
  # epr_excluded_share() above attaches it with library(priogrid).
  f <- priogrid::get_pgfile(source_name = "ETH ICR EPR Core", source_version = "2023", id = "287bfdf7-2f4f-402a-88df-5fe1f8b7046b")

  DBI::dbWithTransaction(con, {
    .db_exec(con, paste0("CREATE OR REPLACE TABLE epr_raw AS SELECT * FROM read_csv(", DBI::dbQuoteString(con, f),
                         ", header = true, sample_size = -1)"))
    .db_write_sw(con, "sw_epr", epr)
    .db_record(con, "epr_raw", "epr", "priogrid::get_pgfile('ETH ICR EPR Core', '2023')")
    .db_record(con, "sw_epr", "epr", sprintf(
      "epr_excluded_share() |> area_weighted_synthetic_data(%s), NA shares set to 0", static_year))
  })
}

# ---- static_world ----------------------------------------------------------

.db_build_static_world <- function(con) {
  required <- c("sw_ucdp", "sw_vdem", "sw_vdem_min", "sw_maddison", "sw_pwt", "sw_wdi", "sw_wcde", "sw_fao", "sw_epr")
  missing <- setdiff(required, DBI::dbListTables(con))
  if (length(missing) > 0) {
    stop("static_world needs tables: ", paste(missing, collapse = ", "),
         ". Run static_world_duckdb() with the corresponding sources.")
  }
  .db_exec(con, .db_static_world_sql())
  .db_record(con, "static_world", "static_world", "SQL over sw_* tables (poldat:::.db_static_world_sql)")
}

.db_static_world_sql <- function() {
  interp_cols <- c("priprop", "secprop", "psecprop", "tdr", "ydr", "odr", "youth", "working", "elderly", "wcde_pop")
  interp <- paste(sprintf(paste0(
    "coalesce(%1$s, last_value(%1$s IGNORE NULLS) OVER wp + ",
    "(first_value(%1$s IGNORE NULLS) OVER wf - last_value(%1$s IGNORE NULLS) OVER wp) * ",
    "(year - last_value(CASE WHEN %1$s IS NOT NULL THEN year END IGNORE NULLS) OVER wp) / ",
    "(first_value(CASE WHEN %1$s IS NOT NULL THEN year END IGNORE NULLS) OVER wf - ",
    "last_value(CASE WHEN %1$s IS NOT NULL THEN year END IGNORE NULLS) OVER wp)) AS %1$s"), interp_cols),
    collapse = ",\n    ")

  growth_cols <- c(pwt_grwt_na = "rgdpna", wdi_grwt_con = "wdi_gdp_pp_con_us", maddison_grwt = "maddison_gdp",
                   pwt_pop_grwt = "pwt_pop", wdi_pop_grwt = "wdi_pop", wcde_pop_grwt = "wcde_pop", maddison_pop_grwt = "maddison_pop")
  growth <- paste(sprintf("(%1$s - lag(%1$s) OVER w) / nullif(lag(%1$s) OVER w, 0) AS %2$s", growth_cols, names(growth_cols)),
                  collapse = ",\n    ")

  sprintf("
CREATE OR REPLACE TABLE static_world AS
WITH keys AS (
  SELECT gwcode, year FROM sw_ucdp UNION SELECT gwcode, year FROM sw_vdem
), joined AS (
  SELECT * FROM keys
  LEFT JOIN sw_ucdp USING (gwcode, year)     LEFT JOIN sw_vdem USING (gwcode, year)
  LEFT JOIN sw_vdem_min USING (gwcode, year) LEFT JOIN sw_maddison USING (gwcode, year)
  LEFT JOIN sw_pwt USING (gwcode, year)      LEFT JOIN sw_wdi USING (gwcode, year)
  LEFT JOIN sw_wcde USING (gwcode, year)     LEFT JOIN sw_fao USING (gwcode, year)
  LEFT JOIN sw_epr USING (gwcode, year)
), interp AS (
  SELECT * REPLACE (
    %s
  ) FROM joined
  WINDOW wp AS (PARTITION BY gwcode ORDER BY year ROWS BETWEEN UNBOUNDED PRECEDING AND CURRENT ROW),
         wf AS (PARTITION BY gwcode ORDER BY year ROWS BETWEEN CURRENT ROW AND UNBOUNDED FOLLOWING)
), scaled AS (
  SELECT * REPLACE (wdi_pop / 1e6 AS wdi_pop, wcde_pop / 1e3 AS wcde_pop, maddison_pop / 1e3 AS maddison_pop,
                    wdi_gdp_pp_con_us / 1e6 AS wdi_gdp_pp_con_us, wdi_gdp_pp_cur_us / 1e6 AS wdi_gdp_pp_cur_us,
                    maddison_gdp / 1e3 AS maddison_gdp)
  FROM interp
), growth AS (
  SELECT *,
    %s,
    row_number() OVER w AS rn
  FROM scaled
  WINDOW w AS (PARTITION BY gwcode ORDER BY year)
), combined AS (
  SELECT *,
    coalesce(pwt_grwt_na, wdi_grwt_con, maddison_grwt) AS gdp_grwt,
    coalesce(wdi_pop_grwt, wcde_pop_grwt, pwt_pop_grwt, maddison_pop_grwt) AS pop_grwt,
    CASE WHEN year = 2017 THEN coalesce(cgdpo, wdi_gdp_pp_con_us, maddison_gdp) END AS gdp_anchor,
    CASE WHEN year = 2017 THEN coalesce(wdi_pop, wcde_pop, pwt_pop) END AS pop_anchor
  FROM growth
), cum AS (
  SELECT *,
    sum(coalesce(gdp_grwt, 0)) OVER wc AS cg_gdp, sum(CASE WHEN gdp_grwt IS NULL THEN 1 ELSE 0 END) OVER wc AS cn_gdp,
    sum(coalesce(pop_grwt, 0)) OVER wc AS cg_pop, sum(CASE WHEN pop_grwt IS NULL THEN 1 ELSE 0 END) OVER wc AS cn_pop
  FROM combined
  WINDOW wc AS (PARTITION BY gwcode ORDER BY year ROWS UNBOUNDED PRECEDING)
), anchored AS (
  SELECT *,
    max(gdp_anchor) OVER g AS a_gdp, max(pop_anchor) OVER g AS a_pop,
    max(CASE WHEN year = 2017 THEN rn END) OVER g AS a_rn,
    max(CASE WHEN year = 2017 THEN cg_gdp END) OVER g AS a_cg_gdp, max(CASE WHEN year = 2017 THEN cn_gdp END) OVER g AS a_cn_gdp,
    max(CASE WHEN year = 2017 THEN cg_pop END) OVER g AS a_cg_pop, max(CASE WHEN year = 2017 THEN cn_pop END) OVER g AS a_cn_pop
  FROM cum
  WINDOW g AS (PARTITION BY gwcode)
), chained AS (
  SELECT *,
    nullif(CASE WHEN cn_gdp = a_cn_gdp AND rn - a_rn BETWEEN -100 AND 15 THEN a_gdp * exp(cg_gdp - a_cg_gdp) END, 0) AS rgdp,
    nullif(CASE WHEN cn_pop = a_cn_pop AND rn - a_rn BETWEEN -100 AND 15 THEN a_pop * exp(cg_pop - a_cg_pop) END, 0) AS population
  FROM anchored
)
SELECT
  gwcode, year,
  rgdp, gdp_grwt, rgdp / population AS gdppc,
  (rgdp / population - lag(rgdp / population) OVER w) / nullif(lag(rgdp / population) OVER w, 0) AS gdppc_grwt,
  population, pop_grwt,
  best, low, high,
  epr_excluded_share,
  v2x_polyarchy, v2x_libdem, v2x_regime, v2x_accountability, v2x_corr, v2regdur, v2xeg_eqdr, v2x_egal, v2pepwrgen, v2regendtype, e_wbgi_gee, e_wbgi_vae, e_peedgini,
  priprop, secprop, psecprop, tdr, ydr, odr, youth, working, elderly,
  wdi_undernourishment, wdi_imr, wdi_nmr, wdi_gini,
  energy_supply, min_energy_req, calorie_var, food_variance, safe_water_pct, basic_water_pct, basic_sanit_pct, wasting_pct, wasting_num, stunting_pct,
  stunting_num, overweight_pct, overweight_num, obesity_pct, obesity_num, anemia_pct, anemia_num, breastfeed_pct, avg_energy_req, retail_loss, rail_density,
  safe_sanit_pct, low_birth_pct, low_birth_num, gdp_per_capita AS fao_gdp_per_capita,
  wcde_pop, nullif(pwt_pop, 0) AS pwt_pop, wdi_pop, nullif(maddison_pop, 0) AS maddison_pop,
  wcde_pop_grwt, pwt_pop_grwt, wdi_pop_grwt, maddison_pop_grwt,
  rgdpna, nullif(wdi_gdp_pp_con_us, 0) AS wdi_gdp_pp_con_us, nullif(maddison_gdp, 0) AS maddison_gdp,
  pwt_grwt_na, wdi_grwt_con, maddison_grwt,
  rgdpe, rgdpo, emp, cgdpe, cgdpo, wdi_gdp_pp_cur_us
FROM chained
WINDOW w AS (PARTITION BY gwcode ORDER BY year)
ORDER BY gwcode, year
", interp, growth)
}
