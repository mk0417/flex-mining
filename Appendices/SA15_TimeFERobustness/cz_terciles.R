# Estimate decay regressions on tercile portfolios built directly from
# Chen-Zimmermann firm-level signals, in equal-, capped-value-, and
# value-weighted form.
#
# How to run: from flex-mining/, normally through
#   Rscript Appendices/SA15_TimeFERobustness/run.R build
# Inputs:  the CZ signal panel and signal documentation under
#            ../Data/{Raw,Processed}/TimeFERobustness (see acquire.R)
#          CRSP.MSF, MSENAMES, and MSEDELIST via WRDS, cached after the first
#            download as crsp-monthly-returns-market-equity.parquet
#          ../Data/Processed/TimeFERobustness/output/jkp-cz-matched-pairs.csv
#          ../Data/Processed/TimeFERobustness/output/jkp-regressions.csv
#            (optional side-by-side comparison)
# Outputs: ../Data/Processed/TimeFERobustness/opensourceap/
#            crsp-monthly-returns-market-equity.parquet
#            cz-signal-terciles-{ew,weighted}-<release>.csv
#          ../Data/Processed/TimeFERobustness/output/
#            cz-signal-based-regressions.csv
#            cz-signal-based-diagnostics.csv
#            cz-signal-vs-jkp-regressions.csv
#
# The main diagnostic asks whether quality-screened signal-level CZ terciles
# reproduce the JKP results under JKP's own weighting schemes.
#
# This is the one analysis stage that runs on its own, rather than alongside
# the others in estimate.R: it holds a multi-gigabyte Arrow signal panel and
# the CRSP monthly file in memory, and running it in its own process releases
# that memory before the exhibits are rendered.
#
# Set CZ_SIGNAL_REBUILD=1 or CZ_SIGNAL_WEIGHTED_REBUILD=1 to rebuild the
# cached tercile returns instead of reading them.

source("Appendices/SA15_TimeFERobustness/setup.R")
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(RPostgres)
})

release <- timefeSettings$pins$release
cache_dir <- timefeSettings$paths$openap_processed
out_dir <- timefeSettings$paths$output

crsp_parquet <- timefeSettings$files$crsp_cache
ew_port_csv <- file.path(
  cache_dir, paste0("cz-signal-terciles-ew-", release, ".csv")
)
weighted_port_csv <- file.path(
  cache_dir, paste0("cz-signal-terciles-weighted-", release, ".csv")
)
official_quintile_csv <- file.path(
  timefeSettings$paths$openap_raw, paste0("quintiles_ew-", release, ".csv")
)
official_quintile_vw_csv <- file.path(
  timefeSettings$paths$openap_raw, paste0("quintiles_vw-", release, ".csv")
)
result_csv <- file.path(out_dir, "cz-signal-based-regressions.csv")
diagnostic_csv <- file.path(out_dir, "cz-signal-based-diagnostics.csv")
comparison_csv <- file.path(out_dir, "cz-signal-vs-jkp-regressions.csv")
matched_pairs_csv <- file.path(out_dir, "jkp-cz-matched-pairs.csv")
jkp_result_csv <- file.path(out_dir, "jkp-regressions.csv")

credential_helper <- "/workspace/.credentials/get-credentials.R"

next_yyyymm <- function(x) {
  year <- x %/% 100L
  month <- x %% 100L
  (year + as.integer(month == 12L)) * 100L +
    fifelse(month == 12L, 1L, month + 1L)
}

# ---- signal panel --------------------------------------------------------

check(
  dir.exists(timefeSettings$files$signal_panel),
  paste0("%s does not exist. Run `Rscript ",
         "Appendices/SA15_TimeFERobustness/run.R acquire` first."),
  timefeSettings$files$signal_panel
)
signal_dataset <- open_dataset(timefeSettings$files$signal_panel,
                               format = "parquet")

# ---- CRSP monthly return cache -------------------------------------------

download_crsp_returns <- function(path) {
  user <- Sys.getenv("WRDS_USER")
  pass <- Sys.getenv("WRDS_PASS")
  if (!nzchar(user) || !nzchar(pass)) {
    check(file.exists(credential_helper), "%s does not exist.",
          credential_helper)
    source(credential_helper)
    if (!nzchar(user)) user <- get_credential("WRDS_USERNAME", "wrds-username")
    if (!nzchar(pass)) pass <- get_credential("WRDS_PASSWORD", "wrds-password")
  }
  check(nzchar(user) && nzchar(pass), paste0(
    "WRDS credentials are required. Configure them through ",
    "/workspace/.credentials/get-credentials.R or set WRDS_USER/WRDS_PASS."
  ))

  message("Downloading CRSP monthly returns and delisting information ...")
  connection <- dbConnect(
    Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu", port = 9737, dbname = "wrds",
    sslmode = "require", user = user, password = pass
  )
  on.exit(dbDisconnect(connection), add = TRUE)

  crsp <- as.data.table(dbGetQuery(connection, paste(
    "select a.permno, a.date, a.ret, a.prc, a.shrout,",
    "b.exchcd, b.shrcd, c.dlstcd, c.dlret",
    "from crsp.msf as a",
    "left join crsp.msenames as b",
    "on a.permno = b.permno",
    "and b.namedt <= a.date and a.date <= b.nameendt",
    "left join crsp.msedelist as c",
    "on a.permno = c.permno",
    "and date_trunc('month', a.date) = date_trunc('month', c.dlstdt)"
  )))

  # Match the delisting-return treatment in OpenSourceAP/CrossSection.
  crsp[is.na(dlret) & (dlstcd == 500L | between(dlstcd, 520L, 584L)) &
         exchcd %in% c(1L, 2L), dlret := -0.35]
  crsp[is.na(dlret) & (dlstcd == 500L | between(dlstcd, 520L, 584L)) &
         exchcd == 3L, dlret := -0.55]
  crsp[!is.na(dlret) & dlret < -1, dlret := -1]
  crsp[is.na(dlret), dlret := 0]
  crsp[, retAdjusted := (1 + ret) * (1 + dlret) - 1]
  crsp[is.na(ret) & dlret != 0, retAdjusted := dlret]
  crsp[, `:=`(
    date = as.IDate(date),
    yyyymm = as.integer(format(date, "%Y%m")),
    retPct = 100 * retAdjusted,
    me = abs(prc) * shrout
  )]
  crsp <- crsp[is.finite(retPct) | (is.finite(me) & me > 0),
               .(permno, date, yyyymm, retPct, me, exchcd, shrcd)]
  check(crsp[, .N, by = .(permno, yyyymm)][N > 1L, .N] == 0L,
        "CRSP return data contain duplicate permno-month observations.")
  write_parquet(crsp, path, compression = "zstd")
  invisible(NULL)
}

if (!file.exists(crsp_parquet)) download_crsp_returns(crsp_parquet)
crsp <- as.data.table(read_parquet(crsp_parquet))
crsp_columns <- c("permno", "date", "yyyymm", "retPct", "me", "exchcd", "shrcd")
check(
  all(crsp_columns %in% names(crsp)),
  paste0("%s has an obsolete schema. Remove it and rerun with WRDS ",
         "credentials; required columns are: %s."),
  crsp_parquet, paste(crsp_columns, collapse = ", ")
)
crsp[, date := as.IDate(date)]
setkey(crsp, permno, yyyymm)

# JKP capped-value weights winsorize each stock's formation-month market
# equity at that month's NYSE 80th-percentile market equity.
nyse_p80 <- crsp[
  exchcd == 1L & shrcd %in% c(10L, 11L) & is.finite(me) & me > 0,
  .(nyse_p80 = quantile(me, 0.8, type = 1L, names = FALSE)),
  by = yyyymm
]
formation_weights <- merge(
  crsp[is.finite(me) & me > 0, .(permno, yyyymm, me)],
  nyse_p80, by = "yyyymm", all = FALSE
)
formation_weights[, me_cap := pmin(me, nyse_p80)]
check(
  formation_weights[
    !is.finite(me) | !is.finite(me_cap) | me <= 0 | me_cap <= 0 |
      me_cap > me | me_cap > nyse_p80, .N
  ] == 0L,
  "CRSP formation weights are invalid or violate the NYSE p80 cap."
)
setkey(formation_weights, permno, yyyymm)

# ---- tercile portfolios --------------------------------------------------

doc <- cz_predictor_doc(continuous_only = TRUE)
signals <- doc[signalname %in% signal_dataset$schema$names, signalname]
check(length(signals) >= 170L,
      "Expected at least 170 continuous CZ predictors; found %d.",
      length(signals))

# Rebalance monthly. For each signal month t, form thirds using all available
# firms, then measure the high-minus-low return in month t + 1. Discrete
# predictors are excluded because literal terciles are not defined when a
# signal has only a small number of categories.
signal_assignments <- function(signalname) {
  message("Building terciles for ", signalname, " ...")
  x <- as.data.table(
    signal_dataset |> select(permno, yyyymm, all_of(signalname)) |> collect()
  )
  setnames(x, signalname, "signal")
  x <- x[is.finite(signal)]
  if (!nrow(x)) return(NULL)

  breakpoints <- x[, as.list(quantile(
    signal, probs = c(1 / 3, 2 / 3), names = FALSE, na.rm = TRUE
  )), by = yyyymm]
  setnames(breakpoints, c("V1", "V2"), c("q1", "q2"))
  # Match the CZ portfolio code's treatment of degenerate extreme breakpoints.
  breakpoints <- breakpoints[is.finite(q1) & is.finite(q2) & q2 > q1]
  x <- breakpoints[x, on = "yyyymm", nomatch = 0L]
  x[, port := fifelse(signal <= q1, 1L, fifelse(signal < q2, 2L, 3L))]
  x[, formation_yyyymm := yyyymm]
  x[, yyyymm := next_yyyymm(formation_yyyymm)]
  x[, .(permno, formation_yyyymm, yyyymm, port)]
}

# Long-short row for each portfolio table: high leg minus low leg, with the
# smaller leg's stock count. by_columns identifies a portfolio-month within
# one signal ("date", plus "weighting_id" when several weightings share the
# table).
add_long_short <- function(ports, signal_id, by_columns) {
  high <- ports[portfolio == "3", c(by_columns, "retPct", "nstocks"),
                with = FALSE]
  setnames(high, c("retPct", "nstocks"), c("high_ret", "high_n"))
  low <- ports[portfolio == "1", c(by_columns, "retPct", "nstocks"),
               with = FALSE]
  setnames(low, c("retPct", "nstocks"), c("low_ret", "low_n"))
  long_short <- merge(high, low, by = by_columns)
  long_short[, `:=`(
    signalname = signal_id,
    portfolio = "LS",
    retPct = high_ret - low_ret,
    nstocks = pmin(high_n, low_n)
  )]
  keep <- c("signalname", by_columns, "portfolio", "retPct", "nstocks")
  rbind(ports[, ..keep], long_short[, ..keep])
}

build_one_signal_ew <- function(signalname) {
  x <- signal_assignments(signalname)
  if (is.null(x)) return(NULL)
  realized <- crsp[x[, .(permno, yyyymm, port)], on = .(permno, yyyymm),
                   nomatch = 0L][is.finite(retPct)]
  ports <- realized[, .(retPct = mean(retPct), nstocks = .N),
                    by = .(date, port)]
  ports[, `:=`(signalname = signalname, portfolio = as.character(port),
               port = NULL)]
  add_long_short(ports, signalname, "date")
}

build_one_signal_weighted <- function(signalname) {
  x <- signal_assignments(signalname)
  if (is.null(x)) return(NULL)
  check(all(x$yyyymm == next_yyyymm(x$formation_yyyymm)),
        "Weighted returns must be joined one month after portfolio formation.")
  # Weights are the formation-month market equity; returns are the following
  # month's.
  x <- formation_weights[x, on = .(permno, yyyymm = formation_yyyymm),
                         nomatch = 0L]
  realized <- crsp[
    x[, .(permno, yyyymm = i.yyyymm, port, weight_vw = me,
          weight_vw_cap = me_cap)],
    on = .(permno, yyyymm), nomatch = 0L
  ][is.finite(retPct)]

  ports <- realized[, .(
    vw = weighted.mean(retPct, weight_vw),
    vw_cap = weighted.mean(retPct, weight_vw_cap),
    nstocks = .N
  ), by = .(date, port)]
  ports <- melt(
    ports, id.vars = c("date", "port", "nstocks"),
    measure.vars = c("vw", "vw_cap"),
    variable.name = "weighting_id", value.name = "retPct"
  )
  ports[, `:=`(signalname = signalname, portfolio = as.character(port),
               port = NULL)]
  add_long_short(ports, signalname, c("date", "weighting_id"))
}

# Rebuilding the terciles takes hours, so the returns are cached beside the
# signal panel and reused unless a rebuild is requested.
cached_portfolios <- function(path, rebuild_var, builder, order_columns) {
  if (file.exists(path) && !identical(Sys.getenv(rebuild_var), "1")) {
    message("Using cached CZ signal tercile returns: ", path)
    portfolios <- fread(path)
    portfolios[, date := as.IDate(date)]
    return(portfolios)
  }
  portfolios <- rbindlist(lapply(signals, builder), use.names = TRUE)
  setorderv(portfolios, order_columns)
  fwrite(portfolios, path)
  portfolios
}

ew_portfolios <- cached_portfolios(
  ew_port_csv, "CZ_SIGNAL_REBUILD", build_one_signal_ew,
  c("signalname", "portfolio", "date")
)
weighted_portfolios <- cached_portfolios(
  weighted_port_csv, "CZ_SIGNAL_WEIGHTED_REBUILD", build_one_signal_weighted,
  c("weighting_id", "signalname", "portfolio", "date")
)

check(
  ew_portfolios[, .N, by = .(signalname, portfolio, date)][N > 1L, .N] == 0L,
  "EW tercile output contains duplicate signal-portfolio-month rows."
)
check(all(is.finite(ew_portfolios$retPct)),
      "EW tercile output contains non-finite returns.")
check(
  weighted_portfolios[
    , .N, by = .(weighting_id, signalname, portfolio, date)
  ][N > 1L, .N] == 0L,
  "Weighted tercile output contains duplicate rows."
)
check(
  setequal(unique(weighted_portfolios$weighting_id), c("vw", "vw_cap")) &&
    all(is.finite(weighted_portfolios$retPct)),
  "Weighted tercile output has invalid weighting identifiers or returns."
)

# ---- specifications ------------------------------------------------------

long_short_returns <- c(
  list(ew = ew_portfolios[portfolio == "LS", .(signalname, date, retPct)]),
  split(
    weighted_portfolios[portfolio == "LS",
                        .(signalname, date, retPct, weighting_id)],
    by = "weighting_id", keep.by = FALSE
  )
)
weighting_labels <- c(
  ew = "CZ signal EW terciles",
  vw_cap = "CZ signal capped-VW terciles",
  vw = "CZ signal VW terciles"
)

cz_signal_panel <- function(weighting_id) {
  scale_by_signal_mean(quality_screen(add_in_sample_stats(
    add_event_indicators(
      merge(long_short_returns[[weighting_id]], doc, by = "signalname")
    )
  )))
}

results <- list()
panels <- list()
for (id in names(weighting_labels)) {
  panel <- cz_signal_panel(id)
  panels[[id]] <- panel
  label <- unname(weighting_labels[[id]])
  results[[paste0(id, "_scaled")]] <- decay_rows(
    panel, "retScaled", weighting_id = id,
    specification = "baseline_quality_t2", label = label,
    scale = "in-sample mean = 100"
  )
  results[[paste0(id, "_unscaled")]] <- decay_rows(
    panel, "retPct", weighting_id = id,
    specification = "baseline_quality_t2_unscaled", label = label,
    scale = "percent/month"
  )
  grand <- grand_mean_scale(panel)
  results[[paste0(id, "_grand")]] <- decay_rows(
    grand, "retGrandScaled", weighting_id = id,
    specification = "baseline_quality_t2_grand_mean_scaled", label = label,
    scale = grand_scale_label(grand)
  )
}

# The JKP-metadata-matched subset of the equal-weighted panel, so the
# replication table can hold the signal set fixed across libraries.
check(file.exists(matched_pairs_csv),
      "%s does not exist. Run the time-FE robustness JKP step first.",
      matched_pairs_csv)
matched_signals <- unique(fread(matched_pairs_csv)$cz_signalname)
matched_panel <- panels[["ew"]][signalname %in% matched_signals]
check(uniqueN(matched_panel$signalname) > 0L,
      "No metadata-matched signals survive the CZ signal-level quality screen.")
results$ew_matched <- decay_rows(
  matched_panel, "retScaled", weighting_id = "ew",
  specification = "baseline_quality_t2_metadata_matched",
  label = unname(weighting_labels[["ew"]]), scale = "in-sample mean = 100"
)

result_table <- rbindlist(results, use.names = TRUE)
setorder(result_table, weighting_id, specification, fixed_effects)
fwrite(result_table, result_csv)

# ---- diagnostics ---------------------------------------------------------

unscreened_ew <- add_in_sample_stats(add_event_indicators(
  merge(long_short_returns[["ew"]], doc, by = "signalname")
))
diagnostics <- data.table(
  item = c(
    "continuous CZ predictors requested",
    "continuous CZ predictors with tercile returns",
    "tercile signal-month duplicates",
    "scaled factors with positive in-sample mean",
    "quality-screened factors retained (t>2)",
    "capped-VW quality-screened factors retained (t>2)",
    "VW quality-screened factors retained (t>2)",
    "metadata-matched EW factors retained (t>2)",
    "median stocks in the smaller LS leg"
  ),
  value = as.character(c(
    length(signals),
    uniqueN(long_short_returns[["ew"]]$signalname),
    ew_portfolios[, .N, by = .(signalname, portfolio, date)][N > 1L, .N],
    uniqueN(scale_by_signal_mean(unscreened_ew)$signalname),
    uniqueN(panels[["ew"]]$signalname),
    uniqueN(panels[["vw_cap"]]$signalname),
    uniqueN(panels[["vw"]]$signalname),
    uniqueN(matched_panel$signalname),
    round(median(ew_portfolios[portfolio == "LS", nstocks], na.rm = TRUE), 1)
  ))
)

# The official CZ quintile portfolios provide an independent audit of the
# sign and timing conventions, and for VW of the formation-month market-equity
# join. Terciles and quintiles need not be identical, but they should comove
# strongly signal by signal.
quintile_audit <- function(path, tercile_returns, screened_panel, weighting) {
  if (!file.exists(path)) return(NULL)
  official <- fread(path)[port == "LS", .(
    signalname, date = as.IDate(date), official_ret = ret
  )]
  audit <- merge(tercile_returns, official, by = c("signalname", "date"))[
    , .(correlation = cor(retPct, official_ret)), by = signalname
  ][signalname %in% unique(screened_panel$signalname)]
  median_correlation <- median(audit$correlation, na.rm = TRUE)
  positive_share <- mean(audit$correlation > 0, na.rm = TRUE)
  check(
    median_correlation > 0.7 && positive_share > 0.95,
    paste0("%s tercile audit against official CZ quintiles failed: median ",
           "correlation %.3f; positive share %.3f."),
    weighting, median_correlation, positive_share
  )
  data.table(
    item = c(
      sprintf("quality-screened median correlation with official CZ %s quintiles",
              weighting),
      sprintf(paste("quality-screened share of %s signal correlations with",
                    "official quintiles above zero"), weighting)
    ),
    value = as.character(c(round(median_correlation, 4),
                           round(positive_share, 4)))
  )
}

diagnostics <- rbindlist(list(
  diagnostics,
  quintile_audit(official_quintile_csv, long_short_returns[["ew"]],
                 panels[["ew"]], "EW"),
  quintile_audit(official_quintile_vw_csv, long_short_returns[["vw"]],
                 panels[["vw"]], "VW")
))
fwrite(diagnostics, diagnostic_csv)

# Put the directly comparable CZ-signal and JKP rows side by side.
comparison_specs <- c(
  "baseline_quality_t2", "baseline_quality_t2_unscaled",
  "baseline_quality_t2_grand_mean_scaled"
)
if (file.exists(jkp_result_csv)) {
  comparison <- merge(
    result_table[weighting_id == "ew" & specification %in% comparison_specs],
    fread(jkp_result_csv)[specification %in% comparison_specs],
    by = c("specification", "fixed_effects"),
    suffixes = c("_cz_signal", "_jkp")
  )
  setorder(comparison, specification, fixed_effects)
  fwrite(comparison, comparison_csv)
}

cat("\n========== CZ signal-based decay regressions ==========\n")
print(result_table, digits = 4)
cat("\n========== Diagnostics ==========\n")
print(diagnostics)
message("Saved ", ew_port_csv)
message("Saved ", weighted_port_csv)
message("Saved ", result_csv)
message("Saved ", diagnostic_csv)
if (file.exists(comparison_csv)) message("Saved ", comparison_csv)
