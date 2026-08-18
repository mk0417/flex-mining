# McLean-Pontiff-style decay regressions using equal-, capped-value-, and
# value-weighted tercile portfolios built directly from Chen-Zimmermann
# firm-level signals.
#
# The main diagnostic asks whether quality-screened signal-level CZ terciles
# reproduce the JKP results. Every reported regression keeps only signals with
# a signed in-sample return t-statistic greater than 2.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R build
#
# Inputs:
#   WRDS credentials resolved by /workspace/.credentials/get-credentials.R
#   configured raw and processed Open Source Asset Pricing folders
#     signed_predictors_all_wide-2025_10.parquet/
#     SignalDoc-2025_10.csv
#   CRSP.MSF, CRSP.MSENAMES, and CRSP.MSEDELIST via WRDS (downloaded once)
#   ../Data/Processed/TimeFERobustness/output/jkp-regressions.csv (optional comparison)
#   ../Data/Processed/TimeFERobustness/output/jkp-cz-matched-pairs.csv
#
# Outputs:
#   configured processed Open Source Asset Pricing folder
#     crsp-monthly-returns-market-equity.parquet
#     cz-signal-terciles-ew-2025_10.csv
#     cz-signal-terciles-weighted-2025_10.csv
#   ../Data/Processed/TimeFERobustness/output/
#     cz-signal-based-regressions.csv
#     cz-signal-based-diagnostics.csv
#     cz-signal-vs-jkp-regressions.csv (when JKP results are available)

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(dplyr)
  library(fixest)
  library(RPostgres)
})

pdf(NULL)
Sys.setenv(TZ = "America/New_York")
source("Appendices/SA15_TimeFERobustness/R/config.R")

# ---- configuration --------------------------------------------------------

RELEASE <- TIMEFE_RELEASE
DATA_DIR <- TIMEFE_OPENAP_PROCESSED_DIR
OUT_DIR <- TIMEFE_OUTPUT_DIR

PANEL_PARQUET <- file.path(
  DATA_DIR, paste0("signed_predictors_all_wide-", RELEASE, ".parquet")
)
DOC_CSV <- TIMEFE_SIGNAL_DOC_CSV
CRSP_PARQUET <- file.path(
  DATA_DIR, "crsp-monthly-returns-market-equity.parquet"
)
EW_PORT_CSV <- file.path(
  DATA_DIR, paste0("cz-signal-terciles-ew-", RELEASE, ".csv")
)
WEIGHTED_PORT_CSV <- file.path(
  DATA_DIR, paste0("cz-signal-terciles-weighted-", RELEASE, ".csv")
)
OFFICIAL_QUINTILE_CSV <- file.path(
  TIMEFE_OPENAP_RAW_DIR, paste0("quintiles_ew-", RELEASE, ".csv")
)
OFFICIAL_QUINTILE_VW_CSV <- file.path(
  TIMEFE_OPENAP_RAW_DIR, paste0("quintiles_vw-", RELEASE, ".csv")
)
RESULT_CSV <- file.path(OUT_DIR, "cz-signal-based-regressions.csv")
DIAGNOSTIC_CSV <- file.path(OUT_DIR, "cz-signal-based-diagnostics.csv")
JKP_RESULT_CSV <- file.path(OUT_DIR, "jkp-regressions.csv")
MATCHED_PAIRS_CSV <- file.path(OUT_DIR, "jkp-cz-matched-pairs.csv")
COMPARISON_CSV <- file.path(OUT_DIR, "cz-signal-vs-jkp-regressions.csv")

dir.create(DATA_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

timefe_source("helpers.R")
credential_helper <- "/workspace/.credentials/get-credentials.R"
check(
  file.exists(credential_helper),
  "%s does not exist.", credential_helper
)
source(credential_helper)

next_yyyymm <- function(x) {
  year <- x %/% 100L
  month <- x %% 100L
  (year + as.integer(month == 12L)) * 100L +
    fifelse(month == 12L, 1L, month + 1L)
}

# ---- signal panel ---------------------------------------------------------

check(dir.exists(PANEL_PARQUET), paste0(
  "%s does not exist. Run `Rscript Appendices/SA15_TimeFERobustness/run.R acquire` first."
), PANEL_PARQUET)
check(file.exists(DOC_CSV), "%s does not exist.", DOC_CSV)
signal_dataset <- open_dataset(PANEL_PARQUET, format = "parquet")

# ---- CRSP monthly return cache -------------------------------------------

download_crsp_returns <- function(path) {
  user <- Sys.getenv("WRDS_USER")
  pass <- Sys.getenv("WRDS_PASS")
  if (!nzchar(user)) {
    user <- get_credential("WRDS_USERNAME", "wrds-username")
  }
  if (!nzchar(pass)) {
    pass <- get_credential("WRDS_PASSWORD", "wrds-password")
  }
  check(nzchar(user) && nzchar(pass), paste0(
    "WRDS credentials are required. Configure them through ",
    "/workspace/.credentials/get-credentials.R or set WRDS_USER/WRDS_PASS."
  ))

  message("Downloading CRSP monthly returns and delisting information ...")
  connection <- dbConnect(
    Postgres(),
    host = "wrds-pgdata.wharton.upenn.edu",
    port = 9737,
    dbname = "wrds",
    sslmode = "require",
    user = user,
    password = pass
  )
  on.exit(dbDisconnect(connection), add = TRUE)

  query <- paste(
    "select a.permno, a.date, a.ret, a.prc, a.shrout,",
    "b.exchcd, b.shrcd, c.dlstcd, c.dlret",
    "from crsp.msf as a",
    "left join crsp.msenames as b",
    "on a.permno = b.permno",
    "and b.namedt <= a.date and a.date <= b.nameendt",
    "left join crsp.msedelist as c",
    "on a.permno = c.permno",
    "and date_trunc('month', a.date) = date_trunc('month', c.dlstdt)"
  )
  crsp <- as.data.table(dbGetQuery(connection, query))

  # Match the delisting-return treatment in OpenSourceAP/CrossSection.
  crsp[
    is.na(dlret) &
      (dlstcd == 500L | between(dlstcd, 520L, 584L)) &
      exchcd %in% c(1L, 2L),
    dlret := -0.35
  ]
  crsp[
    is.na(dlret) &
      (dlstcd == 500L | between(dlstcd, 520L, 584L)) &
      exchcd == 3L,
    dlret := -0.55
  ]
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
  crsp <- crsp[
    is.finite(retPct) | (is.finite(me) & me > 0),
    .(permno, date, yyyymm, retPct, me, exchcd, shrcd)
  ]
  check(
    crsp[, .N, by = .(permno, yyyymm)][N > 1L, .N] == 0L,
    "CRSP return data contain duplicate permno-month observations."
  )
  write_parquet(crsp, path, compression = "zstd")
  invisible(NULL)
}

required_crsp_columns <- c(
  "permno", "date", "yyyymm", "retPct", "me", "exchcd", "shrcd"
)
if (!file.exists(CRSP_PARQUET)) download_crsp_returns(CRSP_PARQUET)
crsp <- as.data.table(read_parquet(CRSP_PARQUET))
check(
  all(required_crsp_columns %in% names(crsp)),
  paste0(
    "%s has an obsolete schema. Remove it and rerun with WRDS credentials; ",
    "required columns are: %s."
  ),
  CRSP_PARQUET, paste(required_crsp_columns, collapse = ", ")
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
  nyse_p80,
  by = "yyyymm",
  all = FALSE
)
formation_weights[, me_cap := pmin(me, nyse_p80)]
check(
  all(is.finite(formation_weights$me)) &&
    all(is.finite(formation_weights$me_cap)) &&
    all(formation_weights$me > 0) && all(formation_weights$me_cap > 0) &&
    all(formation_weights$me_cap <= formation_weights$me) &&
    all(formation_weights$me_cap <= formation_weights$nyse_p80),
  "CRSP formation weights are invalid or violate the NYSE p80 cap."
)
setkey(formation_weights, permno, yyyymm)

# ---- tercile portfolios --------------------------------------------------

doc <- fread(DOC_CSV)
required_doc_columns <- c(
  "Acronym", "Cat.Signal", "Cat.Form", "SampleStartYear",
  "SampleEndYear", "Year"
)
check(
  all(required_doc_columns %in% names(doc)),
  "Signal documentation has an unexpected schema."
)

available_columns <- signal_dataset$schema$names
signals <- doc[
  `Cat.Signal` == "Predictor" & Cat.Form == "continuous" &
    Acronym %in% available_columns,
  Acronym
]
check(length(signals) >= 170L,
      "Expected at least 170 continuous CZ predictors; found %d.",
      length(signals))

# Rebalance monthly. For each signal month t, form thirds using all available
# firms, then measure the equal-weighted high-minus-low return in month t + 1.
# Discrete predictors are excluded because literal terciles are not defined
# when a signal has only a small number of categories.
signal_assignments <- function(signalname) {
  message("Building terciles for ", signalname, " ...")
  selected <- signal_dataset |>
    select(permno, yyyymm, all_of(signalname)) |>
    collect()
  x <- as.data.table(selected)
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
  x[, port := fifelse(
    signal <= q1, 1L, fifelse(signal < q2, 2L, 3L)
  )]
  x[, formation_yyyymm := yyyymm]
  x[, yyyymm := next_yyyymm(formation_yyyymm)]
  x[, .(permno, formation_yyyymm, yyyymm, port)]
}

build_one_signal_ew <- function(signalname) {
  x <- signal_assignments(signalname)
  if (is.null(x)) return(NULL)
  realized <- crsp[
    x[, .(permno, yyyymm, port)],
    on = .(permno, yyyymm),
    nomatch = 0L
  ]
  realized <- realized[is.finite(retPct)]
  ports <- realized[, .(
    retPct = mean(retPct),
    nstocks = .N
  ), by = .(date, port)]
  ports[, `:=`(signalname = signalname, portfolio = as.character(port))]
  ports[, port := NULL]

  high <- ports[portfolio == "3", .(
    date, high_ret = retPct, high_n = nstocks
  )]
  low <- ports[portfolio == "1", .(
    date, low_ret = retPct, low_n = nstocks
  )]
  long_short <- merge(high, low, by = "date")
  long_short[, `:=`(
    signalname = signalname,
    portfolio = "LS",
    retPct = high_ret - low_ret,
    nstocks = pmin(high_n, low_n)
  )]
  long_short <- long_short[, .(
    signalname, date, portfolio, retPct, nstocks
  )]

  rbind(
    ports[, .(signalname, date, portfolio, retPct, nstocks)],
    long_short
  )
}

build_one_signal_weighted <- function(signalname) {
  x <- signal_assignments(signalname)
  if (is.null(x)) return(NULL)
  check(
    all(x$yyyymm == next_yyyymm(x$formation_yyyymm)),
    "Weighted returns must be joined one month after portfolio formation."
  )
  x <- formation_weights[
    x,
    on = .(permno, yyyymm = formation_yyyymm),
    nomatch = 0L
  ]
  realized <- crsp[
    x[, .(
      permno,
      yyyymm = i.yyyymm,
      port,
      weight_vw = me,
      weight_vw_cap = me_cap
    )],
    on = .(permno, yyyymm),
    nomatch = 0L
  ]
  realized <- realized[is.finite(retPct)]

  ports <- realized[, .(
    vw = weighted.mean(retPct, weight_vw),
    vw_cap = weighted.mean(retPct, weight_vw_cap),
    nstocks = .N
  ), by = .(date, port)]
  ports <- melt(
    ports,
    id.vars = c("date", "port", "nstocks"),
    measure.vars = c("vw", "vw_cap"),
    variable.name = "weighting_id",
    value.name = "retPct"
  )
  ports[, `:=`(
    signalname = signalname,
    portfolio = as.character(port)
  )]
  ports[, port := NULL]

  high <- ports[portfolio == "3", .(
    date, weighting_id, high_ret = retPct, high_n = nstocks
  )]
  low <- ports[portfolio == "1", .(
    date, weighting_id, low_ret = retPct, low_n = nstocks
  )]
  long_short <- merge(high, low, by = c("date", "weighting_id"))
  long_short[, `:=`(
    signalname = signalname,
    portfolio = "LS",
    retPct = high_ret - low_ret,
    nstocks = pmin(high_n, low_n)
  )]

  rbind(
    ports[, .(
      signalname, date, portfolio, weighting_id, retPct, nstocks
    )],
    long_short[, .(
      signalname, date, portfolio, weighting_id, retPct, nstocks
    )]
  )
}

rebuild_portfolios <- identical(Sys.getenv("CZ_SIGNAL_REBUILD"), "1")
if (!file.exists(EW_PORT_CSV) || rebuild_portfolios) {
  portfolio_list <- lapply(signals, build_one_signal_ew)
  ew_portfolios <- rbindlist(portfolio_list, use.names = TRUE)
  setorder(ew_portfolios, signalname, portfolio, date)
  fwrite(ew_portfolios, EW_PORT_CSV)
} else {
  message("Using cached CZ EW signal tercile returns: ", EW_PORT_CSV)
  ew_portfolios <- fread(EW_PORT_CSV)
  ew_portfolios[, date := as.IDate(date)]
}

rebuild_weighted <- identical(
  Sys.getenv("CZ_SIGNAL_WEIGHTED_REBUILD"), "1"
)
if (!file.exists(WEIGHTED_PORT_CSV) || rebuild_weighted) {
  portfolio_list <- lapply(signals, build_one_signal_weighted)
  weighted_portfolios <- rbindlist(portfolio_list, use.names = TRUE)
  setorder(weighted_portfolios, weighting_id, signalname, portfolio, date)
  fwrite(weighted_portfolios, WEIGHTED_PORT_CSV)
} else {
  message("Using cached CZ weighted signal tercile returns: ", WEIGHTED_PORT_CSV)
  weighted_portfolios <- fread(WEIGHTED_PORT_CSV)
  weighted_portfolios[, date := as.IDate(date)]
}

check(
  ew_portfolios[
    , .N, by = .(signalname, portfolio, date)
  ][N > 1L, .N] == 0L,
  "EW tercile output contains duplicate signal-portfolio-month rows."
)
check(
  all(is.finite(ew_portfolios$retPct)),
  "EW tercile output contains non-finite returns."
)
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

ew_ls_returns <- ew_portfolios[
  portfolio == "LS",
  .(signalname, date, retPct)
]
weighted_ls_returns <- split(
  weighted_portfolios[
    portfolio == "LS", .(signalname, date, retPct, weighting_id)
  ],
  by = "weighting_id",
  keep.by = FALSE
)

# ---- common preparation and estimation -----------------------------------

build_panel <- function(returns, scaled = TRUE) {
  d <- merge(
    returns,
    doc[
      `Cat.Signal` == "Predictor" & Cat.Form == "continuous",
      .(
        signalname = Acronym,
        SampleStartYear = as.integer(SampleStartYear),
        SampleEndYear = as.integer(SampleEndYear),
        pubYear = as.integer(Year)
      )
    ],
    by = "signalname"
  )
  d[, `:=`(
    yr = year(date),
    retDecimal = retPct / 100
  )]
  d <- d[yr >= SampleStartYear & !is.na(pubYear)]
  d[, `:=`(
    postSampC = as.integer(yr > SampleEndYear),
    postPubC = as.integer(yr > pubYear),
    yyyymm = yr * 100L + month(date)
  )]

  means <- d[postSampC == 0L, .(
    in_sample_mean_decimal = mean(retDecimal),
    in_sample_obs = .N
  ), by = signalname]
  d <- merge(d, means, by = "signalname", all.x = TRUE)
  d[, scale_eligible := (
    is.finite(in_sample_mean_decimal) & in_sample_obs > 0L &
      in_sample_mean_decimal > 0
  )]
  if (scaled) {
    d <- d[scale_eligible == TRUE]
    d[, retScaled := 100 * retDecimal / in_sample_mean_decimal]
  }
  d[]
}

estimate <- function(lhs, fixed_effects, data) {
  feols(
    as.formula(paste(
      lhs, "~ postSampC + postPubC |", fixed_effects
    )),
    data = data,
    cluster = ~ signalname + yyyymm,
    fixef.rm = "singleton",
    notes = FALSE
  )
}

model_row <- function(
    fit, specification, weighting_id, weighting, scale, time_fe, data) {
  b <- coef(fit)
  v <- vcov(fit)
  factor_means <- unique(
    data[, .(signalname, in_sample_mean_decimal)]
  )$in_sample_mean_decimal
  total <- b["postSampC"] + b["postPubC"]
  total_se <- sqrt(
    v["postSampC", "postSampC"] + v["postPubC", "postPubC"] +
      2 * v["postSampC", "postPubC"]
  )
  data.table(
    specification,
    weighting_id,
    weighting,
    scale,
    fixed_effects = if (time_fe) "predictor + month" else "predictor",
    post_sample = unname(b["postSampC"]),
    post_sample_se = sqrt(v["postSampC", "postSampC"]),
    additional_post_publication = unname(b["postPubC"]),
    additional_post_publication_se = sqrt(v["postPubC", "postPubC"]),
    total_post_publication_change = unname(total),
    total_post_publication_change_se = unname(total_se),
    normalization_mean_bps = if (is.null(attr(data, "grand_mean_pct"))) {
      NA_real_
    } else {
      100 * attr(data, "grand_mean_pct")
    },
    mean_in_sample_bps = 1e4 * mean(factor_means),
    min_in_sample_mean_bps = 1e4 * min(factor_means),
    observations = fit$nobs,
    factors = uniqueN(data$signalname),
    singleton_observations_removed = nrow(data) - fit$nobs
  )
}

results <- list()
run_pair <- function(
    specification, weighting_id, weighting, data, lhs, scale) {
  check(nrow(data) > 0L, "No observations for %s.", specification)
  for (time_fe in c(FALSE, TRUE)) {
    fixed_effects <- if (time_fe) {
      "signalname + yyyymm"
    } else {
      "signalname"
    }
    fit <- estimate(lhs, fixed_effects, data)
    key <- paste(
      weighting_id,
      specification,
      if (time_fe) "time_fe" else "predictor_fe",
      sep = "__"
    )
    results[[key]] <<- model_row(
      fit, specification, weighting_id, weighting, scale, time_fe, data
    )
  }
  invisible(NULL)
}

grand_mean_scale <- function(data) {
  grand_mean_pct <- mean(
    data[postSampC == 0L, .(factor_mean_pct = mean(retPct)),
         by = signalname]$factor_mean_pct
  )
  check(
    is.finite(grand_mean_pct) && grand_mean_pct > 0,
    "The grand in-sample mean must be positive."
  )
  d <- copy(data)
  d[, retGrandScaled := 100 * retPct / grand_mean_pct]
  attr(d, "grand_mean_pct") <- grand_mean_pct
  d[]
}

grand_scale_label <- function(data) {
  sprintf(
    "grand mean %.2f bps/month = 100",
    100 * attr(data, "grand_mean_pct")
  )
}

# ---- JKP-matched specifications ------------------------------------------

weighting_labels <- c(
  ew = "CZ signal EW terciles",
  vw_cap = "CZ signal capped-VW terciles",
  vw = "CZ signal VW terciles"
)
return_sets <- c(list(ew = ew_ls_returns), weighted_ls_returns)
quality_keeps <- list()
quality_stats_by_weighting <- list()

for (weighting_id in names(weighting_labels)) {
  weighting <- unname(weighting_labels[[weighting_id]])
  scaled <- build_panel(return_sets[[weighting_id]], scaled = TRUE)
  quality_stats <- scaled[postSampC == 0L, .(
    in_sample_mean_bps = 1e4 * mean(retDecimal),
    in_sample_tstat = mean(retDecimal) / (sd(retDecimal) / sqrt(.N))
  ), by = signalname]
  quality_keep <- quality_stats[
    is.finite(in_sample_tstat) & in_sample_tstat > 2,
    signalname
  ]
  quality_screened <- scaled[signalname %in% quality_keep]
  quality_keeps[[weighting_id]] <- quality_keep
  quality_stats_by_weighting[[weighting_id]] <- quality_stats

  run_pair(
    "baseline_quality_t2", weighting_id, weighting,
    quality_screened, "retScaled", "in-sample mean = 100"
  )
  run_pair(
    "baseline_quality_t2_unscaled", weighting_id, weighting,
    quality_screened, "retPct", "percent/month"
  )
  quality_grand_scaled <- grand_mean_scale(quality_screened)
  run_pair(
    "baseline_quality_t2_grand_mean_scaled", weighting_id, weighting,
    quality_grand_scaled, "retGrandScaled",
    grand_scale_label(quality_grand_scaled)
  )
}

check(
  file.exists(MATCHED_PAIRS_CSV),
  "%s does not exist. Run the time-FE robustness JKP step first.",
  MATCHED_PAIRS_CSV
)
metadata_matched_signals <- unique(
  fread(MATCHED_PAIRS_CSV)$cz_signalname
)
metadata_matched_keep <- intersect(
  quality_keeps[["ew"]], metadata_matched_signals
)
check(
  length(metadata_matched_keep) > 0L,
  "No metadata-matched signals survive the CZ signal-level quality screen."
)
metadata_matched <- build_panel(return_sets[["ew"]], scaled = TRUE)[
  signalname %in% metadata_matched_keep
]
run_pair(
  "baseline_quality_t2_metadata_matched",
  "ew",
  unname(weighting_labels[["ew"]]),
  metadata_matched,
  "retScaled",
  "in-sample mean = 100"
)

result_table <- rbindlist(results, use.names = TRUE)
setorder(result_table, weighting_id, specification, fixed_effects)
fwrite(result_table, RESULT_CSV)

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
    uniqueN(ew_ls_returns$signalname),
    ew_portfolios[
      , .N, by = .(signalname, portfolio, date)
    ][N > 1L, .N],
    uniqueN(build_panel(ew_ls_returns, scaled = TRUE)$signalname),
    length(quality_keeps[["ew"]]),
    length(quality_keeps[["vw_cap"]]),
    length(quality_keeps[["vw"]]),
    length(metadata_matched_keep),
    round(median(
      ew_portfolios[portfolio == "LS", nstocks], na.rm = TRUE
    ), 1)
  ))
)

# If the official CZ equal-weighted quintiles are cached, use them as an
# independent sign/timing audit. Terciles and quintiles need not be identical,
# but they should strongly comove signal by signal.
if (file.exists(OFFICIAL_QUINTILE_CSV)) {
  official <- fread(OFFICIAL_QUINTILE_CSV)[
    port == "LS",
    .(signalname, date = as.IDate(date), official_ret = ret)
  ]
  tercile <- ew_portfolios[
    portfolio == "LS",
    .(signalname, date, tercile_ret = retPct)
  ]
  audit_data <- merge(tercile, official, by = c("signalname", "date"))
  audit <- audit_data[, .(
    correlation = cor(tercile_ret, official_ret)
  ), by = signalname]
  audit <- audit[signalname %in% quality_keeps[["ew"]]]
  median_correlation <- median(audit$correlation, na.rm = TRUE)
  positive_correlation_share <- mean(
    audit$correlation > 0, na.rm = TRUE
  )
  check(
    median_correlation > 0.7 && positive_correlation_share > 0.95,
    paste0(
      "Tercile audit against official CZ quintiles failed: median ",
      "correlation %.3f; positive share %.3f."
    ),
    median_correlation, positive_correlation_share
  )
  diagnostics <- rbind(
    diagnostics,
    data.table(
      item = c(
        "quality-screened median correlation with official CZ EW quintiles",
        paste(
          "quality-screened share of signal correlations with official",
          "quintiles above zero"
        )
      ),
      value = as.character(c(
        round(median_correlation, 4),
        round(positive_correlation_share, 4)
      ))
    )
  )
}

# The official CZ value-weighted quintiles provide an independent audit of
# both the sign/timing convention and the formation-month market-equity join.
if (file.exists(OFFICIAL_QUINTILE_VW_CSV)) {
  official_vw <- fread(OFFICIAL_QUINTILE_VW_CSV)[
    port == "LS",
    .(signalname, date = as.IDate(date), official_ret = ret)
  ]
  tercile_vw <- weighted_portfolios[
    weighting_id == "vw" & portfolio == "LS",
    .(signalname, date, tercile_ret = retPct)
  ]
  audit_vw_data <- merge(
    tercile_vw, official_vw, by = c("signalname", "date")
  )
  audit_vw <- audit_vw_data[, .(
    correlation = cor(tercile_ret, official_ret)
  ), by = signalname]
  audit_vw <- audit_vw[signalname %in% quality_keeps[["vw"]]]
  median_vw_correlation <- median(audit_vw$correlation, na.rm = TRUE)
  positive_vw_correlation_share <- mean(
    audit_vw$correlation > 0, na.rm = TRUE
  )
  check(
    median_vw_correlation > 0.7 && positive_vw_correlation_share > 0.95,
    paste0(
      "VW tercile audit against official CZ quintiles failed: median ",
      "correlation %.3f; positive share %.3f."
    ),
    median_vw_correlation, positive_vw_correlation_share
  )
  diagnostics <- rbind(
    diagnostics,
    data.table(
      item = c(
        "quality-screened median correlation with official CZ VW quintiles",
        paste(
          "quality-screened share of VW signal correlations with official",
          "quintiles above zero"
        )
      ),
      value = as.character(c(
        round(median_vw_correlation, 4),
        round(positive_vw_correlation_share, 4)
      ))
    )
  )
}
fwrite(diagnostics, DIAGNOSTIC_CSV)

# Put the directly comparable CZ-signal and JKP rows side by side.
comparison_specs <- c(
  "baseline_quality_t2",
  "baseline_quality_t2_unscaled",
  "baseline_quality_t2_grand_mean_scaled"
)
if (file.exists(JKP_RESULT_CSV)) {
  jkp <- fread(JKP_RESULT_CSV)[specification %in% comparison_specs]
  cz <- result_table[
    weighting_id == "ew" & specification %in% comparison_specs
  ]
  comparison <- merge(
    cz,
    jkp,
    by = c("specification", "fixed_effects"),
    suffixes = c("_cz_signal", "_jkp")
  )
  setorder(comparison, specification, fixed_effects)
  fwrite(comparison, COMPARISON_CSV)
}

cat("\n========== CZ signal-based decay regressions ==========\n")
print(result_table, digits = 4)
cat("\n========== Diagnostics ==========\n")
print(diagnostics)
cat("\nWrote:\n")
cat("  ", EW_PORT_CSV, "\n", sep = "")
cat("  ", WEIGHTED_PORT_CSV, "\n", sep = "")
cat("  ", RESULT_CSV, "\n", sep = "")
cat("  ", DIAGNOSTIC_CSV, "\n", sep = "")
if (file.exists(COMPARISON_CSV)) {
  cat("  ", COMPARISON_CSV, "\n", sep = "")
}
