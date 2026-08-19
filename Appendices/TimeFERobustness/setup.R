# Settings and shared helpers for the time-fixed-effects robustness appendix.
#
# How to run: sourced by every stage; the stages are launched from the
#   repository root by
#   Rscript Appendices/TimeFERobustness/run.R <command>
# Inputs:  optional TIMEFE_{RAW,PROCESSED,RESULTS}_DIR environment overrides,
#   which redirect the storage roots for isolated validation.
# Outputs: defines timefeSettings and the helpers below, loads the shared
#   packages, and creates this module's data and results folders.
#
# This module is deliberately isolated from the rest of the repository: it
# reads pinned external libraries rather than ../Data/Processed, and it needs
# packages the main pipeline does not. Nothing here should be sourced by a
# main-pipeline script, and nothing here should read a main-pipeline cache.
#
# Definitions with more than one consumer live here, plus the year parsers,
# which have a single consumer but are the fiddliest code in the module and
# are covered by tests/test_timefe_robustness_module.R. Every other
# single-consumer input reader lives in the stage that uses it.

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

# Every stage runs as its own Rscript with the repository root as the working
# directory; run.R sets it. Check it here so a hand-run stage fails clearly.
if (!file.exists("Appendices/TimeFERobustness/setup.R")) {
  stop(
    "error: run this module from the flex-mining/ working directory, ",
    "normally through Appendices/TimeFERobustness/run.R",
    call. = FALSE
  )
}

# Suppress the stray Rplots.pdf that R creates for a transient graphics device.
pdf(NULL)
Sys.setenv(TZ = "America/New_York")
options(stringsAsFactors = FALSE)

# Settings ================================================================
#
# Authored as one list per concern and assembled at the end, so they read as
# timefeSettings$<group>$<name>, following the repository's own config.R.

# Pinned external input versions ------------------------------------------
# The appendix is built from external libraries that are revised over time, so
# every input is pinned. Changing a pin changes the sample, and the counts
# checked at the end of exhibits.R are expected to move with it.
pinSettings <- list(
  release = "2024_10",  # Open Source Asset Pricing release
  jkp_commit = "98adb75ddc66d2cc47613dcab745b0ea6260e902",
  cz_mapping_commit = "e4a1d728caea04e68868614ad938d0293c5d0b11",
  cz_mapping_sha256 = paste0(
    "2d95e3ee4d49cdea9e1e55cd653e8131bd53766566c49eb6f",
    "a357e4b9dcc61bf"
  )
)

# Sample screen -----------------------------------------------------------
# Every specification in this appendix keeps only signals whose in-sample
# long-short return has a signed t-statistic above this threshold. The screen
# is imposed in exactly one place, quality_screen() below.
screenSettings <- list(
  t_min = 2
)

# Storage layout ----------------------------------------------------------
# Roots follow the repository's ../Data and ../Results layout and can be
# redirected with environment variables so a validation run cannot touch the
# real data folders.
rawDir <- Sys.getenv("TIMEFE_RAW_DIR", unset = "../Data/Raw/TimeFERobustness")
processedDir <- Sys.getenv(
  "TIMEFE_PROCESSED_DIR", unset = "../Data/Processed/TimeFERobustness"
)
resultsDir <- Sys.getenv(
  "TIMEFE_RESULTS_DIR", unset = "../Results/TimeFERobustness"
)

pathSettings <- list(
  module = "Appendices/TimeFERobustness",
  raw = rawDir,
  processed = processedDir,
  # Downloaded JKP factor returns and factor metadata.
  jkp = file.path(rawDir, "jkp"),
  # Downloaded Open Source Asset Pricing files, and the constructed signal
  # panel and CRSP cache built from them.
  openap_raw = file.path(rawDir, "opensourceap"),
  openap_processed = file.path(processedDir, "opensourceap"),
  # Machine-readable regression results, read by exhibits.R.
  output = file.path(processedDir, "output"),
  # Generated TeX exhibits.
  exhibits = resultsDir
)

# Individual pinned files -------------------------------------------------
fileSettings <- list(
  op_portfolios = file.path(
    pathSettings$openap_raw, paste0("op-", pinSettings$release, ".csv")
  ),
  signal_doc = file.path(
    pathSettings$openap_raw, paste0("SignalDoc-", pinSettings$release, ".csv")
  ),
  meta_replications = file.path(
    pathSettings$openap_raw, "Comparison_to_MetaReplications.csv"
  ),
  signal_panel = file.path(
    pathSettings$openap_processed,
    paste0("signed_predictors_all_wide-", pinSettings$release, ".parquet")
  ),
  crsp_cache = file.path(
    pathSettings$openap_processed,
    "crsp-monthly-returns-market-equity.parquet"
  ),
  jkp_details = file.path(pathSettings$jkp, "factor_details.xlsx")
)

rm(rawDir, processedDir, resultsDir)

timefeSettings <- list(
  pins   = pinSettings,
  screen = screenSettings,
  paths  = pathSettings,
  files  = fileSettings
)

for (.dir in timefeSettings$paths[c(
  "raw", "processed", "jkp", "openap_raw", "openap_processed", "output",
  "exhibits"
)]) {
  dir.create(.dir, recursive = TRUE, showWarnings = FALSE)
}
rm(.dir)

# Assertions ==============================================================

# Used throughout the module. The message is a sprintf template so a failure
# can name the offending file or value.
check <- function(ok, ...) {
  if (!isTRUE(ok)) stop(sprintf(...), call. = FALSE)
}

# Shared input readers ====================================================

# JKP records sample periods and citations as free text ("1963--1982",
# "Fama and French (1992)"), so the years have to be pulled out of the string.
# Only estimate.R calls these, but they are the fiddliest code in the module
# and live here so tests/test_timefe_robustness_module.R can reach them
# without running a stage.

parse_first_year <- function(x) {
  answer <- rep(NA_integer_, length(x))
  good <- !is.na(x) & grepl("[12][0-9]{3}", x)
  answer[good] <- as.integer(
    sub(".*?([12][0-9]{3}).*", "\\1", x[good], perl = TRUE)
  )
  answer
}

parse_period <- function(x, which = c("start", "end")) {
  which <- match.arg(which)
  hits <- gregexpr("[12][0-9]{3}", x)
  years <- regmatches(x, hits)
  position <- if (which == "start") 1L else 2L

  vapply(years, function(value) {
    if (length(value) < position || identical(value, character(0))) {
      NA_integer_
    } else {
      as.integer(value[position])
    }
  }, integer(1))
}

# Published-predictor metadata: in-sample period and publication year, named
# to match the JKP metadata so one panel builder serves both libraries.
cz_predictor_doc <- function(path = timefeSettings$files$signal_doc,
                             continuous_only = FALSE) {
  check(file.exists(path), "%s does not exist.", path)
  doc <- fread(path)
  required <- c(
    "Acronym", "Cat.Signal", "Cat.Form", "SampleStartYear", "SampleEndYear",
    "Year"
  )
  check(all(required %in% names(doc)),
        "Signal documentation has an unexpected schema.")
  doc <- doc[`Cat.Signal` == "Predictor"]
  if (continuous_only) doc <- doc[`Cat.Form` == "continuous"]
  doc[, .(
    signalname = Acronym,
    SampleStartYear = as.integer(SampleStartYear),
    SampleEndYear = as.integer(SampleEndYear),
    pubYear = as.integer(Year)
  )]
}

# Decay panels and regressions ============================================
#
# A panel is a data.table of monthly long-short returns with one row per
# signal-month and the columns
#
#   signalname, date, retPct, SampleStartYear, SampleEndYear, pubYear
#
# Returns are always percent per month, whatever units the source library
# publishes, so the scaling and reporting helpers need no unit argument.
# Panels are built by composing four steps in this order:
#
#   add_event_indicators()  post-sample and post-publication indicators
#   (caller-specific subsetting, e.g. a truncated end year)
#   add_in_sample_stats()   per-signal in-sample mean, t-statistic, months
#   quality_screen()        keep signals with in-sample t above the threshold
#   scale_by_signal_mean()  or grand_mean_scale(), for the reported LHS
#
# The order matters: subsetting before add_in_sample_stats() means the
# in-sample moments describe the panel actually estimated on.

# Cumulative post-sample and post-publication indicators, plus the yr and
# yyyymm keys used for clustering and time fixed effects. Months before a
# signal's in-sample period, and signals with no publication year, are dropped.
#
# pub_month = NULL turns postPubC on in the calendar year after pubYear.
# An integer 1..12 instead turns it on after month-end of that month in
# pubYear, which is the McLean-Pontiff publication-date convention.
add_event_indicators <- function(d, pub_month = NULL) {
  d <- d[!is.na(pubYear)]
  d[, yr := year(date)]
  d <- d[yr >= SampleStartYear]
  d[, postSampC := as.integer(yr > SampleEndYear)]
  if (is.null(pub_month)) {
    d[, postPubC := as.integer(yr > pubYear)]
  } else {
    check(pub_month %in% 1:12, "pub_month must be in 1,...,12.")
    next_month <- as.IDate(paste(
      d$pubYear + as.integer(pub_month == 12L),
      ifelse(pub_month == 12L, 1L, pub_month + 1L),
      "01", sep = "-"
    ))
    d[, postPubC := as.integer(date >= next_month)]
  }
  d[, yyyymm := yr * 100L + month(date)]
  d[]
}

# Per-signal in-sample moments, merged onto the panel. The t-statistic is
# computed the same way for every library: mean(ret) / (sd(ret) / sqrt(n))
# over the months the source documents as in-sample.
add_in_sample_stats <- function(d) {
  stats <- d[postSampC == 0L, .(
    in_sample_mean_pct = mean(retPct),
    in_sample_tstat = mean(retPct) / (sd(retPct) / sqrt(.N)),
    in_sample_months = .N
  ), by = signalname]
  merge(d, stats, by = "signalname")
}

# The one screen this appendix imposes. Every reported specification keeps
# only signals whose in-sample return is reliably positive, which is also what
# makes scaling by the in-sample mean well behaved.
#
# Without it, the JKP panel produces a large POSITIVE post-sample coefficient,
# the opposite sign of the decay found everywhere else. The cause is the
# scaling: a handful of cited JKP factors have an in-sample mean within a few
# basis points of zero, and dividing by that mean turns ordinary monthly
# returns into retScaled values of five to six figures, so a few factor-months
# dominate the regression. debt_me alone (in-sample mean 2.3 bps/month, t-stat
# 0.23) accounts for most of it; age is a secondary contributor. Winsorizing
# or trimming shrinks but does not remove the artifact, because it is
# concentrated in specific factors rather than in a few months. Screening on
# the in-sample t-statistic removes it entirely, because it excludes the
# mechanism -- an unreliable scale denominator -- rather than its symptoms.
quality_screen <- function(d, t_min = timefeSettings$screen$t_min) {
  check("in_sample_tstat" %in% names(d),
        "quality_screen() needs add_in_sample_stats() first.")
  d[is.finite(in_sample_mean_pct) & is.finite(in_sample_tstat) &
      in_sample_tstat > t_min]
}

# retScaled gives every signal an in-sample mean of 100, so a coefficient
# reads as a percentage of the signal's own in-sample return. Signals with a
# nonpositive in-sample mean have no usable scale and are dropped; after
# quality_screen() there are none, because t > 0 already implies mean > 0.
scale_by_signal_mean <- function(d) {
  d <- d[in_sample_mean_pct > 0]
  d[, retScaled := 100 * retPct / in_sample_mean_pct]
  d[]
}

# retGrandScaled instead divides every signal by one common denominator, the
# equal-weighted average of the signals' in-sample means. Unlike retScaled it
# preserves the relative size of the factors, which is what makes the levels
# comparable with McLean-Pontiff's published table. The denominator is
# recorded on the panel so the reported rows can carry it.
grand_mean_scale <- function(d) {
  grand_mean_pct <- mean(
    d[postSampC == 0L, .(factor_mean_pct = mean(retPct)),
      by = signalname]$factor_mean_pct
  )
  check(is.finite(grand_mean_pct) && grand_mean_pct > 0,
        "The grand in-sample mean must be positive.")
  d <- copy(d)
  d[, retGrandScaled := 100 * retPct / grand_mean_pct]
  setattr(d, "grand_mean_pct", grand_mean_pct)
  d[]
}

grand_scale_label <- function(d) {
  sprintf("grand mean %.2f bps/month = 100",
          100 * attr(d, "grand_mean_pct"))
}

# Every model in the appendix is the same regression: cumulative post-sample
# and post-publication indicators with signal fixed effects, optionally plus
# month fixed effects, standard errors two-way clustered by signal and month.
# Singleton fixed effects are dropped so the estimates line up with the
# pyfixest scripts this appendix replaces (pyfixest drops them by default,
# base fixest does not).
estimate_decay <- function(d, lhs, time_fe) {
  fixed_effects <- if (time_fe) "signalname + yyyymm" else "signalname"
  feols(
    as.formula(paste(lhs, "~ postSampC + postPubC |", fixed_effects)),
    data = d,
    cluster = ~ signalname + yyyymm,
    fixef.rm = "singleton",
    notes = FALSE
  )
}

# Estimate one specification with and without month fixed effects and return
# the two reported rows. Columns passed through ... identify the
# specification and lead the result, so each stage can label its own rows
# while every results CSV shares one set of statistics columns.
decay_rows <- function(d, lhs, ...) {
  check(nrow(d) > 0L, "No observations for %s.",
        paste(c(...), collapse = ", "))
  check(all(c("in_sample_mean_pct", "postSampC", "postPubC") %in% names(d)),
        paste("decay_rows() needs a panel built by add_event_indicators()",
              "and add_in_sample_stats()."))
  signal_means <- unique(d[, .(signalname, in_sample_mean_pct)])
  grand_mean_pct <- attr(d, "grand_mean_pct")

  rbindlist(lapply(c(FALSE, TRUE), function(time_fe) {
    fit <- estimate_decay(d, lhs, time_fe)
    b <- coef(fit)
    v <- vcov(fit)
    data.table(
      ...,
      fixed_effects = if (time_fe) "predictor + month" else "predictor",
      post_sample = unname(b["postSampC"]),
      post_sample_se = sqrt(v["postSampC", "postSampC"]),
      additional_post_publication = unname(b["postPubC"]),
      additional_post_publication_se = sqrt(v["postPubC", "postPubC"]),
      total_post_publication_change = unname(b["postSampC"] + b["postPubC"]),
      total_post_publication_change_se = sqrt(
        v["postSampC", "postSampC"] + v["postPubC", "postPubC"] +
          2 * v["postSampC", "postPubC"]
      ),
      # The denominator behind the reported units, when the LHS was scaled by
      # a single grand mean rather than signal by signal.
      normalization_mean_bps = if (is.null(grand_mean_pct)) {
        NA_real_
      } else {
        100 * grand_mean_pct
      },
      mean_in_sample_bps = 100 * mean(signal_means$in_sample_mean_pct),
      min_in_sample_mean_bps = 100 * min(signal_means$in_sample_mean_pct),
      observations = fit$nobs,
      factors = uniqueN(d$signalname),
      singleton_observations_removed = nrow(d) - fit$nobs
    )
  }), use.names = TRUE)
}
