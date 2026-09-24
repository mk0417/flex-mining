# Estimate Section 3 MP-style decay regressions for two panels:
#   Panel (a) "Scaled Return"   = broad t>2 & uncorrelated benchmark
#                                 (raw_dm_benchmarks$accounting_t2_uncorr), scaled
#   Panel (b) "Unscaled Return" = matched & uncorrelated benchmark
#                                 (raw_dm_benchmarks$matched), unscaled
# Matching on in-sample t-stat and mean return controls for magnitude in the
# unscaled panel, playing the role that scaling plays in Panel (a).
#
# How to run: normally run through S3_Learning.R from flex-mining/.
# Inputs:  ../Data/Processed/raw_dm_benchmarks.RDS
# Outputs: ../Data/Processed/mp_style_decay_models.RDS
#
# S3b_MPStyleDecayTables.R renders the cached models into TeX.

rm(list = ls())
source("0_Environment.R")

benchmark_path <- "../Data/Processed/raw_dm_benchmarks.RDS"
benchmark <- readRDS(benchmark_path)

# Build a regression panel from a wide benchmark panel: published return versus
# one data-mined benchmark return (scaled or unscaled, per the columns passed),
# on the common complete-case sample from the original sample start onward.
build_reg_panel <- function(panel, pub_col, dm_col) {
  panel %>%
    transmute(
      pubname, eventDate, calendarDate, sampstart, sampend, pubdate,
      ret = .data[[pub_col]],
      matchRet = .data[[dm_col]],
      postSample = ifelse(calendarDate >= sampend, 1, 0),
      postPub = ifelse(calendarDate >= pubdate, 1, 0)
    ) %>%
    mutate(diffRet = ret - matchRet) %>%
    filter(
      calendarDate >= sampstart,
      complete.cases(ret, matchRet, postSample, postPub)
    )
}

# Panel (a): scaled returns of the broad excluding-correlated benchmark.
regData_a <- build_reg_panel(
  benchmark$accounting_t2_uncorr, "published_ret_scaled", "dm_ret_scaled"
)
# Panel (b): unscaled returns of the performance-matched benchmark (matching on
# in-sample stats controls for magnitude in lieu of scaling).
regData_b <- build_reg_panel(
  benchmark$matched, "published_ret_unscaled", "matched_uncorr_ret_unscaled"
)

if (nrow(regData_a) == 0L) stop("The scaled excl-corr regression panel is empty.")
if (nrow(regData_b) == 0L) stop("The unscaled matched regression panel is empty.")

cat(
  "S3a Panel (a) scaled, excl-corr:", nrow(regData_a), "signal-months,",
  dplyr::n_distinct(regData_a$pubname), "predictors\n"
)
cat(
  "S3a Panel (b) unscaled, matched:", nrow(regData_b), "signal-months,",
  dplyr::n_distinct(regData_b$pubname), "predictors\n"
)

fit_outcome <- function(lhs, data, time_fe = FALSE) {
  fixed_effects <- if (time_fe) "pubname + calendarDate" else "pubname"
  fixest::feols(
    stats::as.formula(paste0(lhs, " ~ postSample + postPub | ", fixed_effects)),
    data = data,
    cluster = ~pubname + calendarDate
  )
}

# Six fits per benchmark: academic / DM / difference, each without and with
# time fixed effects (S3b selects the no-FE set for Table 3, the FE set for
# Table 4).
fits_for <- function(data) list(
  fit_outcome("ret", data), fit_outcome("ret", data, TRUE),
  fit_outcome("matchRet", data), fit_outcome("matchRet", data, TRUE),
  fit_outcome("diffRet", data), fit_outcome("diffRet", data, TRUE)
)

panel_a <- fits_for(regData_a)
panel_b <- fits_for(regData_b)

for (fits in list(panel_a, panel_b)) {
  nobs_vec <- vapply(fits, stats::nobs, numeric(1))
  stopifnot(length(unique(nobs_vec)) == 1L)
}

saveRDS(
  list(
    metadata = list(
      benchmark_path = benchmark_path,
      panel_a = list(
        source = "accounting_t2_uncorr (scaled)",
        pair_fingerprint_sha256 =
          benchmark$metadata$accounting_t2_uncorr$pair_fingerprint_sha256,
        predictor_count = dplyr::n_distinct(regData_a$pubname),
        observation_count = nrow(regData_a)
      ),
      panel_b = list(
        source = "matched uncorr (unscaled)",
        pair_fingerprint_sha256 =
          benchmark$metadata$matched$pair_fingerprint_sha256,
        predictor_count = dplyr::n_distinct(regData_b$pubname),
        observation_count = nrow(regData_b)
      )
    ),
    panel_a = panel_a,
    panel_b = panel_b
  ),
  "../Data/Processed/mp_style_decay_models.RDS"
)
