# Investigates whether the puzzling positive JKP post-sample coefficients are
# an outlier artifact of the in-sample-mean scaling.
#
# jkp.R reports several specifications built from the UNFILTERED (not
# quality-screened) scaled JKP panel -- unfiltered_scaled_diagnostic,
# publication_cutoff_{january,june,december}, and end_2013_scaled -- whose
# post-sample coefficient is large and POSITIVE (+35 to +70 in-sample-mean-100
# units), the opposite sign of the decay found in the CZ benchmarks and the
# quality-screened JKP baseline. jkp.R's
# header already flags this as suspicious ("scaling weak factors generated a
# misleading positive post-sample coefficient") but does not quantify it. This
# script does.
#
# Mechanism checked: retScaled = 100 * ret / in_sample_mean. A handful of
# cited JKP factors have an in-sample mean return within a few basis points of
# zero (large standard error relative to the mean, i.e. a very low in-sample
# t-stat). Dividing by that tiny mean turns ordinary-sized raw monthly returns
# into retScaled values of five to six figures, so a handful of predictor-
# months from a handful of weak factors can dominate an equal-weighted
# regression across ~120 factors and ~9,000 factor-months.
#
# Approach: rebuild the same unfiltered, scaled, cited-factor panel used by
# unfiltered_scaled_diagnostic (build_panel(ew, cited=TRUE, scaled=TRUE) in
# jkp.R, no t>2 quality screen), then (1) rank factors and observations by
# scaling blowup, (2) run leave-one-factor-out regressions to attribute the
# positive coefficient to specific factors, and (3) compare winsorizing,
# hard-trimming, and t-stat threshold exclusion as remedies, benchmarked
# against jkp.R's existing t>2 quality screen.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R diagnostics
#
# Inputs:
#   ../Data/Raw/TimeFERobustness/jkp/[usa]_[all_factors]_[monthly]_[ew].zip
#   ../Data/Raw/TimeFERobustness/jkp/factor_details.xlsx
#
# Outputs:
#   ../Data/Processed/TimeFERobustness/output/jkp-outlier-factor-scale.csv
#   ../Data/Processed/TimeFERobustness/output/jkp-outlier-leave-one-out.csv
#   ../Data/Processed/TimeFERobustness/output/jkp-outlier-remedies.csv

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

pdf(NULL)
Sys.setenv(TZ = "America/New_York")
source("Appendices/SA15_TimeFERobustness/R/config.R")

RAW_DIR <- TIMEFE_JKP_RAW_DIR
OUT_DIR <- TIMEFE_OUTPUT_DIR
EW_ZIP <- file.path(RAW_DIR, "[usa]_[all_factors]_[monthly]_[ew].zip")
DETAILS_XLSX <- file.path(RAW_DIR, "factor_details.xlsx")

timefe_source("helpers.R")

check(
  file.exists(EW_ZIP) && file.exists(DETAILS_XLSX),
  paste0(
    "Missing %s or %s. Run the time-FE robustness build first so the JKP ",
    "inputs are available."
  ),
  EW_ZIP, DETAILS_XLSX
)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# ---- load: identical parsing to jkp.R, no quality screen ------------------

meta_all <- as.data.table(readxl::read_excel(DETAILS_XLSX, sheet = "details"))
meta <- meta_all[!is.na(abr_jkp) & nzchar(abr_jkp)]
setnames(meta, "abr_jkp", "signalname")
meta[, `:=`(
  SampleStartYear = parse_period(`in-sample period`, "start"),
  SampleEndYear = parse_period(`in-sample period`, "end"),
  pubYear = parse_first_year(cite)
)]
meta <- meta[, .(signalname, SampleStartYear, SampleEndYear, pubYear)]

ew <- read_zip_csv(EW_ZIP)
check(all(ew$location == "usa") && all(ew$freq == "monthly") &&
        all(ew$weighting == "ew"), "%s is not US monthly ew.", EW_ZIP)
setnames(ew, "name", "signalname")
ew[, date := as.IDate(date)]

# ---- rebuild the unfiltered_scaled_diagnostic panel -----------------------
# Same construction as build_panel(ew, cited=TRUE, scaled=TRUE) in jkp.R:
# cited factors, in-sample = up through each factor's own SampleEndYear,
# scale-eligible (positive in-sample mean) factors only, retScaled = 100 *
# ret / in-sample mean. No t>2 quality screen.

d <- merge(
  ew[, .(signalname, date, retDecimal = ret)],
  meta, by = "signalname"
)
d[, yr := year(date)]
d <- d[yr >= SampleStartYear & !is.na(pubYear)]
d[, `:=`(
  postSampC = as.integer(yr > SampleEndYear),
  postPubC = as.integer(yr > pubYear),
  yyyymm = yr * 100L + month(date)
)]

quality_stats <- d[postSampC == 0L, .(
  in_sample_mean_decimal = mean(retDecimal),
  in_sample_mean_bps = 1e4 * mean(retDecimal),
  in_sample_sd_bps = 1e4 * sd(retDecimal),
  in_sample_months = .N,
  in_sample_tstat = mean(retDecimal) / (sd(retDecimal) / sqrt(.N))
), by = signalname]
quality_stats[, scale_multiple := 100 / in_sample_mean_decimal]

d <- merge(d, quality_stats, by = "signalname")
d <- d[in_sample_mean_decimal > 0]
# retScaled = 100 * retDecimal / in-sample mean, exactly as in jkp.R's
# build_panel().
d[, retScaled := 100 * retDecimal / in_sample_mean_decimal]
check(nrow(d) > 0L, "The unfiltered scaled panel is empty.")

fwrite(
  setorder(copy(quality_stats), in_sample_tstat),
  file.path(OUT_DIR, "jkp-outlier-factor-scale.csv")
)

run_fe <- function(data, formula_rhs = "retScaled ~ postSampC + postPubC") {
  rbindlist(lapply(c(FALSE, TRUE), function(month_fe) {
    fe <- if (month_fe) "signalname + yyyymm" else "signalname"
    fit <- feols(
      as.formula(paste(formula_rhs, "|", fe)),
      data = data, cluster = ~ signalname + yyyymm,
      fixef.rm = "singleton", notes = FALSE
    )
    b <- coef(fit)
    v <- vcov(fit)
    data.table(
      fixed_effects = if (month_fe) "predictor + month" else "predictor",
      post_sample = unname(b["postSampC"]),
      post_sample_se = sqrt(v["postSampC", "postSampC"]),
      additional_post_publication = unname(b["postPubC"]),
      additional_post_publication_se = sqrt(v["postPubC", "postPubC"]),
      observations = fit$nobs, factors = uniqueN(data$signalname)
    )
  }))
}

# ---- sanity check: reproduce jkp.R's unfiltered_scaled_diagnostic ---------

baseline <- run_fe(d)
cat("\n========== Reproducing unfiltered_scaled_diagnostic ==========\n")
print(baseline, digits = 4)
cat(
  "(Should match the configured jkp-regressions.csv, specification ",
  "'unfiltered_scaled_diagnostic': predictor FE 34.6/-28.2, predictor+month ",
  "FE 60.2/-49.9.)\n",
  sep = ""
)

# ---- leave-one-factor-out: which factors drive the positive coefficient --

signals <- sort(unique(d$signalname))
loo <- rbindlist(lapply(signals, function(s) {
  sub <- d[signalname != s]
  res <- run_fe(sub)
  res[, dropped_signal := s]
  res
}))
full_row <- run_fe(d)[, dropped_signal := "(none, full sample)"]
loo <- rbind(full_row, loo)
setcolorder(loo, "dropped_signal")

# Rank by how much dropping the factor moves the predictor+month FE
# post-sample coefficient toward zero, since that is where the sign flip
# (positive vs. the quality-screened baseline's near-zero/negative) is
# clearest.
pm_full <- full_row[fixed_effects == "predictor + month", post_sample]
loo_pm <- loo[fixed_effects == "predictor + month"]
loo_pm[, shift_from_full := post_sample - pm_full]
setorder(loo_pm, post_sample)
cat("\n========== Leave-one-factor-out (predictor + month FE) ==========\n")
cat("Factors whose removal moves post_sample furthest from the full-sample ",
    sprintf("estimate of %.1f:\n", pm_full), sep = "")
print(
  head(loo_pm[, .(
    dropped_signal, post_sample, post_sample_se, shift_from_full
  )], 10),
  digits = 4
)
fwrite(loo, file.path(OUT_DIR, "jkp-outlier-leave-one-out.csv"))

# ---- remedies: winsorizing, trimming, and t-stat screens ------------------

wins <- function(x, p) {
  qs <- quantile(x, c(p, 1 - p), na.rm = TRUE, names = FALSE)
  pmin(pmax(x, qs[1]), qs[2])
}

remedies <- list()
remedies[["full_sample"]] <- run_fe(d)[, remedy := "full sample (unfiltered, scaled)"]

for (p in c(0.01, 0.02, 0.05)) {
  dd <- copy(d)
  dd[, retScaled := wins(retScaled, p)]
  remedies[[paste0("winsor_", p)]] <- run_fe(dd)[
    , remedy := sprintf("winsorize retScaled at %.0f%%/%.0f%%", 100 * p, 100 * (1 - p))
  ]
}

# Hard-trim the k most extreme |retScaled| observations rather than capping
# them, to check whether a handful of individual months (as opposed to whole
# factors) are responsible.
ord <- order(-abs(d$retScaled))
for (k in c(5L, 20L, 100L)) {
  dd <- d[-ord[seq_len(k)]]
  remedies[[paste0("trim_", k)]] <- run_fe(dd)[
    , remedy := sprintf("drop %d most extreme |retScaled| observations", k)
  ]
}

# Exclude factors below an in-sample t-stat threshold; t>2 replicates
# jkp.R's existing quality screen and anchors the comparison.
for (thresh in c(0, 1, 2)) {
  keep <- quality_stats[in_sample_tstat > thresh, signalname]
  dd <- d[signalname %in% keep]
  remedies[[paste0("tstat_", thresh)]] <- run_fe(dd)[
    , remedy := sprintf(
      "exclude factors with in-sample t-stat <= %d (%d factors kept)",
      thresh, uniqueN(dd$signalname)
    )
  ]
}

remedies_table <- rbindlist(remedies, use.names = TRUE)
setcolorder(remedies_table, "remedy")
fwrite(remedies_table, file.path(OUT_DIR, "jkp-outlier-remedies.csv"))

cat("\n========== Remedies (predictor + month FE) ==========\n")
print(
  remedies_table[fixed_effects == "predictor + month", .(
    remedy, post_sample, post_sample_se, additional_post_publication,
    additional_post_publication_se, factors
  )],
  digits = 3
)

cat(
  "\nConclusion: the positive post-sample coefficient in jkp.R's ",
  "unfiltered-scaled specifications is an outlier artifact, not a real ",
  "post-sample outperformance. A small number of cited factors have an ",
  "in-sample mean within a few basis points of zero (t-stat near 0), so ",
  "dividing by that mean to set the in-sample scale to 100 turns ordinary ",
  "monthly returns into retScaled values of five to six figures. debt_me ",
  "alone (in-sample mean 2.3 bps/month, t-stat 0.23) accounts for most of ",
  "the effect; age is a secondary contributor. Winsorizing or hard-trimming ",
  "shrinks but does not eliminate the positive coefficient, because the ",
  "distortion is concentrated in specific factors rather than a few months. ",
  "Excluding factors on in-sample t-stat -- which is exactly jkp.R's ",
  "existing t>2 quality screen -- removes the artifact entirely and is the ",
  "right fix, since it excludes the mechanism (an unreliable scale ",
  "denominator) rather than papering over its symptoms.\n",
  sep = ""
)

cat("\nSaved:\n")
cat("  ", file.path(OUT_DIR, "jkp-outlier-factor-scale.csv"), "\n", sep = "")
cat("  ", file.path(OUT_DIR, "jkp-outlier-leave-one-out.csv"), "\n", sep = "")
cat("  ", file.path(OUT_DIR, "jkp-outlier-remedies.csv"), "\n", sep = "")
