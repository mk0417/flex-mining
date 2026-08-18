# Check the pinned Jeff-response results and generated exhibits.
#
# Run from the repository root through the single entry point:
#   Rscript Appendices/SA15_TimeFERobustness/run.R check
#
# Inputs: configured regression CSVs and generated TeX exhibits.
# Outputs: none; exits with an error when a pinned expectation changes.

pdf(NULL)
suppressPackageStartupMessages(library(data.table))
source("Appendices/SA15_TimeFERobustness/R/config.R")
timefe_source("helpers.R")

# These are historical expectations for the current input vintage, not rules
# used to construct the samples. Change them deliberately when inputs change.
EXPECTED <- c(
  metadata_rows = 255L,
  jkp_return_series = 153L,
  cited_jkp_factors = 142L,
  jkp_quality_factors = 97L,
  cz_signal_quality_factors = 138L,
  metadata_matched_cz_signals = 69L
)

jkp_diagnostics <- fread(file.path(TIMEFE_OUTPUT_DIR, "jkp-diagnostics.csv"))
cz_diagnostics <- fread(
  file.path(TIMEFE_OUTPUT_DIR, "cz-signal-based-diagnostics.csv")
)

diagnostic_value <- function(data, item) {
  wanted_item <- item
  value <- data[item == wanted_item, value]
  check(length(value) == 1L, "Missing or duplicate diagnostic: %s", item)
  as.integer(value)
}

actual <- c(
  metadata_rows = diagnostic_value(jkp_diagnostics, "metadata rows"),
  jkp_return_series = diagnostic_value(
    jkp_diagnostics, "unique equal-weighted return series"
  ),
  cited_jkp_factors = diagnostic_value(
    jkp_diagnostics, "cited metadata factors"
  ),
  jkp_quality_factors = diagnostic_value(
    jkp_diagnostics, "baseline quality factors retained (t>2)"
  ),
  cz_signal_quality_factors = diagnostic_value(
    cz_diagnostics, "quality-screened factors retained (t>2)"
  ),
  metadata_matched_cz_signals = diagnostic_value(
    cz_diagnostics, "metadata-matched EW factors retained (t>2)"
  )
)

check(
  identical(actual, EXPECTED),
  "Pinned result counts changed:\n%s",
  paste(sprintf("  %s: expected %d, found %d",
                names(EXPECTED), EXPECTED, actual), collapse = "\n")
)

jkp_results <- fread(file.path(TIMEFE_OUTPUT_DIR, "jkp-regressions.csv"))
cz_signal_results <- fread(
  file.path(TIMEFE_OUTPUT_DIR, "cz-signal-based-regressions.csv")
)
check(
  jkp_results[, .N, by = .(specification, fixed_effects)][N > 1L, .N] == 0L,
  "JKP results contain duplicate specification/fixed-effect rows."
)
check(
  !any(startsWith(jkp_results$specification, "matched_")),
  "Deprecated matched-sample specifications remain in JKP results."
)
check(
  "normalization_mean_bps" %in% names(jkp_results) &&
    jkp_results[
      specification == "baseline_quality_t2_grand_mean_scaled",
      all(is.finite(normalization_mean_bps))
    ],
  "JKP grand-mean results lack numeric normalization metadata."
)
check(
  all(c("weighting_id", "mean_in_sample_bps") %in%
        names(cz_signal_results)),
  "CZ signal results lack weighting or in-sample-mean metadata."
)
check(
  cz_signal_results[
    , .N, by = .(weighting_id, specification, fixed_effects)
  ][N > 1L, .N] == 0L,
  "CZ signal results contain duplicate weighting/specification/FE rows."
)
check(
  setequal(unique(cz_signal_results$weighting_id), c("ew", "vw_cap", "vw")),
  "CZ signal results do not contain EW, capped-VW, and VW constructions."
)
replication_specs <- data.table(
  weighting_id = c("ew", "ew", "vw_cap", "vw"),
  specification = c(
    "baseline_quality_t2",
    "baseline_quality_t2_grand_mean_scaled",
    "baseline_quality_t2",
    "baseline_quality_t2"
  )
)
replication_rows <- merge(
  cz_signal_results,
  replication_specs,
  by = c("weighting_id", "specification")
)
check(
  nrow(replication_rows) == 8L &&
    setequal(
      unique(replication_rows$fixed_effects),
      c("predictor", "predictor + month")
    ) &&
    all(is.finite(replication_rows$mean_in_sample_bps)) &&
    all(is.finite(replication_rows$min_in_sample_mean_bps)),
  "The four-panel JKP replication lacks complete CZ target rows."
)

matched_pairs <- fread(file.path(
  TIMEFE_OUTPUT_DIR, "jkp-cz-matched-pairs.csv"
))
matched_signal_rows <- cz_signal_results[
  weighting_id == "ew" &
    specification == "baseline_quality_t2_metadata_matched"
]
check(
  nrow(matched_pairs) == 74L &&
    uniqueN(matched_pairs$signalname) == 74L &&
    uniqueN(matched_pairs$cz_signalname) == 74L,
  "The metadata-matched JKP-CZ crosswalk must contain 74 one-to-one pairs."
)
check(
  nrow(matched_signal_rows) == 2L &&
    setequal(
      matched_signal_rows$fixed_effects,
      c("predictor", "predictor + month")
    ) &&
    all(is.finite(matched_signal_rows$mean_in_sample_bps)) &&
    all(is.finite(matched_signal_rows$min_in_sample_mean_bps)),
  "The metadata-matched CZ signal-level rows are missing or incomplete."
)

exhibits <- file.path(
  TIMEFE_EXHIBIT_DIR,
  c(
    "mp-cz-normalized.tex", "jkp-cz-normalized.tex",
    "cz-alternative-specs.tex", "jkp-rep-using-cz.tex",
    "s6-timefe-summary.tex", "jkp-cz-date-comparison.tex"
  )
)
check(all(file.exists(exhibits)), "One or more configured exhibits are missing.")
check(all(file.info(exhibits)$size > 0L), "One or more exhibits are empty.")
jkp_exhibit <- readLines(
  file.path(TIMEFE_EXHIBIT_DIR, "jkp-cz-normalized.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-D]:", jkp_exhibit)) == 4L &&
    !any(grepl("Panel [EF]:", jkp_exhibit)),
  "The JKP exhibit must contain exactly Panels A--D."
)
jkp_rep_exhibit <- readLines(
  file.path(TIMEFE_EXHIBIT_DIR, "jkp-rep-using-cz.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-E]:", jkp_rep_exhibit)) == 5L &&
    !any(grepl("Panel [F-G]:", jkp_rep_exhibit)) &&
    !any(startsWith(jkp_rep_exhibit, "JKP,")),
  "The JKP replication exhibit must contain exactly five CZ-only panels."
)
s6_exhibit <- readLines(
  file.path(TIMEFE_EXHIBIT_DIR, "s6-timefe-summary.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-F]:", s6_exhibit)) == 6L &&
    !any(grepl("Panel [G-Z]:", s6_exhibit)) &&
    sum(startsWith(s6_exhibit, "Predictor FE &")) == 6L &&
    sum(startsWith(s6_exhibit, "Predictor + time FE &")) == 6L,
  "The S6 summary must contain exactly six two-row panels."
)

message("Pinned counts and generated exhibits check out.")
