# Compare post-sample decay across the JKP within-leg weighting schemes that
# JKP publishes in bulk for all factors at once: equal weighted (baseline),
# capped value weighted, and (uncapped) value weighted.
#
# Unlike the vw_cap comparison inside jkp.R, this script recomputes the
# t>2 quality screen separately for each weighting from that weighting's OWN
# in-sample returns, matching the treatment of each alternative CZ portfolio
# construction in cz-alternative-specs.R.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R build
#
# Inputs:
#   ../Data/Raw/TimeFERobustness/jkp/[usa]_[all_factors]_[monthly]_[ew].zip
#   ../Data/Raw/TimeFERobustness/jkp/[usa]_[all_factors]_[monthly]_[vw_cap].zip
#   ../Data/Raw/TimeFERobustness/jkp/[usa]_[all_factors]_[monthly]_[vw].zip
#   ../Data/Raw/TimeFERobustness/jkp/factor_details.xlsx
#
# Output:
#   ../Data/Processed/TimeFERobustness/output/jkp-alternative-specs.csv

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(readxl)
})

pdf(NULL)
Sys.setenv(TZ = "America/New_York")
source("Appendices/SA15_TimeFERobustness/R/config.R")

RAW_DIR <- TIMEFE_JKP_RAW_DIR
OUT_DIR <- TIMEFE_OUTPUT_DIR
OUT_CSV <- file.path(OUT_DIR, "jkp-alternative-specs.csv")
DETAILS_XLSX <- file.path(RAW_DIR, "factor_details.xlsx")

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

timefe_source("helpers.R")

specs <- data.table(
  weighting = c("ew", "vw_cap", "vw"),
  label = c(
    "Equal weighted (baseline)", "Capped value weighted",
    "Value weighted"
  )
)

zip_path <- function(weighting) {
  file.path(
    RAW_DIR,
    sprintf("[usa]_[all_factors]_[monthly]_[%s].zip", weighting)
  )
}
zip_url <- function(weighting) {
  paste0(
    "https://jkpfactors-data.s3.amazonaws.com/public/",
    sprintf("%%5Busa%%5D_%%5Ball_factors%%5D_%%5Bmonthly%%5D_%%5B%s%%5D.zip",
            weighting)
  )
}

download_one <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) return(invisible(NULL))
  message("Downloading ", basename(dest), " ...")
  tmp <- tempfile(pattern = paste0(basename(dest), "-"), tmpdir = RAW_DIR)
  on.exit(unlink(tmp), add = TRUE)
  status <- tryCatch(
    download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(status, "error") || !file.exists(tmp) ||
      file.info(tmp)$size == 0) {
    fail(
      paste0(
        "Could not download %s from %s. The likely cause is network/",
        "firewall access: %s"
      ),
      basename(dest), url,
      if (inherits(status, "error")) conditionMessage(status) else "empty file"
    )
  }
  check(file.rename(tmp, dest), "Could not move downloaded input to %s", dest)
  invisible(NULL)
}

meta_all <- as.data.table(read_excel(DETAILS_XLSX, sheet = "details"))
meta <- meta_all[!is.na(abr_jkp) & nzchar(abr_jkp)]
setnames(meta, "abr_jkp", "signalname")
meta[, `:=`(
  SampleStartYear = parse_period(`in-sample period`, "start"),
  SampleEndYear = parse_period(`in-sample period`, "end"),
  pubYear = parse_first_year(cite)
)]
meta <- meta[, .(signalname, SampleStartYear, SampleEndYear, pubYear)]

read_returns <- function(path, expected_weighting) {
  x <- read_zip_csv(path)
  required <- c(
    "location", "name", "freq", "weighting", "direction", "date", "ret"
  )
  check(all(required %in% names(x)),
        "%s is missing required columns: %s", path,
        paste(setdiff(required, names(x)), collapse = ", "))
  check(all(x$location == "usa") && all(x$freq == "monthly") &&
          all(x$weighting == expected_weighting),
        "%s is not the requested US monthly %s file.",
        path, expected_weighting)
  setnames(x, "name", "signalname")
  x[, date := as.IDate(date)]
  check(x[, .N, by = .(signalname, date)][N > 1L, .N] == 0L,
        "Duplicate signalname-month observations found in %s.", path)
  check(all(is.finite(x$ret)), "Non-finite returns found in %s.", path)
  x[, .(signalname, date, retPct = 100 * ret)]
}

estimate_spec <- function(weighting, label) {
  path <- zip_path(weighting)
  download_one(zip_url(weighting), path)
  d <- read_returns(path, weighting)
  d <- merge(d, meta, by = "signalname")
  d[, yr := year(date)]
  d <- d[yr >= SampleStartYear & !is.na(pubYear)]
  d[, `:=`(
    postSampC = as.integer(yr > SampleEndYear),
    postPubC = as.integer(yr > pubYear),
    yyyymm = yr * 100L + month(date)
  )]

  quality <- d[postSampC == 0L, .(
    in_sample_mean_pct = mean(retPct),
    in_sample_t = mean(retPct) / (sd(retPct) / sqrt(.N)),
    in_sample_months = .N
  ), by = signalname]
  keep <- quality[
    is.finite(in_sample_mean_pct) & is.finite(in_sample_t) &
      in_sample_t > 2,
    signalname
  ]
  check(length(keep) > 0L, "No factors pass the quality screen for %s.", weighting)
  d <- merge(d[signalname %in% keep], quality, by = "signalname")
  d[, retScaled := 100 * retPct / in_sample_mean_pct]

  rbindlist(lapply(c(FALSE, TRUE), function(month_fe) {
    fe <- if (month_fe) "signalname + yyyymm" else "signalname"
    fit <- feols(
      as.formula(paste("retScaled ~ postSampC + postPubC |", fe)),
      data = d,
      cluster = ~ signalname + yyyymm,
      fixef.rm = "singleton",
      notes = FALSE
    )
    b <- coef(fit)
    v <- vcov(fit)
    data.table(
      data_name = weighting, label,
      fixed_effects = if (month_fe) "predictor + month" else "predictor",
      post_sample = unname(b["postSampC"]),
      post_sample_se = sqrt(v["postSampC", "postSampC"]),
      additional_post_publication = unname(b["postPubC"]),
      additional_post_publication_se = sqrt(v["postPubC", "postPubC"]),
      mean_in_sample_bps = 100 * mean(quality[signalname %in% keep, in_sample_mean_pct]),
      min_in_sample_mean_bps = 100 * min(
        quality[signalname %in% keep, in_sample_mean_pct]
      ),
      observations = fit$nobs,
      factors = uniqueN(d$signalname),
      singleton_observations_removed = nrow(d) - fit$nobs
    )
  }))
}

results <- rbindlist(Map(estimate_spec, specs$weighting, specs$label))
results[, spec_order := match(data_name, specs$weighting)]
setorder(results, spec_order, fixed_effects)
results[, spec_order := NULL]
fwrite(results, OUT_CSV)

print(results[, .(
  label, fixed_effects, post_sample, post_sample_se,
  additional_post_publication, additional_post_publication_se,
  mean_in_sample_bps, min_in_sample_mean_bps, factors
)], digits = 4)
message("Saved ", OUT_CSV)
