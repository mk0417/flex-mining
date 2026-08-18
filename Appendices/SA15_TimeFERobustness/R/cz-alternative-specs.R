# Compare post-sample decay across alternative Chen-Zimmermann portfolios
# downloaded with OpenSourceAP.DownloadR.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R build

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(OpenSourceAP.DownloadR)
})

pdf(NULL)
Sys.setenv(TZ = "America/New_York")
source("Appendices/SA15_TimeFERobustness/R/config.R")

RELEASE <- TIMEFE_RELEASE
RAW_DIR <- TIMEFE_OPENAP_RAW_DIR
OUT_DIR <- TIMEFE_OUTPUT_DIR
OUT_CSV <- file.path(OUT_DIR, "cz-alternative-specs.csv")
DOC_CSV <- TIMEFE_SIGNAL_DOC_CSV

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

specs <- data.table(
  data_name = c(
    "op", "deciles_ew", "deciles_vw", "quintiles_ew", "quintiles_vw",
    "ex_nyse_p20_me", "nyse", "ex_price5"
  ),
  label = c(
    "Original portfolios", "Deciles, equal weighted",
    "Deciles, value weighted", "Quintiles, equal weighted",
    "Quintiles, value weighted", "Exclude below NYSE 20th pct. size",
    "NYSE stocks only", "Exclude price below $5"
  )
)

httr::set_config(httr::config(http_version = 2))
openap <- OpenAP$new(release_year = RELEASE)

download_spec <- function(data_name) {
  path <- file.path(RAW_DIR, paste0(data_name, "-", RELEASE, ".csv"))
  if (!file.exists(path)) {
    message("Downloading ", data_name, " with OpenSourceAP.DownloadR ...")
    fwrite(as.data.table(openap$dl_port(data_name)), path)
  }
  fread(path)
}

doc <- fread(DOC_CSV)[
  `Cat.Signal` == "Predictor",
  .(
    signalname = Acronym,
    SampleStartYear = as.integer(SampleStartYear),
    SampleEndYear = as.integer(SampleEndYear),
    pubYear = as.integer(Year)
  )
]

estimate_spec <- function(data_name, label) {
  ports <- download_spec(data_name)
  stopifnot(all(c("signalname", "port", "date", "ret") %in% names(ports)))
  if (!"LS" %in% ports$port) {
    stop(data_name, " does not contain a precomputed LS portfolio.")
  }

  d <- ports[port == "LS", .(
    signalname, date = as.IDate(date), retPct = as.numeric(ret)
  )]
  d <- merge(d, doc, by = "signalname")
  d[, yr := year(date)]
  d <- d[
    yr >= SampleStartYear & !is.na(pubYear) &
      is.finite(retPct)
  ]
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
  d <- merge(d[signalname %in% keep], quality, by = "signalname")
  d[, retScaled := 100 * retPct / in_sample_mean_pct]

  rbindlist(lapply(c(FALSE, TRUE), function(month_fe) {
    fe <- if (month_fe) "signalname + yyyymm" else "signalname"
    fit <- feols(
      as.formula(paste(
        "retScaled ~ postSampC + postPubC |", fe
      )),
      data = d,
      cluster = ~ signalname + yyyymm,
      fixef.rm = "singleton",
      notes = FALSE
    )
    b <- coef(fit)
    v <- vcov(fit)
    data.table(
      data_name, label,
      fixed_effects = if (month_fe) {
        "predictor + month"
      } else {
        "predictor"
      },
      post_sample = unname(b["postSampC"]),
      post_sample_se = sqrt(v["postSampC", "postSampC"]),
      additional_post_publication = unname(b["postPubC"]),
      additional_post_publication_se =
        sqrt(v["postPubC", "postPubC"]),
      mean_in_sample_bps = 100 * mean(quality[
        signalname %in% keep, in_sample_mean_pct
      ]),
      observations = fit$nobs,
      factors = uniqueN(d$signalname),
      singleton_observations_removed = nrow(d) - fit$nobs
    )
  }))
}

results <- rbindlist(Map(
  estimate_spec, specs$data_name, specs$label
))
results[, spec_order := match(data_name, specs$data_name)]
setorder(results, spec_order, fixed_effects)
results[, spec_order := NULL]
fwrite(results, OUT_CSV)

print(results[, .(
  label, fixed_effects, post_sample, post_sample_se,
  additional_post_publication, additional_post_publication_se,
  mean_in_sample_bps, factors
)], digits = 4)
message("Saved ", OUT_CSV)
