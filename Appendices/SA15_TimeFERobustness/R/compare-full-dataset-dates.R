# Compare date metadata for quality-screened JKP and Chen-Zimmermann signals.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R build
#
# Inputs:
#   ../Data/Raw/TimeFERobustness/jkp/[usa]_[all_factors]_[monthly]_[ew].zip
#   ../Data/Raw/TimeFERobustness/jkp/factor_details.xlsx
#   ../Data/Raw/TimeFERobustness/opensourceap/op-2025_10.csv
#   ../Data/Raw/TimeFERobustness/opensourceap/SignalDoc-2025_10.csv
#
# Output:
#   ../Data/Processed/TimeFERobustness/output/jkp-cz-full-date-comparison.csv

suppressPackageStartupMessages({
  library(data.table)
  library(readxl)
})
pdf(NULL)
source("Appendices/SA15_TimeFERobustness/R/config.R")

JKP_DETAILS <- file.path(TIMEFE_JKP_RAW_DIR, "factor_details.xlsx")
JKP_RETURNS <- file.path(
  TIMEFE_JKP_RAW_DIR, "[usa]_[all_factors]_[monthly]_[ew].zip"
)
CZ_DETAILS <- TIMEFE_SIGNAL_DOC_CSV
CZ_RETURNS <- TIMEFE_OP_CSV
OUT_CSV <- file.path(TIMEFE_OUTPUT_DIR, "jkp-cz-full-date-comparison.csv")

timefe_source("helpers.R")

jkp <- as.data.table(read_excel(JKP_DETAILS, sheet = "details"))
jkp <- jkp[!is.na(abr_jkp) & nzchar(abr_jkp)]
setnames(jkp, "abr_jkp", "signalname")
jkp[, `:=`(
  sample_start = parse_period(`in-sample period`, "start"),
  sample_end = parse_period(`in-sample period`, "end"),
  publication = parse_first_year(cite)
)]

cz <- fread(CZ_DETAILS)[
  `Cat.Signal` == "Predictor",
  .(
    signalname = Acronym,
    sample_start = as.integer(SampleStartYear),
    sample_end = as.integer(SampleEndYear),
    publication = as.integer(Year)
  )
]

stopifnot(
  all(complete.cases(jkp[, .(sample_start, sample_end)])),
  all(complete.cases(cz))
)

screen_signals <- function(metadata, returns, return_column) {
  d <- merge(
    returns[, .(
      signalname,
      date = as.IDate(date),
      return_value = as.numeric(get(return_column))
    )],
    metadata[, .(signalname, sample_start, sample_end)],
    by = "signalname"
  )
  d[, year := year(date)]
  quality <- d[
    year >= sample_start & year <= sample_end,
    .(
      in_sample_tstat =
        mean(return_value) / (sd(return_value) / sqrt(.N))
    ),
    by = signalname
  ]
  quality[
    is.finite(in_sample_tstat) & in_sample_tstat > 2,
    signalname
  ]
}

jkp_returns <- read_zip_csv(JKP_RETURNS)
setnames(jkp_returns, "name", "signalname")
cz_returns <- fread(CZ_RETURNS)[port == "LS"]

jkp_keep <- screen_signals(jkp, jkp_returns, "ret")
cz_keep <- screen_signals(cz, cz_returns, "ret")
jkp <- jkp[signalname %in% jkp_keep]
cz <- cz[signalname %in% cz_keep]

summarize_field <- function(data, dataset, field) {
  value <- data[[field]]
  value <- value[!is.na(value)]
  data.table(
    dataset,
    field,
    observations = length(value),
    mean = mean(value),
    sd = sd(value),
    p10 = unname(quantile(value, 0.10, type = 1L)),
    median = median(value),
    p90 = unname(quantile(value, 0.90, type = 1L)),
    min = min(value),
    max = max(value)
  )
}

jkp[, between_length := publication - sample_end]
cz[, between_length := publication - sample_end]

fields <- c(
  "sample_start", "sample_end", "publication", "between_length"
)
comparison <- rbindlist(list(
  rbindlist(lapply(fields, function(field) {
    summarize_field(jkp, "JKP", field)
  })),
  rbindlist(lapply(fields, function(field) {
    summarize_field(cz, "CZ", field)
  }))
))
comparison[, `:=`(
  total_factors = ifelse(dataset == "JKP", nrow(jkp), nrow(cz)),
  published_factors = ifelse(
    dataset == "JKP",
    sum(!is.na(jkp$publication)),
    sum(!is.na(cz$publication))
  )
)]

fwrite(comparison, OUT_CSV)
print(comparison, digits = 4)
message("Saved ", OUT_CSV)
