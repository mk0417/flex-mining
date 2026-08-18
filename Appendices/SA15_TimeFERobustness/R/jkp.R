# McLean-Pontiff-style decay regressions using JKP (2023) factor returns.
#
# Baseline: US, monthly, equal-weighted long-short factors with an in-sample
# return t-statistic greater than 2. Returns are then scaled factor by factor
# so that the in-sample mean is 100.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R build
#
# Outputs:
#   ../Data/Processed/TimeFERobustness/output/jkp-regressions.csv
#   ../Data/Processed/TimeFERobustness/output/jkp-diagnostics.csv
#   ../Data/Processed/TimeFERobustness/output/jkp-cz-crosswalk.csv
#   ../Data/Processed/TimeFERobustness/output/jkp-cz-matched-pairs.csv
#   ../Data/Raw/TimeFERobustness/jkp/download-manifest.csv

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
  library(readxl)
})

pdf(NULL)
Sys.setenv(TZ = "America/New_York")
source("Appendices/SA15_TimeFERobustness/R/config.R")

# ---- configuration --------------------------------------------------------

RAW_DIR <- TIMEFE_JKP_RAW_DIR
OUT_DIR <- TIMEFE_OUTPUT_DIR

EW_ZIP <- file.path(RAW_DIR, "[usa]_[all_factors]_[monthly]_[ew].zip")
DETAILS_XLSX <- file.path(RAW_DIR, "factor_details.xlsx")

CZ_RET <- TIMEFE_OP_CSV
CZ_DOC <- TIMEFE_SIGNAL_DOC_CSV
CZ_MAP <- TIMEFE_META_REPLICATIONS_CSV

JKP_COMMIT <- TIMEFE_JKP_COMMIT
INPUTS <- data.table(
  file = c(EW_ZIP, DETAILS_XLSX),
  url = c(
    paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/",
      "%5Busa%5D_%5Ball_factors%5D_%5Bmonthly%5D_%5Bew%5D.zip"
    ),
    paste0(
      "https://raw.githubusercontent.com/bkelly-lab/jkp-data/",
      JKP_COMMIT, "/src/jkp/data/resources/factor_details.xlsx"
    )
  ),
  role = c(
    "baseline equal-weighted factor returns",
    "factor metadata"
  )
)

AUDIT_SIGNALS <- c("age", "be_me", "ret_12_1")
portfolio_audit_files <- file.path(
  RAW_DIR,
  paste0("[usa]_[", AUDIT_SIGNALS, "]_[monthly]_[ew]_[portfolios].zip")
)
INPUTS <- rbind(
  INPUTS,
  data.table(
    file = portfolio_audit_files,
    url = paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/portfolios/",
      "%5Busa%5D_%5B", AUDIT_SIGNALS,
      "%5D_%5Bmonthly%5D_%5Bew%5D.zip"
    ),
    role = paste("underlying-portfolio sign audit:", AUDIT_SIGNALS)
  )
)

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# Remove obsolete outputs from the older two-sided matched comparison.
obsolete_matched_outputs <- file.path(
  OUT_DIR,
  c(
    "jkp-cz-matched-performance.csv", "jkp-cz-matched-69.csv",
    "jkp-cz-date-comparison.csv"
  )
)
unlink(obsolete_matched_outputs)

timefe_source("helpers.R")

# ---- acquire and record pinned inputs -------------------------------------

download_one <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) return(FALSE)
  message("Downloading ", basename(dest), " ...")
  tmp <- tempfile(pattern = paste0(basename(dest), "-"), tmpdir = RAW_DIR)
  # A failed download must not leave a partial file behind in RAW_DIR.
  on.exit(unlink(tmp), add = TRUE)
  status <- tryCatch(
    download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  if (inherits(status, "error") || !file.exists(tmp) ||
      file.info(tmp)$size == 0) {
    fail(
      paste0(
        "Could not download %s from %s. Do not substitute another weighting. ",
        "The likely cause is network/firewall access: %s"
      ),
      basename(dest), url,
      if (inherits(status, "error")) conditionMessage(status) else "empty file"
    )
  }
  check(file.rename(tmp, dest), "Could not move downloaded input to %s", dest)
  TRUE
}

# Record which inputs this run actually fetched. Everything else was already on
# disk, so its timestamp is a local file mtime and not a download date.
fetched_this_run <- vapply(
  seq_len(nrow(INPUTS)),
  function(i) download_one(INPUTS$url[i], INPUTS$file[i]),
  logical(1)
)
run_time_ny <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z", tz = "America/New_York")

check(requireNamespace("digest", quietly = TRUE),
      "The already-declared dependency 'digest' is unavailable.")
manifest <- copy(INPUTS)
manifest[, `:=`(
  fetched_this_run = fetched_this_run,
  # Only meaningful as a download date when fetched_this_run is TRUE; otherwise
  # this is the mtime of a file that was already present.
  file_mtime_ny = format(
    file.info(file)$mtime, "%Y-%m-%d %H:%M:%S %Z",
    tz = "America/New_York"
  ),
  downloaded_ny = fifelse(fetched_this_run, run_time_ny, NA_character_),
  bytes = as.numeric(file.info(file)$size),
  sha256 = vapply(
    file, digest::digest, character(1), algo = "sha256", file = TRUE
  )
)]
setcolorder(
  manifest,
  c(
    "file", "role", "url", "fetched_this_run", "downloaded_ny",
    "file_mtime_ny", "bytes", "sha256"
  )
)
fwrite(manifest, file.path(RAW_DIR, "download-manifest.csv"))

# ---- input readers and metadata audit ------------------------------------

meta_all <- as.data.table(read_excel(DETAILS_XLSX, sheet = "details"))
meta <- meta_all[!is.na(abr_jkp) & nzchar(abr_jkp)]
setnames(meta, "abr_jkp", "signalname")
meta[, `:=`(
  SampleStartYear = parse_period(`in-sample period`, "start"),
  SampleEndYear = parse_period(`in-sample period`, "end"),
  pubYear = parse_first_year(cite),
  direction_meta = as.integer(direction)
)]

check(all(!is.na(meta$SampleStartYear) & !is.na(meta$SampleEndYear)),
      "At least one JKP in-sample period did not parse.")

date_exceptions <- meta[
  !is.na(pubYear) & SampleEndYear > pubYear,
  .(signalname, SampleEndYear, pubYear)
]

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
  # Decimal monthly returns have typical absolute values near 0.01--0.03.
  med_abs <- median(abs(x$ret))
  check(med_abs > 0.001 && med_abs < 0.10 && max(abs(x$ret)) < 2,
        paste0(
          "Return-unit audit failed for %s (median |ret| = %.4f). ",
          "Expected decimal monthly returns."
        ),
        path, med_abs)
  x[]
}

ew <- read_returns(EW_ZIP, "ew")

audit_join <- merge(
  ew[, .(signalname, date, direction_file = as.integer(direction))],
  meta[, .(signalname, direction_meta)],
  by = "signalname", all.x = TRUE
)
check(!anyNA(audit_join$direction_meta),
      "At least one return series failed to join to factor metadata.")
check(all(audit_join$direction_file == audit_join$direction_meta),
      paste0(
        "Return-file and metadata directions disagree. Published factor ",
        "returns are already signed and must not be signed a second time."
      ))

# Reconstruct three selected long-short series from the official underlying
# high and low portfolio files. This independently checks both the sign and
# the factor-return arithmetic.
portfolio_audits <- rbindlist(lapply(seq_along(AUDIT_SIGNALS), function(i) {
  signal <- AUDIT_SIGNALS[i]
  p <- read_zip_csv(portfolio_audit_files[i])
  required <- c("location", "name", "freq", "weighting", "pf", "date", "ret")
  check(all(required %in% names(p)),
        "Underlying portfolio archive for %s has an unexpected schema.", signal)
  # Hold the archive to the same scope the return files are held to, so a file
  # carrying several weightings cannot be silently averaged or deduplicated.
  check(all(p$location == "usa") && all(p$freq == "monthly") &&
          all(p$weighting == "ew"),
        "Underlying portfolio archive for %s is not US monthly equal weighted.",
        signal)
  p <- p[name == signal & pf %in% c(1, 3)]
  p[, date := as.IDate(date)]
  check(p[, .N, by = .(date, pf)][N > 1L, .N] == 0L,
        "Duplicate date-portfolio rows in the underlying archive for %s.",
        signal)
  p <- dcast(p, date ~ pf, value.var = "ret")
  check(all(c("1", "3") %in% names(p)),
        "Underlying portfolio archive for %s lacks a high or low leg.", signal)
  direction_i <- meta[signalname == signal, direction_meta]
  p[, reconstructed := direction_i * (`3` - `1`)]
  published <- ew[signalname == signal, .(date, published = ret)]
  z <- merge(p, published, by = "date")
  z <- z[is.finite(reconstructed) & is.finite(published)]
  # A handful of overlapping months would make the reconstruction check
  # vacuous, so require that the audit actually covers the series.
  check(nrow(z) >= 0.9 * nrow(published),
        paste0(
          "Underlying portfolio audit for %s covers only %d of %d published ",
          "months."
        ),
        signal, nrow(z), nrow(published))
  max_error <- max(abs(z$reconstructed - z$published))
  check(max_error < 1e-12,
        "Underlying portfolio reconstruction failed for %s (max error %.3g).",
        signal, max_error)
  data.table(
    signalname = signal, overlapping_months = nrow(z),
    max_absolute_error = max_error
  )
}))

# ---- common preparation and estimation -----------------------------------

build_panel <- function(
    returns, cited = TRUE, end_year = NULL, pub_month = NULL,
    scaled = TRUE, post_publication = TRUE, keep_signals = NULL) {
  d <- merge(
    returns[, .(signalname, date, retDecimal = ret)],
    meta[, .(
      signalname, SampleStartYear, SampleEndYear, pubYear, cite, name,
      abr_hxz
    )],
    by = "signalname"
  )
  d[, yr := year(date)]
  d <- d[yr >= SampleStartYear]
  d <- d[if (cited) !is.na(pubYear) else is.na(pubYear)]
  if (!is.null(end_year)) d <- d[yr <= end_year]
  if (!is.null(keep_signals)) d <- d[signalname %in% keep_signals]

  d[, postSampC := as.integer(yr > SampleEndYear)]
  if (post_publication) {
    check(cited, "post_publication=TRUE requires cited factors.")
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
  }
  d[, `:=`(
    yyyymm = yr * 100L + month(date),
    retPct = 100 * retDecimal
  )]

  means <- d[postSampC == 0L, .(
    in_sample_mean_decimal = mean(retDecimal),
    in_sample_obs = .N
  ), by = signalname]
  d <- merge(d, means, by = "signalname", all.x = TRUE)
  d[, scale_eligible := (
    !is.na(in_sample_mean_decimal) & in_sample_obs > 0L &
      in_sample_mean_decimal > 0
  )]
  if (scaled) {
    d <- d[scale_eligible == TRUE]
    d[, retScaled := 100 * retDecimal / in_sample_mean_decimal]
  }
  attr(d, "means") <- means
  d[]
}

estimate <- function(lhs, rhs, fixed_effects, data) {
  feols(
    as.formula(paste(lhs, "~", rhs, "|", fixed_effects)),
    data = data,
    cluster = ~ signalname + yyyymm,
    fixef.rm = "singleton",
    notes = FALSE
  )
}

model_row <- function(fit, specification, weighting, scale, time_fe, data) {
  b <- coef(fit)
  v <- vcov(fit)
  has_s <- "postSampC" %in% names(b)
  has_p <- "postPubC" %in% names(b)
  total <- if (has_s && has_p) b["postSampC"] + b["postPubC"] else NA_real_
  total_se <- if (has_s && has_p) {
    sqrt(
      v["postSampC", "postSampC"] + v["postPubC", "postPubC"] +
        2 * v["postSampC", "postPubC"]
    )
  } else NA_real_
  if ("in_sample_mean_decimal" %in% names(data)) {
    factor_means <- unique(
      data[, .(signalname, in_sample_mean_decimal)]
    )$in_sample_mean_decimal
    min_in_sample_mean_bps <- 1e4 * min(factor_means)
  } else if ("in_sample_mean_pct" %in% names(data)) {
    factor_means <- unique(
      data[, .(signalname, in_sample_mean_pct)]
    )$in_sample_mean_pct
    min_in_sample_mean_bps <- 100 * min(factor_means)
  } else {
    stop(
      sprintf("In-sample means are unavailable for '%s'.", specification),
      call. = FALSE
    )
  }
  data.table(
    specification, weighting, scale,
    fixed_effects = if (time_fe) "predictor + month" else "predictor",
    post_sample = if (has_s) unname(b["postSampC"]) else NA_real_,
    post_sample_se = if (has_s) sqrt(v["postSampC", "postSampC"]) else NA_real_,
    additional_post_publication =
      if (has_p) unname(b["postPubC"]) else NA_real_,
    additional_post_publication_se =
      if (has_p) sqrt(v["postPubC", "postPubC"]) else NA_real_,
    total_post_publication_change = total,
    total_post_publication_change_se = total_se,
    normalization_mean_bps = if (is.null(attr(data, "grand_mean_pct"))) {
      NA_real_
    } else {
      100 * attr(data, "grand_mean_pct")
    },
    min_in_sample_mean_bps,
    observations = fit$nobs,
    factors = uniqueN(data$signalname),
    singleton_observations_removed = nrow(data) - fit$nobs
  )
}

results <- list()
run_pair <- function(
    specification, data, lhs, weighting, scale,
    rhs = "postSampC + postPubC") {
  check(nrow(data) > 0L, "No observations for specification %s.", specification)
  for (time_fe in c(FALSE, TRUE)) {
    fe <- if (time_fe) "signalname + yyyymm" else "signalname"
    fit <- estimate(lhs, rhs, fe, data)
    key <- paste(specification, if (time_fe) "time_fe" else "predictor_fe",
                 sep = "__")
    results[[key]] <<- model_row(
      fit, specification, weighting, scale, time_fe, data
    )
  }
  invisible(NULL)
}

# Common-denominator normalization for comparisons that should preserve the
# relative return scale across factors. This differs from retScaled, which
# gives every factor an in-sample mean of 100. Here the denominator is the
# equal-weighted average of factor-specific in-sample mean returns.
grand_mean_scale <- function(data) {
  grand_mean_pct <- mean(
    data[postSampC == 0L, .(factor_mean_pct = mean(retPct)),
         by = signalname]$factor_mean_pct
  )
  check(is.finite(grand_mean_pct) && grand_mean_pct > 0,
        "The grand in-sample mean must be positive.")
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

# ---- specifications -------------------------------------------------------

main <- build_panel(ew, cited = TRUE, scaled = TRUE)

# The t-statistic is
# calculated consistently from each JKP return series over its stated
# in-sample months: mean(ret) / (sd(ret) / sqrt(n)). Retain a factor only when
# this t-stat is greater than 2.
quality_stats <- main[postSampC == 0L, .(
  in_sample_mean_bps = 1e4 * mean(retDecimal),
  in_sample_tstat = mean(retDecimal) / (sd(retDecimal) / sqrt(.N))
), by = signalname]
quality_keep <- quality_stats[in_sample_tstat > 2, signalname]
quality_screened <- main[signalname %in% quality_keep]
run_pair(
  "baseline_quality_t2", quality_screened, "retScaled", "ew",
  "in-sample mean = 100"
)
run_pair(
  "baseline_quality_t2_unscaled", quality_screened, "retPct", "ew",
  "percent/month"
)
quality_grand_scaled <- grand_mean_scale(quality_screened)
run_pair(
  "baseline_quality_t2_grand_mean_scaled", quality_grand_scaled,
  "retGrandScaled", "ew", grand_scale_label(quality_grand_scaled)
)

end_2013 <- build_panel(
  ew, cited = TRUE, end_year = 2013L, scaled = TRUE,
  keep_signals = quality_keep
)
run_pair("end_2013_scaled", end_2013, "retScaled", "ew",
         "in-sample mean = 100")

pub_months <- c(January = 1L, June = 6L, December = 12L)
for (cutoff_name in names(pub_months)) {
  pm <- pub_months[[cutoff_name]]
  d <- build_panel(
    ew, cited = TRUE, pub_month = pm, scaled = TRUE,
    keep_signals = quality_keep
  )
  run_pair(
    paste0("publication_cutoff_", tolower(cutoff_name)),
    d, "retScaled", "ew", "in-sample mean = 100"
  )
}

# CZ reference rows put the quality-screened all-predictor and MP-matched
# estimates in the same machine-readable table as JKP.
cz_doc <- fread(CZ_DOC, encoding = "UTF-8")
cz_ret <- fread(CZ_RET)[port == "LS", .(
  signalname, date = as.IDate(date), retPct = ret
)]
cz_predictor_meta <- cz_doc[`Cat.Signal` == "Predictor", .(
  signalname = Acronym, SampleStartYear, SampleEndYear,
  pubYear = as.integer(Year)
)]

build_cz_panel <- function(keep_signals = NULL, end_year = NULL) {
  d <- merge(cz_ret, cz_predictor_meta, by = "signalname")
  d[, yr := year(date)]
  d <- d[yr >= SampleStartYear]
  if (!is.null(keep_signals)) d <- d[signalname %in% keep_signals]
  if (!is.null(end_year)) d <- d[yr <= end_year]
  d[, `:=`(
    postSampC = as.integer(yr > SampleEndYear),
    postPubC = as.integer(yr > pubYear),
    yyyymm = yr * 100L + month(date)
  )]
  quality <- d[postSampC == 0L, .(
    in_sample_mean_pct = mean(retPct),
    in_sample_tstat = mean(retPct) / (sd(retPct) / sqrt(.N))
  ), by = signalname]
  quality_keep <- quality[
    is.finite(in_sample_tstat) & in_sample_tstat > 2,
    signalname
  ]
  d <- merge(d[signalname %in% quality_keep], quality, by = "signalname")
  d[, retScaled := 100 * retPct / in_sample_mean_pct]
  d[]
}

cz_all <- build_cz_panel()
run_pair(
  "cz_all_scaled_reference", cz_all, "retScaled", "CZ original-paper ports",
  "in-sample mean = 100"
)
run_pair(
  "cz_all_unscaled_reference", cz_all, "retPct", "CZ original-paper ports",
  "percent/month"
)
cz_all_grand_scaled <- grand_mean_scale(cz_all)
run_pair(
  "cz_all_grand_mean_scaled_reference", cz_all_grand_scaled,
  "retGrandScaled", "CZ original-paper ports",
  grand_scale_label(cz_all_grand_scaled)
)

mp_names <- unique(
  fread(CZ_MAP, encoding = "UTF-8")[
    metastudy == "MP" & !is.na(ourname), ourname
  ]
)
mp_names[mp_names == "IdioRisk"] <- "IdioVol3F"
cz_mp <- build_cz_panel(keep_signals = mp_names, end_year = 2013L)
run_pair(
  "cz_mp_matched_2013_scaled_reference", cz_mp, "retScaled",
  "CZ original-paper ports", "in-sample mean = 100"
)

# ---- curated JKP-to-CZ metadata crosswalk --------------------------------

# This is the Panel E matching procedure from commit 833cc71, retained only to
# identify the CZ signals corresponding to quality-screened JKP factors. The
# table regression itself is estimated from current signal-level CZ terciles
# in cz.R, so it is a strict subset of that table's Panel A.
normalize_id <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
strip_holding_period <- function(x) sub("(12|6|1)$", "", x)
MIN_MNEMONIC_CHARS <- 2L

CZ_RENAMES <- c(
  IdioRisk = "IdioVol3F",
  zerotrade = "zerotrade6M",
  zerotradeAlt1 = "zerotrade1M",
  zerotradeAlt12 = "zerotrade12M"
)

hxz <- fread(CZ_MAP, encoding = "UTF-8")[metastudy == "HXZ"]
hxz[, ourname := fifelse(
  ourname %in% names(CZ_RENAMES), unname(CZ_RENAMES[ourname]), ourname
)]
hxz[, id_norm := normalize_id(theirname)]
hxz[, id_base := strip_holding_period(id_norm)]

manual_crosswalk <- data.table(
  signalname = c(
    "resff3_6_1", "resff3_12_1", "iskew_hxz4_21d",
    "inv_gr1a", "ivol_capm_21d", "beta_dimson_21d"
  ),
  cz_signalname = c(
    "ResidualMomentum6m", "ResidualMomentum", "ReturnSkewQF",
    "ChInv", "IdioVolCAPM", "BetaDimson"
  ),
  method = c(
    "manual: e6 -> epsilon6",
    "manual: e11 -> epsilon12",
    "manual: lsq -> Isq",
    "manual: resolve HXZ Ivc collision by factor name",
    "manual: resolve HXZ Ivc collision by factor name",
    "manual: repair mis-encoded betaD mnemonic (HXZ BD, not D1)"
  )
)

crosswalk_rows <- lapply(seq_len(nrow(meta)), function(i) {
  row <- meta[i]
  manual <- manual_crosswalk[signalname == row$signalname]
  if (nrow(manual)) {
    return(data.table(
      signalname = row$signalname,
      cz_signalname = manual$cz_signalname,
      method = manual$method,
      candidate_count = 1L
    ))
  }
  key <- normalize_id(row$abr_hxz)
  if (is.na(row$abr_hxz) || !nzchar(key)) {
    return(data.table(
      signalname = row$signalname,
      cz_signalname = NA_character_,
      method = "unmatched: no HXZ mnemonic",
      candidate_count = 0L
    ))
  }
  candidates <- if (nchar(key) >= MIN_MNEMONIC_CHARS) {
    unique(hxz[id_norm == key | id_base == key, ourname])
  } else {
    unique(hxz[id_norm == key, ourname])
  }
  if (length(candidates) == 1L) {
    data.table(
      signalname = row$signalname,
      cz_signalname = candidates,
      method = "curated HXZ-to-CZ mapping",
      candidate_count = 1L
    )
  } else {
    data.table(
      signalname = row$signalname,
      cz_signalname = NA_character_,
      method = if (length(candidates)) {
        paste0("ambiguous HXZ targets: ", paste(candidates, collapse = "; "))
      } else {
        "unmatched HXZ mnemonic"
      },
      candidate_count = length(candidates)
    )
  }
})

crosswalk <- rbindlist(crosswalk_rows)
crosswalk <- merge(
  crosswalk,
  meta[, .(
    signalname, jkp_name = name, jkp_group = group, cite, pubYear, abr_hxz,
    jkp_sample_start = SampleStartYear, jkp_sample_end = SampleEndYear
  )],
  by = "signalname",
  all.x = TRUE
)

unresolved <- setdiff(
  crosswalk[!is.na(cz_signalname), unique(cz_signalname)],
  cz_doc$Acronym
)
check(
  length(unresolved) == 0L,
  "Crosswalk targets missing from CZ SignalDoc: %s.",
  paste(unresolved, collapse = ", ")
)

crosswalk <- merge(
  crosswalk,
  cz_doc[, .(
    cz_signalname = Acronym,
    cz_name = LongDescription,
    cz_category = get("Cat.Signal"),
    cz_sample_start = SampleStartYear,
    cz_sample_end = SampleEndYear,
    cz_year = as.integer(Year)
  )],
  by = "cz_signalname",
  all.x = TRUE
)
set(
  crosswalk,
  j = "duplicated_cz_target",
  value = !is.na(crosswalk$cz_signalname) & (
    duplicated(crosswalk$cz_signalname) |
      duplicated(crosswalk$cz_signalname, fromLast = TRUE)
  )
)
set(
  crosswalk,
  j = "eligible_common",
  value = (
    !is.na(crosswalk$pubYear) & !is.na(crosswalk$cz_signalname) &
      crosswalk$cz_category %in% "Predictor" &
      !crosswalk$duplicated_cz_target
  )
)

common <- crosswalk[eligible_common == TRUE]
check(nrow(common) > 0L, "The common JKP-CZ crosswalk is empty.")
cz_quality_keep <- unique(cz_all$signalname)
matched_pairs <- common[
  signalname %in% quality_keep & cz_signalname %in% cz_quality_keep
]
check(nrow(matched_pairs) > 0L, "No metadata-matched pairs pass both screens.")
set(
  crosswalk,
  j = "eligible_quality_matched",
  value = crosswalk$signalname %in% matched_pairs$signalname
)
setorder(crosswalk, -eligible_quality_matched, signalname)
setorder(matched_pairs, jkp_group, signalname)
fwrite(crosswalk, file.path(OUT_DIR, "jkp-cz-crosswalk.csv"))
fwrite(matched_pairs, file.path(OUT_DIR, "jkp-cz-matched-pairs.csv"))

result_table <- rbindlist(results, use.names = TRUE, fill = TRUE)
setorder(result_table, specification, fixed_effects)
fwrite(result_table, file.path(OUT_DIR, "jkp-regressions.csv"))

# ---- diagnostics ----------------------------------------------------------

main_panel <- build_panel(ew, cited = TRUE, scaled = FALSE)
main_means <- attr(main_panel, "means")
return_names <- unique(ew$signalname)
cited_names <- meta[!is.na(pubYear), signalname]
# means is built by aggregating the in-sample rows, so a factor with no
# in-sample months is absent from it rather than present with a zero count.
# Detect the condition by set difference against the panel, not by looking for
# zeros inside the aggregation.
cited_with_returns <- intersect(cited_names, return_names)
missing_in_sample <- setdiff(cited_with_returns, main_means$signalname)
diagnostics <- data.table(
  item = c(
    "metadata rows",
    "unique metadata factor mnemonics",
    "unique equal-weighted return series",
    "duplicate equal-weighted signal-month rows",
    "cited metadata factors",
    "uncited metadata factors excluded from baseline",
    "cited factors missing all return history",
    "cited factors missing in-sample observations",
    "cited factors with nonpositive in-sample mean",
    "positive-mean factors available before t>2 screen",
    "baseline quality factors retained (t>2)",
    "known sample-end/publication exception",
    "return and metadata directions disagree",
    "underlying portfolio series reconstructed",
    "maximum underlying portfolio reconstruction error"
  ),
  value = as.character(c(
    nrow(meta_all),
    uniqueN(meta$signalname),
    uniqueN(ew$signalname),
    ew[, .N, by = .(signalname, date)][N > 1L, .N],
    length(cited_names),
    meta[is.na(pubYear), .N],
    length(setdiff(cited_names, return_names)),
    length(missing_in_sample),
    main_means[
      is.na(in_sample_mean_decimal) | in_sample_mean_decimal <= 0, .N
    ],
    uniqueN(main$signalname),
    length(quality_keep),
    paste0(
      date_exceptions$signalname, ": ",
      date_exceptions$SampleEndYear, " > ", date_exceptions$pubYear
    ),
    audit_join[direction_file != direction_meta, .N],
    nrow(portfolio_audits),
    format(max(portfolio_audits$max_absolute_error), scientific = TRUE)
  ))
)
fwrite(diagnostics, file.path(OUT_DIR, "jkp-diagnostics.csv"))

# ---- console report -------------------------------------------------------

cat("\n========== JKP input and panel audit ==========\n")
print(diagnostics)

show_specs <- c(
  "cz_all_scaled_reference", "cz_mp_matched_2013_scaled_reference",
  "cz_all_unscaled_reference", "cz_all_grand_mean_scaled_reference",
  "baseline_quality_t2",
  "baseline_quality_t2_unscaled",
  "baseline_quality_t2_grand_mean_scaled",
  "end_2013_scaled",
  "publication_cutoff_january", "publication_cutoff_june",
  "publication_cutoff_december"
)
print_cols <- c(
  "specification", "fixed_effects", "post_sample", "post_sample_se",
  "additional_post_publication", "additional_post_publication_se",
  "total_post_publication_change", "observations", "factors",
  "singleton_observations_removed"
)
cat("\n========== JKP decay regressions ==========\n")
print(result_table[specification %in% show_specs, ..print_cols], digits = 4)
cat(
  "\nHeadline interpretation: the signed t>2 quality screen defines the ",
  "97-factor JKP baseline. Weighting comparisons are estimated separately ",
  "by Appendices/SA15_TimeFERobustness/R/jkp-alternative-specs.R.\n",
  sep = ""
)

cat("\nSaved:\n")
cat("  ", file.path(OUT_DIR, "jkp-regressions.csv"), "\n", sep = "")
cat("  ", file.path(OUT_DIR, "jkp-diagnostics.csv"), "\n", sep = "")
cat("  ", file.path(OUT_DIR, "jkp-cz-crosswalk.csv"), "\n", sep = "")
cat("  ", file.path(OUT_DIR, "jkp-cz-matched-pairs.csv"), "\n", sep = "")
cat("  ", file.path(RAW_DIR, "download-manifest.csv"), "\n", sep = "")
