# Estimate the McLean-Pontiff-style decay regressions of the time-fixed-effects
# robustness appendix, on every factor library except the CZ signal-level
# terciles.
#
# How to run: from flex-mining/, normally through
#   Rscript Appendices/SA15_TimeFERobustness/run.R build
# Inputs:  the pinned JKP factor returns, underlying-portfolio archives, and
#            factor_details.xlsx, all downloaded here on first use
#          the pinned CZ portfolio returns, signal documentation, and
#            meta-replication mapping under ../Data/Raw/TimeFERobustness
#            (see download.R)
# Outputs: ../Data/Raw/TimeFERobustness/jkp/download-manifest.csv
#          ../Data/Processed/TimeFERobustness/output/
#            mp-regressions.csv
#            jkp-regressions.csv, jkp-diagnostics.csv
#            jkp-cz-crosswalk.csv, jkp-cz-matched-pairs.csv
#            cz-alternative-specs.csv
#            jkp-alternative-specs.csv
#            jkp-cz-full-date-comparison.csv
#
# Five sections, in the order they appear below:
#
#   1. MP-style decay on CZ's own published portfolios, including the closest
#      available match to MP Table II col (1).
#   2. The JKP baseline, plus CZ reference rows on identical definitions, plus
#      the curated JKP-to-CZ crosswalk that cz_terciles.R matches on.
#   3. Alternative CZ portfolio constructions.
#   4. The JKP within-leg weighting schemes.
#   5. Date metadata for the two screened libraries.
#
# They share one process because they share their inputs: the JKP metadata and
# equal-weighted returns are read once here and used by sections 2, 4, and 5,
# and the screened CZ panel built in section 1 is reused by sections 2 and 5.
# The signal-level CZ terciles are estimated separately, in cz_terciles.R,
# because that stage holds multi-gigabyte data in memory.
#
# Interpretation: MP and CZ agree closely on total post-publication decay
# without month fixed effects. With month fixed effects, CZ continues to show
# post-sample decay but little additional change at publication. MP's own
# additional time-FE publication effect is smaller than its baseline effect,
# and its standard error is not recoverable from the published table.

source("Appendices/SA15_TimeFERobustness/setup.R")

out_dir <- timefeSettings$paths$output
raw_dir <- timefeSettings$paths$jkp

report_cols <- c(
  "specification", "fixed_effects", "post_sample", "post_sample_se",
  "additional_post_publication", "additional_post_publication_se",
  "total_post_publication_change", "observations", "factors"
)

# This stage and cz_terciles.R read files that only download.R produces. Check
# them before the JKP downloads below, so a run that skipped `download` stops
# at once with a clear instruction rather than fetching the JKP inputs first
# and then failing on the first missing CZ file. The CRSP cache is not checked
# here: cz_terciles.R builds it on first run.
downloaded_inputs <- unlist(timefeSettings$files[c(
  "op_portfolios", "signal_doc", "meta_replications", "signal_panel"
)])
missing_inputs <- downloaded_inputs[
  !(file.exists(downloaded_inputs) | dir.exists(downloaded_inputs))
]
check(
  length(missing_inputs) == 0L,
  paste0(
    "Missing downloaded input(s):\n  %s\n",
    "Run `Rscript Appendices/SA15_TimeFERobustness/run.R download` first."
  ),
  paste(missing_inputs, collapse = "\n  ")
)

# =========================================================================
# Input readers
# =========================================================================
#
# These read the pinned external files and validate them. Every reader is used
# only by this stage; the readers shared with cz_terciles.R are in setup.R.
# The appendix is built from external libraries that are revised over time, so
# a schema, weighting, or unit change must stop the run rather than propagate
# into an exhibit.

# Downloading -------------------------------------------------------------

# Fetch url to dest unless dest already exists. Returns TRUE when this call
# actually downloaded the file. A failed download must not leave a partial
# file behind, so the fetch lands on a temporary name first.
download_pinned <- function(url, dest) {
  if (file.exists(dest) && file.info(dest)$size > 0) return(FALSE)
  message("Downloading ", basename(dest), " ...")
  tmp <- tempfile(pattern = paste0(basename(dest), "-"),
                  tmpdir = dirname(dest))
  on.exit(unlink(tmp), add = TRUE)
  status <- tryCatch(
    download.file(url, tmp, mode = "wb", quiet = TRUE),
    error = function(e) e
  )
  check(
    !inherits(status, "error") && file.exists(tmp) && file.info(tmp)$size > 0,
    paste0(
      "Could not download %s from %s. Do not substitute another input. The ",
      "likely cause is network/firewall access: %s"
    ),
    basename(dest), url,
    if (inherits(status, "error")) conditionMessage(status) else "empty file"
  )
  check(file.rename(tmp, dest), "Could not move the download to %s.", dest)
  TRUE
}

read_zip_csv <- function(path) {
  listing <- unzip(path, list = TRUE)
  csv <- listing$Name[grepl("\\.csv$", listing$Name, ignore.case = TRUE)]
  check(length(csv) == 1L, "%s must contain exactly one CSV.", path)

  extract_dir <- tempfile("timefe-unzip-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)

  extracted <- unzip(path, files = csv, exdir = extract_dir)
  check(
    length(extracted) == 1L && file.exists(extracted),
    "Could not extract the CSV from %s.", path
  )
  data.table::fread(extracted)
}

# JKP factor library ------------------------------------------------------

jkp_zip_path <- function(weighting) {
  file.path(
    timefeSettings$paths$jkp,
    sprintf("[usa]_[all_factors]_[monthly]_[%s].zip", weighting)
  )
}

jkp_zip_url <- function(weighting) {
  paste0(
    "https://jkpfactors-data.s3.amazonaws.com/public/",
    sprintf("%%5Busa%%5D_%%5Ball_factors%%5D_%%5Bmonthly%%5D_%%5B%s%%5D.zip",
            weighting)
  )
}

# Factor metadata: mnemonic, in-sample period, publication year, and the sign
# convention JKP applies to the published return series. The unfiltered row
# count is attached as the "detail_rows" attribute for the input diagnostics.
jkp_factor_metadata <- function(path = timefeSettings$files$jkp_details) {
  check(file.exists(path), "%s does not exist.", path)
  all_rows <- as.data.table(readxl::read_excel(path, sheet = "details"))
  meta <- all_rows[!is.na(abr_jkp) & nzchar(abr_jkp)]
  setnames(meta, "abr_jkp", "signalname")
  meta[, `:=`(
    SampleStartYear = parse_period(`in-sample period`, "start"),
    SampleEndYear = parse_period(`in-sample period`, "end"),
    pubYear = parse_first_year(cite),
    direction_meta = as.integer(direction)
  )]
  check(
    all(!is.na(meta$SampleStartYear) & !is.na(meta$SampleEndYear)),
    "At least one JKP in-sample period did not parse."
  )
  setattr(meta, "detail_rows", nrow(all_rows))
  meta[]
}

# Published long-short factor returns for one weighting, in decimal units.
jkp_returns <- function(weighting, path = jkp_zip_path(weighting)) {
  x <- read_zip_csv(path)
  required <- c(
    "location", "name", "freq", "weighting", "direction", "date", "ret"
  )
  check(all(required %in% names(x)),
        "%s is missing required columns: %s", path,
        paste(setdiff(required, names(x)), collapse = ", "))
  check(all(x$location == "usa") && all(x$freq == "monthly") &&
          all(x$weighting == weighting),
        "%s is not the requested US monthly %s file.", path, weighting)
  setnames(x, "name", "signalname")
  x[, date := as.IDate(date)]
  check(x[, .N, by = .(signalname, date)][N > 1L, .N] == 0L,
        "Duplicate signalname-month observations found in %s.", path)
  check(all(is.finite(x$ret)), "Non-finite returns found in %s.", path)
  # Decimal monthly returns have typical absolute values near 0.01--0.03. A
  # file switched to percent units would sail through every other check.
  med_abs <- median(abs(x$ret))
  check(med_abs > 0.001 && med_abs < 0.10 && max(abs(x$ret)) < 2,
        paste0(
          "Return-unit audit failed for %s (median |ret| = %.4f). ",
          "Expected decimal monthly returns."
        ),
        path, med_abs)
  x[]
}

# Long-short returns from a Chen-Zimmermann portfolio file, in percent/month.
cz_longshort_returns <- function(path = timefeSettings$files$op_portfolios) {
  check(file.exists(path), "%s does not exist.", path)
  ports <- fread(path)
  check(all(c("signalname", "port", "date", "ret") %in% names(ports)),
        "%s has an unexpected portfolio schema.", path)
  check("LS" %in% ports$port,
        "%s does not contain a precomputed LS portfolio.", path)
  ports[port == "LS", .(
    signalname, date = as.IDate(date), retPct = as.numeric(ret)
  )]
}

# Signals that McLean-Pontiff study, named as CZ names them. CZ renamed
# IdioRisk (Ang et al. 2006) to IdioVol3F after the mapping was written.
mp_signal_names <- function(path = timefeSettings$files$meta_replications) {
  check(file.exists(path), "%s does not exist.", path)
  names_mp <- unique(na.omit(
    fread(path, encoding = "UTF-8")[metastudy == "MP", ourname]
  ))
  names_mp[names_mp == "IdioRisk"] <- "IdioVol3F"
  names_mp
}

# =========================================================================
# Pinned inputs
# =========================================================================

# Three factors whose long-short series are rebuilt below from the official
# underlying high and low portfolios, as a sign and arithmetic audit.
audit_signals <- c("age", "be_me", "ret_12_1")
audit_zips <- file.path(
  raw_dir, paste0("[usa]_[", audit_signals, "]_[monthly]_[ew]_[portfolios].zip")
)

inputs <- rbind(
  data.table(
    file = c(jkp_zip_path("ew"), timefeSettings$files$jkp_details),
    url = c(
      jkp_zip_url("ew"),
      paste0(
        "https://raw.githubusercontent.com/bkelly-lab/jkp-data/",
        timefeSettings$pins$jkp_commit,
        "/src/jkp/data/resources/factor_details.xlsx"
      )
    ),
    role = c("baseline equal-weighted factor returns", "factor metadata")
  ),
  data.table(
    file = audit_zips,
    url = paste0(
      "https://jkpfactors-data.s3.amazonaws.com/public/portfolios/",
      "%5Busa%5D_%5B", audit_signals, "%5D_%5Bmonthly%5D_%5Bew%5D.zip"
    ),
    role = paste("underlying-portfolio sign audit:", audit_signals)
  )
)

# Record which inputs this run actually fetched. Everything else was already
# on disk, so its timestamp is a local file mtime and not a download date.
fetched_this_run <- vapply(
  seq_len(nrow(inputs)),
  function(i) download_pinned(inputs$url[i], inputs$file[i]),
  logical(1)
)
run_time_ny <- format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z",
                      tz = "America/New_York")

manifest <- copy(inputs)
manifest[, `:=`(
  fetched_this_run = fetched_this_run,
  # Only a download date when fetched_this_run is TRUE; otherwise the mtime of
  # a file that was already present.
  file_mtime_ny = format(file.info(file)$mtime, "%Y-%m-%d %H:%M:%S %Z",
                         tz = "America/New_York"),
  downloaded_ny = fifelse(fetched_this_run, run_time_ny, NA_character_),
  bytes = as.numeric(file.info(file)$size),
  sha256 = vapply(file, digest::digest, character(1),
                  algo = "sha256", file = TRUE)
)]
setcolorder(manifest, c("file", "role", "url", "fetched_this_run",
                        "downloaded_ny", "file_mtime_ny", "bytes", "sha256"))
fwrite(manifest, file.path(raw_dir, "download-manifest.csv"))

meta <- jkp_factor_metadata()
ew <- jkp_returns("ew")
factor_dates <- meta[
  , .(signalname, SampleStartYear, SampleEndYear, pubYear)
]

# ---- input audits --------------------------------------------------------

date_exceptions <- meta[
  !is.na(pubYear) & SampleEndYear > pubYear,
  .(signalname, SampleEndYear, pubYear)
]

# Published factor returns are already signed. Signing them a second time
# would silently reverse every coefficient in this appendix, so the file's own
# direction column is held against the metadata.
direction_audit <- merge(
  ew[, .(signalname, date, direction_file = as.integer(direction))],
  meta[, .(signalname, direction_meta)],
  by = "signalname", all.x = TRUE
)
check(!anyNA(direction_audit$direction_meta),
      "At least one return series failed to join to factor metadata.")
check(all(direction_audit$direction_file == direction_audit$direction_meta),
      paste0("Return-file and metadata directions disagree. Published factor ",
             "returns are already signed and must not be signed again."))

# Rebuild three long-short series from the official underlying high and low
# portfolio files, checking both the sign and the factor-return arithmetic.
portfolio_audits <- rbindlist(lapply(seq_along(audit_signals), function(i) {
  signal <- audit_signals[i]
  p <- read_zip_csv(audit_zips[i])
  check(all(c("location", "name", "freq", "weighting", "pf", "date", "ret")
            %in% names(p)),
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
  p[, reconstructed := meta[signalname == signal, direction_meta] *
      (`3` - `1`)]
  z <- merge(p, ew[signalname == signal, .(date, published = ret)], by = "date")
  z <- z[is.finite(reconstructed) & is.finite(published)]
  # A handful of overlapping months would make the reconstruction check
  # vacuous, so require that the audit actually covers the series.
  check(nrow(z) >= 0.9 * ew[signalname == signal, .N],
        paste0("Underlying portfolio audit for %s covers only %d of %d ",
               "published months."),
        signal, nrow(z), ew[signalname == signal, .N])
  max_error <- max(abs(z$reconstructed - z$published))
  check(max_error < 1e-12,
        "Underlying portfolio reconstruction failed for %s (max error %.3g).",
        signal, max_error)
  data.table(signalname = signal, overlapping_months = nrow(z),
             max_absolute_error = max_error)
}))

cz_signal_doc <- fread(timefeSettings$files$signal_doc)
cz_doc <- cz_predictor_doc()
cz_returns <- cz_longshort_returns()
mp_names <- mp_signal_names()

# ---- panel builders ------------------------------------------------------

# JKP publishes decimal returns; the panel helpers work in percent. In-sample
# moments are added by the caller, after any subsetting.
jkp_panel <- function(returns, pub_month = NULL) {
  add_event_indicators(
    merge(returns[, .(signalname, date, retPct = 100 * ret)], factor_dates,
          by = "signalname"),
    pub_month = pub_month
  )
}

# One CZ panel builder serves both the MP-style section and the JKP section's
# reference rows: they differ only in which signals and years they keep.
cz_panel <- function(keep_signals = NULL, end_year = NULL, pub_month = NULL) {
  d <- merge(cz_returns, cz_doc, by = "signalname")
  if (!is.null(keep_signals)) d <- d[signalname %in% keep_signals]
  d <- add_event_indicators(d, pub_month = pub_month)
  if (!is.null(end_year)) d <- d[yr <= end_year]
  quality_screen(add_in_sample_stats(d))
}

# The two screened CZ panels both sections report on.
cz_all <- scale_by_signal_mean(cz_panel())
cz_mp_2013 <- scale_by_signal_mean(
  cz_panel(keep_signals = mp_names, end_year = 2013L)
)

# =========================================================================
# 1. MP-style decay regressions on CZ's published portfolios
# =========================================================================
#
#   A. cz_all_scaled              published predictors passing the in-sample
#                                 t-statistic screen, returns scaled so each
#                                 signal's in-sample mean is 100.
#   B. cz_mp_matched_2013_scaled  the same, restricted to CZ signals that
#                                 appear in MP and truncated to MP's sample.
#   C. cz_mp_matched_2013_unscaled_pub_*
#                                 the closest match to MP Table II col (1):
#                                 MP subset through 2013, unscaled returns in
#                                 MP's percent/month units, with the three
#                                 publication-date conventions.

mp_results <- list()

mp_results$cz_all <- decay_rows(
  cz_all, "retScaled",
  specification = "cz_all_scaled", label = "CZ original portfolios",
  scale = "in-sample mean = 100"
)
mp_results$cz_mp_scaled <- decay_rows(
  cz_mp_2013, "retScaled",
  specification = "cz_mp_matched_2013_scaled", label = "CZ, MP signals",
  scale = "in-sample mean = 100"
)

# The grand in-sample mean (MP's 0.652 analog) is attached by
# grand_mean_scale() and reported alongside the unscaled coefficients, so the
# table can put MP's normalized numbers and these on one scale. The
# publication convention shifts postPubC only, so the panel and its in-sample
# moments -- including that grand mean -- are the same in all three variants.
mp_pub_months <- c(December = 12L, June = 6L, January = 1L)
for (cutoff in names(mp_pub_months)) {
  mp_unscaled <- grand_mean_scale(cz_panel(
    keep_signals = mp_names, end_year = 2013L,
    pub_month = mp_pub_months[[cutoff]]
  ))
  mp_results[[paste0("cz_mp_unscaled_", cutoff)]] <- decay_rows(
    mp_unscaled, "retPct",
    specification = paste0(
      "cz_mp_matched_2013_unscaled_pub_", tolower(substr(cutoff, 1, 3))
    ),
    label = paste("CZ, MP signals, publication in", cutoff),
    scale = "percent/month"
  )
  if (cutoff == "December") {
    cat(sprintf(
      "MP-matched predictors: %d, obs: %d, grand mean %.3f %%/mo (MP: 0.652)\n",
      uniqueN(mp_unscaled$signalname), nrow(mp_unscaled),
      attr(mp_unscaled, "grand_mean_pct")
    ))
  }
}

# MP's published headline rows, for the same table. The additional-effect SE
# in Table II is implied by MP's reported equality-test p-value; Table III
# does not report enough information to recover it.
mp_results$published <- data.table(
  specification = c(
    "mp_published_table_ii_col1", "mp_published_table_iii_col4"
  ),
  label = "MP published",
  scale = "percent/month",
  fixed_effects = c("predictor", "predictor + month"),
  post_sample = c(-0.150, -0.179),
  post_sample_se = c(0.077, 0.080),
  additional_post_publication = c(-0.187, -0.131),
  additional_post_publication_se = c(0.083, NA_real_),
  total_post_publication_change = c(-0.337, -0.310),
  total_post_publication_change_se = c(0.090, 0.122),
  normalization_mean_bps = 58.2,
  mean_in_sample_bps = 58.2,
  min_in_sample_mean_bps = NA_real_,
  observations = 51851L,
  factors = 97L,
  singleton_observations_removed = NA_integer_
)

mp_result_table <- rbindlist(mp_results, use.names = TRUE, fill = TRUE)
setorder(mp_result_table, specification, fixed_effects)
fwrite(mp_result_table, file.path(out_dir, "mp-regressions.csv"))

cat("\n========== 1. MP-style decay regressions ==========\n")
print(mp_result_table[, ..report_cols], digits = 4)
cat(
  "\nMP Table II col (1):  between -0.150 (0.077), additional -0.187,",
  "total -0.337 (0.090)\n",
  "MP Table III col (4), with month FE: between -0.179 (0.080),",
  "additional -0.131 (SE unavailable), total -0.310 (0.122)\n",
  "The replicated incremental publication effect under month fixed effects",
  "is small and imprecise; MP's time-FE table does not establish its",
  "significance.\n"
)

# =========================================================================
# 2. JKP decay regressions, CZ reference rows, and the JKP-to-CZ crosswalk
# =========================================================================
#
# The baseline is US monthly equal-weighted long-short factors that pass the
# in-sample t-statistic screen, with returns scaled factor by factor so the
# in-sample mean is 100. The CZ reference rows put both libraries in one
# machine-readable table on identical definitions.

jkp_results <- list()

baseline <- scale_by_signal_mean(
  quality_screen(add_in_sample_stats(jkp_panel(ew)))
)
quality_keep <- unique(baseline$signalname)
jkp_results$baseline <- decay_rows(
  baseline, "retScaled",
  specification = "baseline_quality_t2", label = "JKP equal weighted",
  scale = "in-sample mean = 100"
)
jkp_results$baseline_unscaled <- decay_rows(
  baseline, "retPct",
  specification = "baseline_quality_t2_unscaled",
  label = "JKP equal weighted", scale = "percent/month"
)
baseline_grand <- grand_mean_scale(baseline)
jkp_results$baseline_grand <- decay_rows(
  baseline_grand, "retGrandScaled",
  specification = "baseline_quality_t2_grand_mean_scaled",
  label = "JKP equal weighted", scale = grand_scale_label(baseline_grand)
)

# Sample- and publication-date variants, holding the baseline factor set fixed
# so only the date convention changes.
end_2013 <- scale_by_signal_mean(add_in_sample_stats(
  jkp_panel(ew)[signalname %in% quality_keep & yr <= 2013L]
))
jkp_results$end_2013 <- decay_rows(
  end_2013, "retScaled",
  specification = "end_2013_scaled", label = "JKP equal weighted",
  scale = "in-sample mean = 100"
)

jkp_pub_months <- c(january = 1L, june = 6L, december = 12L)
for (cutoff in names(jkp_pub_months)) {
  shifted <- scale_by_signal_mean(add_in_sample_stats(
    jkp_panel(ew, pub_month = jkp_pub_months[[cutoff]])[
      signalname %in% quality_keep
    ]
  ))
  jkp_results[[paste0("pub_", cutoff)]] <- decay_rows(
    shifted, "retScaled",
    specification = paste0("publication_cutoff_", cutoff),
    label = "JKP equal weighted", scale = "in-sample mean = 100"
  )
}

# The CZ panels from section 1, reported again under the JKP specification
# names so the two libraries sit in one table.
jkp_results$cz_all <- decay_rows(
  cz_all, "retScaled",
  specification = "cz_all_scaled_reference",
  label = "CZ original portfolios", scale = "in-sample mean = 100"
)
jkp_results$cz_all_unscaled <- decay_rows(
  cz_all, "retPct",
  specification = "cz_all_unscaled_reference",
  label = "CZ original portfolios", scale = "percent/month"
)
cz_all_grand <- grand_mean_scale(cz_all)
jkp_results$cz_all_grand <- decay_rows(
  cz_all_grand, "retGrandScaled",
  specification = "cz_all_grand_mean_scaled_reference",
  label = "CZ original portfolios", scale = grand_scale_label(cz_all_grand)
)
jkp_results$cz_mp <- decay_rows(
  cz_mp_2013, "retScaled",
  specification = "cz_mp_matched_2013_scaled_reference",
  label = "CZ, MP signals", scale = "in-sample mean = 100"
)

jkp_result_table <- rbindlist(jkp_results, use.names = TRUE, fill = TRUE)
setorder(jkp_result_table, specification, fixed_effects)
fwrite(jkp_result_table, file.path(out_dir, "jkp-regressions.csv"))

# ---- curated JKP-to-CZ metadata crosswalk --------------------------------

# Identifies the CZ signals corresponding to quality-screened JKP factors.
# The matched regression itself is estimated from current signal-level CZ
# terciles in cz_terciles.R, so its sample is a strict subset of that table.
normalize_id <- function(x) tolower(gsub("[^A-Za-z0-9]", "", x))
strip_holding_period <- function(x) sub("(12|6|1)$", "", x)
min_mnemonic_chars <- 2L

cz_renames <- c(
  IdioRisk = "IdioVol3F",
  zerotrade = "zerotrade6M",
  zerotradeAlt1 = "zerotrade1M",
  zerotradeAlt12 = "zerotrade12M"
)

hxz <- fread(timefeSettings$files$meta_replications,
             encoding = "UTF-8")[metastudy == "HXZ"]
hxz[, ourname := fifelse(ourname %in% names(cz_renames),
                         unname(cz_renames[ourname]), ourname)]
hxz[, `:=`(
  id_norm = normalize_id(theirname),
  id_base = strip_holding_period(normalize_id(theirname))
)]

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

match_one <- function(jkp_id, hxz_mnemonic) {
  manual <- manual_crosswalk[signalname == jkp_id]
  if (nrow(manual)) {
    return(data.table(signalname = jkp_id,
                      cz_signalname = manual$cz_signalname,
                      method = manual$method, candidate_count = 1L))
  }
  key <- normalize_id(hxz_mnemonic)
  if (is.na(hxz_mnemonic) || !nzchar(key)) {
    return(data.table(signalname = jkp_id, cz_signalname = NA_character_,
                      method = "unmatched: no HXZ mnemonic",
                      candidate_count = 0L))
  }
  candidates <- if (nchar(key) >= min_mnemonic_chars) {
    unique(hxz[id_norm == key | id_base == key, ourname])
  } else {
    unique(hxz[id_norm == key, ourname])
  }
  if (length(candidates) == 1L) {
    data.table(signalname = jkp_id, cz_signalname = candidates,
               method = "curated HXZ-to-CZ mapping", candidate_count = 1L)
  } else {
    data.table(
      signalname = jkp_id, cz_signalname = NA_character_,
      method = if (length(candidates)) {
        paste0("ambiguous HXZ targets: ", paste(candidates, collapse = "; "))
      } else {
        "unmatched HXZ mnemonic"
      },
      candidate_count = length(candidates)
    )
  }
}

crosswalk <- rbindlist(Map(match_one, meta$signalname, meta$abr_hxz))
crosswalk <- merge(
  crosswalk,
  meta[, .(signalname, jkp_name = name, jkp_group = group, cite, pubYear,
           abr_hxz, jkp_sample_start = SampleStartYear,
           jkp_sample_end = SampleEndYear)],
  by = "signalname", all.x = TRUE
)

unresolved <- setdiff(crosswalk[!is.na(cz_signalname), unique(cz_signalname)],
                      cz_signal_doc$Acronym)
check(length(unresolved) == 0L,
      "Crosswalk targets missing from CZ SignalDoc: %s.",
      paste(unresolved, collapse = ", "))

crosswalk <- merge(
  crosswalk,
  cz_signal_doc[, .(cz_signalname = Acronym, cz_name = LongDescription,
             cz_category = get("Cat.Signal"), cz_sample_start = SampleStartYear,
             cz_sample_end = SampleEndYear, cz_year = as.integer(Year))],
  by = "cz_signalname", all.x = TRUE
)
crosswalk[, duplicated_cz_target := !is.na(cz_signalname) &
            (duplicated(cz_signalname) |
               duplicated(cz_signalname, fromLast = TRUE))]
crosswalk[, eligible_common := !is.na(pubYear) & !is.na(cz_signalname) &
            cz_category %in% "Predictor" & !duplicated_cz_target]

common <- crosswalk[eligible_common == TRUE]
check(nrow(common) > 0L, "The common JKP-CZ crosswalk is empty.")
matched_pairs <- common[
  signalname %in% quality_keep & cz_signalname %in% unique(cz_all$signalname)
]
check(nrow(matched_pairs) > 0L, "No metadata-matched pairs pass both screens.")
crosswalk[, eligible_quality_matched := signalname %in% matched_pairs$signalname]
setorder(crosswalk, -eligible_quality_matched, signalname)
setorder(matched_pairs, jkp_group, signalname)
fwrite(crosswalk, file.path(out_dir, "jkp-cz-crosswalk.csv"))
fwrite(matched_pairs, file.path(out_dir, "jkp-cz-matched-pairs.csv"))

# ---- JKP input and panel diagnostics -------------------------------------

unscreened <- add_in_sample_stats(jkp_panel(ew))
return_names <- unique(ew$signalname)
cited_names <- meta[!is.na(pubYear), signalname]
# add_in_sample_stats() aggregates the in-sample rows, so a factor with no
# in-sample months drops out of the panel rather than appearing with a zero
# count. Detect it by set difference, not by looking for zeros.
missing_in_sample <- setdiff(intersect(cited_names, return_names),
                             unique(unscreened$signalname))
signal_means <- unique(unscreened[, .(signalname, in_sample_mean_pct)])

jkp_diagnostics <- data.table(
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
    attr(meta, "detail_rows"),
    uniqueN(meta$signalname),
    uniqueN(ew$signalname),
    ew[, .N, by = .(signalname, date)][N > 1L, .N],
    length(cited_names),
    meta[is.na(pubYear), .N],
    length(setdiff(cited_names, return_names)),
    length(missing_in_sample),
    signal_means[is.na(in_sample_mean_pct) | in_sample_mean_pct <= 0, .N],
    signal_means[in_sample_mean_pct > 0, .N],
    length(quality_keep),
    paste0(date_exceptions$signalname, ": ", date_exceptions$SampleEndYear,
           " > ", date_exceptions$pubYear),
    direction_audit[direction_file != direction_meta, .N],
    nrow(portfolio_audits),
    format(max(portfolio_audits$max_absolute_error), scientific = TRUE)
  ))
)
fwrite(jkp_diagnostics, file.path(out_dir, "jkp-diagnostics.csv"))

cat("\n========== 2. JKP input and panel audit ==========\n")
print(jkp_diagnostics)
cat("\n========== 2. JKP decay regressions ==========\n")
print(jkp_result_table[, ..report_cols], digits = 4)
cat(
  "\nThe signed t>2 quality screen defines the JKP baseline. Weighting ",
  "comparisons are estimated in section 4 below.\n",
  sep = ""
)

# =========================================================================
# 3. Alternative CZ portfolio constructions
# =========================================================================
#
# Each construction is screened on its own in-sample returns, so a portfolio
# rule that changes which signals look reliable is reflected in the sample as
# well as in the coefficients.

suppressPackageStartupMessages(library(OpenSourceAP.DownloadR))

cz_alt_specs <- data.table(
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

# Google Drive downloads can fail under libcurl's HTTP/2 implementation in
# this container. In httr/libcurl, http_version = 2 requests HTTP/1.1.
httr::set_config(httr::config(http_version = 2))
openap <- OpenAP$new(release_year = timefeSettings$pins$release)

cz_alt_rows <- function(data_name, label) {
  path <- file.path(
    timefeSettings$paths$openap_raw,
    paste0(data_name, "-", timefeSettings$pins$release, ".csv")
  )
  if (!file.exists(path)) {
    message("Downloading ", data_name, " with OpenSourceAP.DownloadR ...")
    fwrite(as.data.table(openap$dl_port(data_name)), path)
  }
  d <- merge(cz_longshort_returns(path), cz_doc, by = "signalname")
  d <- scale_by_signal_mean(quality_screen(add_in_sample_stats(
    add_event_indicators(d[is.finite(retPct)])
  )))
  decay_rows(d, "retScaled", data_name = data_name, label = label)
}

cz_alt_table <- rbindlist(Map(
  cz_alt_rows, cz_alt_specs$data_name, cz_alt_specs$label
))
cz_alt_table[, spec_order := match(data_name, cz_alt_specs$data_name)]
setorder(cz_alt_table, spec_order, fixed_effects)
cz_alt_table[, spec_order := NULL]
fwrite(cz_alt_table, file.path(out_dir, "cz-alternative-specs.csv"))

cat("\n========== 3. Alternative CZ portfolio constructions ==========\n")
print(cz_alt_table[, .(
  label, fixed_effects, post_sample, post_sample_se,
  additional_post_publication, additional_post_publication_se,
  mean_in_sample_bps, factors
)], digits = 4)

# =========================================================================
# 4. JKP within-leg weighting schemes
# =========================================================================
#
# Unlike the baseline above, each weighting is screened on its OWN in-sample
# returns, matching the treatment of each alternative CZ construction in
# section 3.

jkp_alt_specs <- data.table(
  weighting = c("ew", "vw_cap", "vw"),
  label = c(
    "Equal weighted (baseline)", "Capped value weighted", "Value weighted"
  )
)

jkp_alt_rows <- function(weighting, label) {
  download_pinned(jkp_zip_url(weighting), jkp_zip_path(weighting))
  returns <- if (weighting == "ew") ew else jkp_returns(weighting)
  d <- scale_by_signal_mean(quality_screen(add_in_sample_stats(
    jkp_panel(returns)
  )))
  check(uniqueN(d$signalname) > 0L,
        "No factors pass the quality screen for %s.", weighting)
  decay_rows(d, "retScaled", data_name = weighting, label = label)
}

jkp_alt_table <- rbindlist(Map(
  jkp_alt_rows, jkp_alt_specs$weighting, jkp_alt_specs$label
))
jkp_alt_table[, spec_order := match(data_name, jkp_alt_specs$weighting)]
setorder(jkp_alt_table, spec_order, fixed_effects)
jkp_alt_table[, spec_order := NULL]
fwrite(jkp_alt_table, file.path(out_dir, "jkp-alternative-specs.csv"))

cat("\n========== 4. JKP weighting schemes ==========\n")
print(jkp_alt_table[, .(
  label, fixed_effects, post_sample, post_sample_se,
  additional_post_publication, additional_post_publication_se,
  mean_in_sample_bps, min_in_sample_mean_bps, factors
)], digits = 4)

# =========================================================================
# 5. Date metadata for the two screened libraries
# =========================================================================
#
# The libraries differ in how long they wait between the end of a signal's
# in-sample period and its publication, which is what the post-sample and
# additional post-publication coefficients are measured against. Both screened
# signal sets are already in hand: the JKP baseline and the CZ panel above.

date_libraries <- list(
  JKP = factor_dates[signalname %in% quality_keep],
  CZ = cz_doc[signalname %in% unique(cz_all$signalname)]
)

summarize_field <- function(data, dataset, field) {
  value <- data[[field]]
  value <- value[!is.na(value)]
  data.table(
    dataset, field,
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

date_fields <- c(
  "SampleStartYear", "SampleEndYear", "pubYear", "between_length"
)
date_comparison <- rbindlist(lapply(names(date_libraries), function(dataset) {
  data <- copy(date_libraries[[dataset]])
  data[, between_length := pubYear - SampleEndYear]
  rows <- rbindlist(lapply(date_fields, function(field) {
    summarize_field(data, dataset, field)
  }))
  rows[, `:=`(
    total_factors = nrow(data),
    published_factors = sum(!is.na(data$pubYear))
  )]
}))
fwrite(date_comparison, file.path(out_dir, "jkp-cz-full-date-comparison.csv"))

cat("\n========== 5. Date metadata ==========\n")
print(date_comparison, digits = 4)

# =========================================================================

for (saved in c(
  "mp-regressions.csv", "jkp-regressions.csv", "jkp-diagnostics.csv",
  "jkp-cz-crosswalk.csv", "jkp-cz-matched-pairs.csv",
  "cz-alternative-specs.csv", "jkp-alternative-specs.csv",
  "jkp-cz-full-date-comparison.csv"
)) {
  message("Saved ", file.path(out_dir, saved))
}
message("Saved ", file.path(raw_dir, "download-manifest.csv"))
