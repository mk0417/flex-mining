# Test the configuration, source integrity, and shared helpers of the time-FE
# robustness appendix.
#
# How to run: from flex-mining/, `Rscript tests/test_timefe_robustness_module.R`
# Inputs:  Appendices/SA15_TimeFERobustness source files. No pinned external
#          data is required; the helper tests run on a synthetic panel.
# Outputs: console assertion summary; no files are written.

pdf(NULL)

module_dir <- "Appendices/SA15_TimeFERobustness"
files <- list.files(module_dir, pattern = "[.]R$", full.names = TRUE)
stopifnot(
  identical(basename(files),
            c("cz_terciles.R", "download.R", "estimate.R", "exhibits.R",
              "run.R", "setup.R")),
  all(file.exists(files))
)
invisible(lapply(files, parse))

# Sourcing setup.R must not need any external data, only the packages.
temp_root <- tempfile("timefe-test-")
withr::with_envvar(
  c(TIMEFE_RAW_DIR = file.path(temp_root, "raw"),
    TIMEFE_PROCESSED_DIR = file.path(temp_root, "processed"),
    TIMEFE_RESULTS_DIR = file.path(temp_root, "results")),
  source(file.path(module_dir, "setup.R"))
)
on.exit(unlink(temp_root, recursive = TRUE), add = TRUE)

# Pins and paths ----------------------------------------------------------

stopifnot(
  identical(timefeSettings$pins$release, "2024_10"),
  nchar(timefeSettings$pins$jkp_commit) == 40L,
  nchar(timefeSettings$pins$cz_mapping_commit) == 40L,
  nchar(timefeSettings$pins$cz_mapping_sha256) == 64L,
  identical(timefeSettings$screen$t_min, 2),
  identical(timefeSettings$paths$output,
            file.path(temp_root, "processed", "output")),
  identical(timefeSettings$paths$exhibits, file.path(temp_root, "results")),
  dir.exists(timefeSettings$paths$output)
)

# Year parsing ------------------------------------------------------------

stopifnot(
  identical(parse_first_year(c("Smith 1999", NA_character_)),
            c(1999L, NA_integer_)),
  identical(parse_period(c("1963--1982", "2001 to 2010"), "start"),
            c(1963L, 2001L)),
  identical(parse_period(c("1963--1982", "2001 to 2010"), "end"),
            c(1982L, 2010L))
)

# Panel construction ------------------------------------------------------

# Two signals: "strong" has a large, reliably positive in-sample mean; "weak"
# has a mean near zero and must fail the screen.
set.seed(20260817)
months <- seq(as.IDate("1990-01-01"), as.IDate("2009-12-01"), by = "month")
synthetic <- rbind(
  data.table(signalname = "strong", date = months,
             retPct = rnorm(length(months), mean = 1, sd = 0.5)),
  data.table(signalname = "weak", date = months,
             retPct = rnorm(length(months), mean = 0.001, sd = 3))
)
# The sample and publication dates are staggered across signals, as they are
# in the real libraries; without that the indicators would be collinear with
# month fixed effects.
synthetic <- merge(
  synthetic,
  data.table(signalname = c("strong", "weak"), SampleStartYear = 1990L,
             SampleEndYear = c(1999L, 2002L), pubYear = c(2002L, 2005L)),
  by = "signalname"
)

panel <- add_event_indicators(copy(synthetic))
strong <- panel[signalname == "strong"]
stopifnot(
  # Cumulative indicators switch on the year after the sample ends and the
  # year after publication.
  strong[yr == 1999L, all(postSampC == 0L)],
  strong[yr == 2000L, all(postSampC == 1L)],
  strong[yr == 2002L, all(postPubC == 0L)],
  strong[yr == 2003L, all(postPubC == 1L)],
  # ... signal by signal, not on one common date.
  panel[signalname == "weak" & yr == 2002L, all(postSampC == 0L)],
  panel[signalname == "weak" & yr == 2005L, all(postPubC == 0L)],
  identical(panel[date == as.IDate("2003-04-01"), unique(yyyymm)], 200304L)
)

# The MP publication convention shifts the indicator within the publication
# year instead: June means postPubC turns on in July of pubYear.
june <- add_event_indicators(copy(synthetic), pub_month = 6L)
stopifnot(
  june[signalname == "strong" & date == as.IDate("2002-06-01"),
       all(postPubC == 0L)],
  june[signalname == "strong" & date == as.IDate("2002-07-01"),
       all(postPubC == 1L)]
)

# Screening and scaling ---------------------------------------------------

with_stats <- add_in_sample_stats(panel)
in_sample <- strong[yr <= 1999L, retPct]
stopifnot(
  all.equal(with_stats[signalname == "strong", unique(in_sample_mean_pct)],
            mean(in_sample)),
  all.equal(with_stats[signalname == "strong", unique(in_sample_tstat)],
            mean(in_sample) / (sd(in_sample) / sqrt(length(in_sample)))),
  identical(with_stats[signalname == "strong", unique(in_sample_months)],
            length(in_sample))
)

screened <- quality_screen(with_stats)
stopifnot(
  identical(sort(unique(screened$signalname)), "strong"),
  # The threshold is a setting, not a literal buried in a stage script.
  uniqueN(quality_screen(with_stats, t_min = -Inf)$signalname) == 2L
)

scaled <- scale_by_signal_mean(screened)
stopifnot(
  all.equal(scaled[postSampC == 0L, mean(retScaled)], 100),
  all.equal(scaled$retScaled, 100 * scaled$retPct / scaled$in_sample_mean_pct)
)

grand <- grand_mean_scale(scaled)
stopifnot(
  # One signal survives, so the grand mean is that signal's in-sample mean.
  all.equal(attr(grand, "grand_mean_pct"), mean(in_sample)),
  all.equal(grand$retGrandScaled, grand$retScaled)
)

# Estimation --------------------------------------------------------------

# Two signals are needed for clustered SEs, so estimate on the unscreened
# panel with both.
both <- scale_by_signal_mean(add_in_sample_stats(panel))
rows <- decay_rows(both, "retScaled", specification = "test", label = "unit")

fit <- fixest::feols(
  retScaled ~ postSampC + postPubC | signalname,
  data = both, cluster = ~ signalname + yyyymm, fixef.rm = "singleton",
  notes = FALSE
)
stopifnot(
  identical(names(rows)[1:2], c("specification", "label")),
  identical(rows$fixed_effects, c("predictor", "predictor + month")),
  nrow(rows) == 2L,
  all.equal(rows$post_sample[1], unname(coef(fit)["postSampC"])),
  all.equal(rows$additional_post_publication[1],
            unname(coef(fit)["postPubC"])),
  all.equal(rows$total_post_publication_change[1],
            unname(sum(coef(fit)[c("postSampC", "postPubC")]))),
  # The reported total SE accounts for the covariance of the two indicators.
  all.equal(rows$total_post_publication_change_se[1],
            sqrt(sum(vcov(fit)))),
  all.equal(rows$mean_in_sample_bps[1],
            100 * mean(unique(both[, .(signalname, in_sample_mean_pct)]
                              )$in_sample_mean_pct)),
  identical(rows$factors, c(2L, 2L)),
  is.na(rows$normalization_mean_bps[1])
)

# A grand-mean-scaled panel records its denominator for the tables to report.
grand_rows <- decay_rows(grand_mean_scale(both), "retGrandScaled",
                         specification = "test")
stopifnot(all.equal(grand_rows$normalization_mean_bps[1],
                    100 * attr(grand_mean_scale(both), "grand_mean_pct")))

message("test_timefe_robustness_module.R: all assertions passed")
