# McLean-Pontiff (2016) style decay regressions on Chen-Zimmermann data.
#
# One script, three specifications, all driven by a single prep() pipeline:
#
#   A. mp_style_scaled     : published predictors with signed in-sample
#                            return t-statistics greater than 2, returns scaled
#                            so the in-sample mean = 100 bps/month, cumulative
#                            indicators, predictor FE +/- month FE.
#   B. mp_style_scaled_mp  : same, but restricted to CZ signals that appear in
#                            McLean-Pontiff, panel truncated to MP's sample
#                            (ends 2013).
#   C. mp_match_unscaled   : the closest match to MP Table II col (1) -- MP
#                            subset, panel through 2013, UNSCALED returns in
#                            percent/month (MP's units), with publication-date
#                            variants (Dec/Jun/Jan of the pub year) and an
#                            alternative presentation scaled by the grand-mean
#                            in-sample return (MP's 0.652 analog).
#
# All regressions use cumulative indicators, predictor FE, and SEs two-way
# clustered by predictor and month, matching the original pyfixest scripts.
#
# Interpretation: MP and CZ agree closely on total post-publication decay
# without month fixed effects. With month fixed effects, CZ continues to show
# post-sample decay but little additional change at publication. MP's own
# additional time-FE publication effect is smaller than its baseline effect,
# and its standard error is not recoverable from the published table.
#
# Normally run through: Rscript Appendices/SA15_TimeFERobustness/run.R build

suppressPackageStartupMessages({
  library(data.table)
  library(fixest)
})

pdf(NULL)
source("Appendices/SA15_TimeFERobustness/R/config.R")

# ---- config ---------------------------------------------------------------

RELEASE  <- TIMEFE_RELEASE
DATA_DIR <- TIMEFE_OPENAP_RAW_DIR
PORT_CSV <- TIMEFE_OP_CSV
DOC_CSV  <- TIMEFE_SIGNAL_DOC_CSV
META_CSV <- TIMEFE_META_REPLICATIONS_CSV
OUT_DIR  <- TIMEFE_OUTPUT_DIR
OUT_CSV  <- file.path(OUT_DIR, "mp-regressions.csv")
dir.create(OUT_DIR, recursive = TRUE, showWarnings = FALSE)

# CZ renamed IdioRisk (Ang et al. 2006) to IdioVol3F after the MP mapping was
# written; patch the mapping so the join finds it.
NAME_PATCH <- c(IdioRisk = "IdioVol3F")

MP_BENCH <- paste(
  "MP Table II col (1):  between -0.150 (0.077),",
  "additional -0.187, total -0.337 (0.090)"
)
MP_TIME_BENCH <- paste(
  "MP Table III col (4), with month FE: between -0.179 (0.080),",
  "additional -0.131 (SE unavailable), total -0.310 (0.122)"
)

# ---- data: download once if missing ---------------------------------------

ensure_data <- function() {
  required <- c(PORT_CSV, DOC_CSV, META_CSV)
  if (all(file.exists(required))) return(invisible())
  stop(
    "Missing pinned CZ input(s): ",
    paste(required[!file.exists(required)], collapse = ", "),
    ". Run `Rscript Appendices/SA15_TimeFERobustness/run.R acquire` first.",
    call. = FALSE
  )
}
ensure_data()

# ---- shared prep pipeline -------------------------------------------------

mp_names <- function() {
  meta <- fread(META_CSV, encoding = "UTF-8")
  meta[, ourname := fifelse(ourname %in% names(NAME_PATCH),
                            NAME_PATCH[ourname], ourname)]
  unique(na.omit(meta[metastudy == "MP", ourname]))
}

# Build the regression panel.
#   mp_only   : restrict to CZ signals matched to an MP predictor.
#   end_year  : drop months after this calendar year (NULL = keep all).
#   pub_month : publication cutoff. NULL => postPubC turns on the year AFTER
#               pubYear; an integer 1..12 => turns on after month-end of that
#               month in pubYear (MP-style date shift).
# Every CZ specification keeps only predictors with a signed in-sample return
# t-statistic greater than 2. The scaled argument determines whether retained
# predictors' returns are scaled to an in-sample mean of 100.
prep <- function(mp_only = FALSE, end_year = NULL, pub_month = NULL,
                 scaled = TRUE) {
  ret <- fread(PORT_CSV)
  ret <- ret[port == "LS"]
  ret[, date := as.IDate(date)]

  doc <- fread(DOC_CSV)
  doc <- doc[`Cat.Signal` == "Predictor",
             .(signalname = Acronym, SampleStartYear, SampleEndYear,
               pubYear = Year)]
  if (mp_only) doc <- doc[signalname %in% mp_names()]

  df <- merge(ret, doc, by = "signalname")
  df[, yr := year(date)]
  if (!is.null(end_year)) df <- df[yr <= end_year]
  df <- df[yr >= SampleStartYear]

  df[, postSampC := as.integer(yr > SampleEndYear)]
  if (is.null(pub_month)) {
    df[, postPubC := as.integer(yr > pubYear)]
  } else {
    # month-end of (pubYear, pub_month) = day before the first of next month
    nxt <- as.IDate(paste(df$pubYear + (pub_month == 12L),
                          ifelse(pub_month == 12L, 1L, pub_month + 1L),
                          "01", sep = "-"))
    df[, postPubC := as.integer(date > nxt - 1L)]
  }

  df[, yyyymm := yr * 100L + month(date)]

  quality <- df[postSampC == 0L, .(
    in_sample_mean_pct = mean(ret),
    in_sample_tstat = mean(ret) / (sd(ret) / sqrt(.N))
  ), by = signalname]
  keep <- quality[
    is.finite(in_sample_tstat) & in_sample_tstat > 2,
    signalname
  ]
  df <- merge(df[signalname %in% keep], quality, by = "signalname")

  if (scaled) {
    df[, retScaled := 100 * ret / in_sample_mean_pct]
  }
  df[]
}

# All models: SEs two-way clustered by predictor and month, and singleton
# fixed effects dropped so results line up with the original pyfixest scripts
# (pyfixest drops singletons by default; base fixest does not).
est <- function(fml, data) {
  feols(as.formula(fml), data, cluster = ~ signalname + yyyymm,
        fixef.rm = "singleton")
}

result_row <- function(fit, specification, data_source, units, fixed_effects,
                       data) {
  b <- coef(fit)
  v <- vcov(fit)
  total <- b[["postSampC"]] + b[["postPubC"]]
  min_in_sample_mean_bps <- 100 * min(
    data[postSampC == 0L, .(mean_return_pct = mean(ret)), by = signalname][
      , mean_return_pct
    ]
  )
  data.table(
    specification, data_source, units, fixed_effects,
    post_sample = b[["postSampC"]],
    post_sample_se = sqrt(v["postSampC", "postSampC"]),
    additional_post_publication = b[["postPubC"]],
    additional_post_publication_se = sqrt(v["postPubC", "postPubC"]),
    total_post_publication_change = total,
    total_post_publication_change_se = sqrt(sum(v)),
    min_in_sample_mean_bps,
    observations = fit$nobs,
    factors = uniqueN(data$signalname),
    singleton_observations_removed = nrow(data) - fit$nobs
  )
}

mp_results <- list()

# ===========================================================================
# A. quality-screened published predictors, scaled, full sample
# ===========================================================================
cat("\n========== A. mp_style_scaled (signed t>2, scaled) ==========\n")
dfA <- prep(mp_only = FALSE, scaled = TRUE)
cat(sprintf("predictors: %d, obs: %d\n",
            uniqueN(dfA$signalname), nrow(dfA)))
mA1 <- est("retScaled ~ postSampC + postPubC | signalname", dfA)
mA2 <- est("retScaled ~ postSampC + postPubC | signalname + yyyymm", dfA)
print(etable(mA1, mA2))
mp_results[["cz_all_pred"]] <- result_row(
  mA1, "cz_all_scaled", "CZ", "in-sample mean = 100", "predictor", dfA
)
mp_results[["cz_all_time"]] <- result_row(
  mA2, "cz_all_scaled", "CZ", "in-sample mean = 100",
  "predictor + month", dfA
)

# ===========================================================================
# B. MP-matched subset, scaled, panel through 2013
# ===========================================================================
cat("\n========== B. mp_style_scaled_mp (MP subset, signed t>2, scaled, <=2013) ==========\n")
dfB <- prep(mp_only = TRUE, end_year = 2013, scaled = TRUE)
cat(sprintf("MP-matched predictors in panel: %d, obs: %d\n",
            uniqueN(dfB$signalname), nrow(dfB)))
mB1 <- est("retScaled ~ postSampC + postPubC | signalname", dfB)
mB2 <- est("retScaled ~ postSampC + postPubC | signalname + yyyymm", dfB)
print(etable(mB1, mB2))
mp_results[["cz_mp_scaled_pred"]] <- result_row(
  mB1, "cz_mp_matched_2013_scaled", "CZ", "in-sample mean = 100",
  "predictor", dfB
)
mp_results[["cz_mp_scaled_time"]] <- result_row(
  mB2, "cz_mp_matched_2013_scaled", "CZ", "in-sample mean = 100",
  "predictor + month", dfB
)

# ===========================================================================
# C. MP-matched subset, unscaled, <=2013, publication-date variants
# ===========================================================================
cat("\n========== C. mp_match_unscaled (MP subset, signed t>2, unscaled, pub-date variants) ==========\n")

pub_months <- c(Dec = 12L, Jun = 6L, Jan = 1L)
fe_specs   <- c(`pred FE` = "signalname", `+time FE` = "signalname + yyyymm")

# grand-mean in-sample return (MP's 0.652 analog): average of per-predictor
# in-sample means, computed on the unscaled MP panel.
dfC0 <- prep(mp_only = TRUE, end_year = 2013, scaled = FALSE)
grand_mean <- mean(dfC0[postSampC == 0, .(m = mean(ret)), by = signalname]$m)
cat(sprintf("predictors: %d, obs: %d\n", uniqueN(dfC0$signalname), nrow(dfC0)))
cat(sprintf("grand mean in-sample return: %.3f %%/mo (MP: 0.652)\n\n", grand_mean))

specs <- list()
for (pm in names(pub_months)) {
  dfC <- prep(mp_only = TRUE, end_year = 2013, pub_month = pub_months[[pm]],
              scaled = FALSE)
  for (fe in names(fe_specs)) {
    fit <- est(paste("ret ~ postSampC + postPubC |", fe_specs[[fe]]), dfC)
    b <- coef(fit); s <- se(fit)
    specs[[paste0(pm, ", ", fe)]] <- list(
      bS = b[["postSampC"]], seS = s[["postSampC"]],
      bP = b[["postPubC"]],  seP = s[["postPubC"]],
      tot = b[["postSampC"]] + b[["postPubC"]], n = fit$nobs
    )
    result <- result_row(
      fit, paste0("cz_mp_matched_2013_unscaled_pub_", tolower(pm)),
      "CZ", "percent/month", fe, dfC
    )
    result[, normalization_mean_pct := grand_mean]
    mp_results[[paste0("cz_mp_unscaled_", pm, "_", fe)]] <- result
  }
}

labs <- names(specs)
row  <- function(title, f) cat(sprintf("%-38s", title),
                               vapply(specs, f, character(1)), "\n")
num  <- function(key) function(x) sprintf("%16.3f", x[[key]])
par  <- function(key) function(x) sprintf("%16s", sprintf("(%.3f)", x[[key]]))

cat(sprintf("%-38s", ""), sprintf("%16s", labs), "\n")
row("Post-Sample, cumulative",       num("bS"))
row("",                              par("seS"))
row("Additional Post-Pub effect",    num("bP"))
row("",                              par("seP"))
row("Total post-publication change", num("tot"))
row("Observations",                  function(x) sprintf("%16s",
                                        formatC(x$n, big.mark = ",", format = "d")))
cat("\n", MP_BENCH, "\n", sep = "")
cat(MP_TIME_BENCH, "\n")

cat(sprintf("\nScaled by grand mean (%.3f):\n", grand_mean))
row("Post-Sample, cumulative",       function(x) sprintf("%16.3f", x$bS / grand_mean))
row("Additional Post-Pub effect",    function(x) sprintf("%16.3f", x$bP / grand_mean))
row("Total post-publication change", function(x) sprintf("%16.3f", x$tot / grand_mean))
cat("\nMP scaled by 0.582:   between -0.258, additional -0.321, total -0.579\n")

# Add MP's published headline rows. The additional-effect SE in Table II is
# implied by MP's reported equality-test p-value; Table III does not report
# enough information to recover it.
mp_results[["mp_table_ii"]] <- data.table(
  specification = "mp_published_table_ii_col1",
  data_source = "MP published", units = "percent/month",
  normalization_mean_pct = 0.582,
  fixed_effects = "predictor", post_sample = -0.150,
  post_sample_se = 0.077, additional_post_publication = -0.187,
  additional_post_publication_se = 0.083,
  total_post_publication_change = -0.337,
  total_post_publication_change_se = 0.090,
  observations = 51851L, factors = 97L,
  singleton_observations_removed = NA_integer_
)
mp_results[["mp_table_iii_time"]] <- data.table(
  specification = "mp_published_table_iii_col4",
  data_source = "MP published", units = "percent/month",
  normalization_mean_pct = 0.582,
  fixed_effects = "predictor + month", post_sample = -0.179,
  post_sample_se = 0.080, additional_post_publication = -0.131,
  additional_post_publication_se = NA_real_,
  total_post_publication_change = -0.310,
  total_post_publication_change_se = 0.122,
  observations = 51851L, factors = 97L,
  singleton_observations_removed = NA_integer_
)

mp_result_table <- rbindlist(mp_results, use.names = TRUE, fill = TRUE)
setorder(mp_result_table, specification, fixed_effects)
fwrite(mp_result_table, OUT_CSV)

cz_time <- specs[["Dec, +time FE"]]
cat(
  "\nInterpretation of month fixed effects:\n",
  sprintf(
    paste0(
      "  CZ MP-matched: post-sample %.3f (%.3f), additional publication ",
      "%.3f (%.3f), total %.3f.\n"
    ),
    cz_time$bS, cz_time$seS, cz_time$bP, cz_time$seP, cz_time$tot
  ),
  "  MP published: post-sample -0.179 (0.080), additional publication ",
  "-0.131 (SE unavailable), total -0.310 (0.122).\n",
  "  The replicated incremental publication effect is small and imprecise; ",
  "MP's time-FE table does not establish its significance.\n",
  sep = ""
)
cat("\nSaved: ", OUT_CSV, "\n", sep = "")
