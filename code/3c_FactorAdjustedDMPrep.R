# Prepare the sample-specific factor-adjusted DM benchmarks.
#
# How to run: normally run through 3_Precompute.R after
#   3a_PrepDMBenchmarks.R. For validation, set FACTOR_DM_OUT_DIR to a temporary
#   directory.
# Inputs:  dmcomp_sumstats.RDS, raw_dm_benchmarks.RDS, the versioned mined
#          long-short universe, cleaned published returns, and FF factors
# Outputs: factor_adjusted_dm_benchmarks.RDS
#
# Sample-specific CAPM/FF4 begins from the exact broad accounting |t| > 2 pair
# universe used by the raw benchmark; no published mean-return or t-stat
# matching is applied. The alpha-t screen on both sides uses the OLS intercept
# t-statistic (see the note above factor_fit()).

rm(list = ls())
source("0_Environment.R")
# Factor-adjustment implementation ---------------------------------------
#
# An alpha is an OLS intercept, so its standard error is
# s * sqrt((X'X)^-1_11) with s^2 = RSS/(n - p). Reading the standard error off
# the abnormal-return series instead -- mean(abnormal)/sd(abnormal)*sqrt(n) --
# treats the estimated factor loadings as known and ignores the degrees of
# freedom they consume, which understates the standard error by roughly 2%
# under the CAPM and 10% under FF4, and so admits ratios and predictors that
# the alpha-t screen should reject. Both sides screen on the intercept
# t-statistic below.

# Fit one factor model to one return series. Returns the slopes, the intercept,
# and the intercept's OLS standard error.
factor_fit <- function(ret, factors, minimum_observations = 60L) {
  factors <- as.matrix(factors)
  n_factor <- ncol(factors)
  observed <- stats::complete.cases(ret, factors)
  n_observed <- sum(observed)
  unfitted <- list(
    slopes = rep(NA_real_, n_factor), alpha = NA_real_,
    alpha_se = NA_real_, nobs = n_observed
  )
  if (n_observed < minimum_observations) return(unfitted)
  fit <- stats::lm(ret[observed] ~ factors[observed, , drop = FALSE])
  estimates <- summary(fit)$coefficients
  # A rank-deficient design drops rows from the coefficient table.
  if (nrow(estimates) != n_factor + 1L) return(unfitted)
  list(
    slopes = unname(stats::coef(fit))[-1L],
    alpha = unname(estimates[1L, 1L]),
    alpha_se = unname(estimates[1L, 2L]),
    nobs = n_observed
  )
}

factor_model_fits <- function(y, factors, minimum_observations = 60L) {
  y <- as.matrix(y)
  factors <- as.matrix(factors)
  if (nrow(y) != nrow(factors)) {
    stop("Return and factor matrices must have the same number of rows.")
  }
  factor_complete <- stats::complete.cases(factors)
  y <- y[factor_complete, , drop = FALSE]
  factors <- factors[factor_complete, , drop = FALSE]

  n_series <- ncol(y)
  n_factor <- ncol(factors)
  slopes <- matrix(NA_real_, nrow = n_series, ncol = n_factor)
  colnames(slopes) <- colnames(factors)
  alpha <- rep(NA_real_, n_series)
  alpha_se <- rep(NA_real_, n_series)
  if (nrow(y) == 0L || n_series == 0L) {
    return(list(slopes = slopes, alpha = alpha, alpha_se = alpha_se,
                nobs = integer(n_series)))
  }

  observed <- !is.na(y)
  y_zero <- y
  y_zero[!observed] <- 0
  design <- cbind(`(Intercept)` = 1, factors)
  p <- ncol(design)
  nobs <- colSums(observed)
  xty <- crossprod(design, y_zero)
  yty <- colSums(y_zero * y_zero)

  # Each return series may have a different history.  Compute its small X'X
  # from vectorized sufficient statistics, then solve only the p-by-p system.
  xtx_terms <- vector("list", p * (p + 1L) / 2L)
  term <- 0L
  for (a in seq_len(p)) {
    for (b in a:p) {
      term <- term + 1L
      xtx_terms[[term]] <- as.numeric(crossprod(
        design[, a] * design[, b], observed
      ))
    }
  }

  eligible <- which(nobs >= minimum_observations)
  for (j in eligible) {
    xtx <- matrix(0, p, p)
    term <- 0L
    for (a in seq_len(p)) {
      for (b in a:p) {
        term <- term + 1L
        xtx[a, b] <- xtx_terms[[term]][j]
        xtx[b, a] <- xtx[a, b]
      }
    }
    xtx_inverse <- tryCatch(solve(xtx), error = function(e) NULL)
    if (is.null(xtx_inverse)) next
    coef <- drop(xtx_inverse %*% xty[, j])
    slopes[j, ] <- coef[-1L]
    alpha[j] <- coef[1L]
    # At the OLS solution the residual sum of squares is y'y - coef'X'y.
    residual_ss <- max(yty[j] - sum(coef * xty[, j]), 0)
    degrees_freedom <- nobs[j] - p
    if (degrees_freedom > 0L) {
      alpha_se[j] <- sqrt(residual_ss / degrees_freedom * xtx_inverse[1L, 1L])
    }
  }
  list(slopes = slopes, alpha = alpha, alpha_se = alpha_se,
       nobs = as.integer(nobs))
}

factor_abnormal_returns <- function(y, factors, slopes) {
  y <- as.matrix(y)
  factors <- as.matrix(factors)
  slopes <- as.matrix(slopes)
  if (nrow(y) != nrow(factors) || ncol(y) != nrow(slopes) ||
      ncol(factors) != ncol(slopes)) {
    stop("Nonconformable return, factor, and slope matrices.")
  }
  abnormal <- y - factors %*% t(slopes)
  abnormal[is.na(y)] <- NA_real_
  abnormal
}

# Alpha statistics for a matrix of abnormal returns, paired with the standard
# errors of the fit that produced it. `alpha_sd` describes the dispersion of
# the abnormal returns; `alpha_se` is what the t-statistic divides by.
factor_alpha_stats <- function(abnormal, alpha_se) {
  abnormal <- as.matrix(abnormal)
  alpha_se <- as.numeric(alpha_se)
  if (length(alpha_se) != ncol(abnormal)) {
    stop("One alpha standard error is required per return series.")
  }
  observed <- !is.na(abnormal)
  x <- abnormal
  x[!observed] <- 0
  n <- colSums(observed)
  total <- colSums(x)
  total_sq <- colSums(x * x)
  mean <- ifelse(n > 0L, total / n, NA_real_)
  variance <- ifelse(
    n > 1L,
    pmax((total_sq - total * total / n) / (n - 1L), 0),
    NA_real_
  )
  sd <- sqrt(variance)
  tstat <- ifelse(!is.na(alpha_se) & alpha_se > 0, mean / alpha_se, NA_real_)
  data.table::data.table(
    alpha_mean = mean, alpha_sd = sd, alpha_se = alpha_se,
    alpha_n = as.integer(n), alpha_t = tstat
  )
}

cross_join_publications <- function(publications, panel) {
  publications <- data.table::copy(data.table::as.data.table(publications))
  panel <- data.table::copy(data.table::as.data.table(panel))
  publications[, `.factor_cross_key` := 1L]
  panel[, `.factor_cross_key` := 1L]
  result <- merge(
    publications, panel, by = ".factor_cross_key",
    allow.cartesian = TRUE
  )
  result[, `.factor_cross_key` := NULL]
  result
}

load_selected_dm_return_matrix <- function(dm_path, selected_pairs) {
  selected_pairs <- data.table::as.data.table(selected_pairs)
  selected_keys <- unique(selected_pairs[, .(
    sweight = tolower(sweight), dmname
  )])
  data.table::setorder(selected_keys, sweight, dmname)
  selected_keys[, column := .I]

  message("Loading mined returns and constructing the selected-strategy matrix...")
  mined <- readRDS(dm_path)
  returns <- data.table::as.data.table(mined$ret)
  weights <- data.table::as.data.table(mined$port_list)[, .(
    portid, sweight = tolower(sweight)
  )]
  rm(mined)
  returns <- weights[returns, on = "portid", nomatch = 0L]
  returns <- selected_keys[
    returns,
    on = c("sweight", "dmname" = "signalid"),
    nomatch = 0L
  ]

  dates <- sort(unique(as.numeric(returns$yearm)))
  returns[, row := match(as.numeric(yearm), dates)]
  matrix_returns <- matrix(
    NA_real_, nrow = length(dates), ncol = nrow(selected_keys)
  )
  matrix_returns[cbind(returns$row, returns$column)] <- returns$ret
  colnames(matrix_returns) <- paste(
    selected_keys$sweight, selected_keys$dmname, sep = "::"
  )
  rm(returns)
  gc()
  list(dates = dates, returns = matrix_returns, keys = selected_keys)
}

aggregate_normalized_abnormal <- function(
    y, factor_matrix, slopes_is, slopes_oos, is_rows, oos_rows,
    alpha_stats, alpha_threshold = 2, denominator_tolerance = 1e-10) {
  eligible <- !is.na(alpha_stats$alpha_t) &
    alpha_stats$alpha_t > alpha_threshold &
    !is.na(alpha_stats$alpha_mean) &
    abs(alpha_stats$alpha_mean) > denominator_tolerance
  result <- data.table::data.table(
    row = seq_len(nrow(y)), dm_return = NA_real_,
    n_pairs_available = 0L,
    n_eligible_pairs = sum(eligible)
  )
  if (!any(eligible)) return(result)

  abnormal <- matrix(NA_real_, nrow = nrow(y), ncol = sum(eligible))
  if (any(is_rows)) {
    abnormal[is_rows, ] <- factor_abnormal_returns(
      y[is_rows, eligible, drop = FALSE],
      factor_matrix[is_rows, , drop = FALSE],
      slopes_is[eligible, , drop = FALSE]
    )
  }
  if (any(oos_rows)) {
    abnormal[oos_rows, ] <- factor_abnormal_returns(
      y[oos_rows, eligible, drop = FALSE],
      factor_matrix[oos_rows, , drop = FALSE],
      slopes_oos[eligible, , drop = FALSE]
    )
  }
  normalized <- sweep(
    abnormal, 2L, alpha_stats$alpha_mean[eligible], "/"
  ) * 100
  available <- rowSums(!is.na(normalized))
  averaged <- rowMeans(normalized, na.rm = TRUE)
  averaged[available == 0L] <- NA_real_
  result[, `:=`(
    dm_return = averaged,
    n_pairs_available = as.integer(available)
  )]
  result
}

fit_dm_window_models <- function(
    return_store, factor_data, window_pairs,
    minimum_observations = 60L, alpha_threshold = 2) {
  window_pairs <- data.table::as.data.table(window_pairs)
  # The window is read off the first row and every series is fit once, so a
  # caller that mixed windows or repeated a mined ratio would be mis-fit
  # silently rather than loudly.
  if (data.table::uniqueN(window_pairs[, .(sampstart, sampend)]) != 1L) {
    stop("fit_dm_window_models() fits one publication sample window at a time.")
  }
  if (anyDuplicated(window_pairs, by = c("sweight", "dmname"))) {
    stop("A mined ratio appears more than once in one sample window.")
  }
  data.table::setorder(window_pairs, sweight, dmname)
  key_lookup <- return_store$keys[window_pairs, on = c("sweight", "dmname")]
  if (anyNA(key_lookup$column)) {
    stop("A selected mined strategy is absent from the return matrix.")
  }
  y <- return_store$returns[, key_lookup$column, drop = FALSE]
  y <- sweep(y, 2L, window_pairs$orientation, "*")
  dates <- return_store$dates
  start <- as.numeric(window_pairs$sampstart[1L])
  end <- as.numeric(window_pairs$sampend[1L])
  is_rows <- dates >= start & dates <= end
  oos_rows <- dates > end

  factor_rows <- match(dates, as.numeric(factor_data$date))
  factor_matrix <- as.matrix(factor_data[factor_rows, .(mktrf, smb, hml, umd)])
  model_factors <- list(capm = "mktrf", ff4 = c("mktrf", "smb", "hml", "umd"))
  panels <- list()
  stats <- data.table::copy(window_pairs[, .(
    sampstart, sampend, sweight, dmname, raw_mean = rbar,
    raw_t = tstat, orientation
  )])

  for (model in names(model_factors)) {
    factor_names <- model_factors[[model]]
    f <- factor_matrix[, factor_names, drop = FALSE]
    fit_is <- factor_model_fits(
      y[is_rows, , drop = FALSE], f[is_rows, , drop = FALSE],
      minimum_observations
    )
    fit_oos <- factor_model_fits(
      y[oos_rows, , drop = FALSE], f[oos_rows, , drop = FALSE],
      minimum_observations
    )
    abnormal_is <- factor_abnormal_returns(
      y[is_rows, , drop = FALSE], f[is_rows, , drop = FALSE], fit_is$slopes
    )
    alpha <- factor_alpha_stats(abnormal_is, fit_is$alpha_se)
    # The mean in-sample abnormal return is the fitted intercept. Disagreement
    # would mean the fit and the abnormal returns covered different months.
    if (!identical(is.na(alpha$alpha_mean), is.na(fit_is$alpha)) ||
        any(abs(alpha$alpha_mean - fit_is$alpha) > 1e-8, na.rm = TRUE)) {
      stop("In-sample abnormal returns disagree with the fitted intercepts.")
    }
    stats[, paste0(model, "_", names(alpha)) := alpha]
    stats[, paste0(model, "_eligible") :=
      !is.na(alpha$alpha_t) & alpha$alpha_t > alpha_threshold]

    panel <- aggregate_normalized_abnormal(
      y, f, fit_is$slopes, fit_oos$slopes, is_rows, oos_rows,
      alpha, alpha_threshold
    )
    panel[, `:=`(
      calendarDate = dates,
      eventDate = as.integer(round(12 * (dates - end)))
    )]
    panels[[model]] <- panel[, .(
      eventDate, calendarDate, dm_return,
      n_eligible_pairs, n_pairs_available
    )]
  }
  list(panels = panels, stats = stats)
}

build_broad_factor_adjusted_dm <- function(
    selected_pairs, return_store, factors, minimum_observations = 60L,
    alpha_threshold = 2L, progress = TRUE) {
  pairs <- data.table::copy(data.table::as.data.table(selected_pairs))
  pairs[, sweight := tolower(sweight)]
  window_candidates <- unique(pairs[, .(
    sampstart, sampend, sweight, dmname, rbar, tstat, orientation
  )])
  windows <- unique(pairs[, .(sampstart, sampend)])
  data.table::setorder(windows, sampend, sampstart)
  factors <- data.table::as.data.table(factors)

  panels <- list(capm = vector("list", nrow(windows)),
                 ff4 = vector("list", nrow(windows)))
  window_stats <- vector("list", nrow(windows))
  for (i in seq_len(nrow(windows))) {
    start <- windows$sampstart[i]
    end <- windows$sampend[i]
    if (progress) {
      message("Factor-adjusting window ", i, "/", nrow(windows),
              " (", start, " to ", end, ")")
    }
    candidates <- window_candidates[sampstart == start & sampend == end]
    fitted <- fit_dm_window_models(
      return_store, factors, candidates,
      minimum_observations, alpha_threshold
    )
    publications <- unique(pairs[
      sampstart == start & sampend == end, .(pubname)
    ])
    for (model in names(panels)) {
      panels[[model]][[i]] <- cross_join_publications(
        publications, fitted$panels[[model]]
      )
    }
    window_stats[[i]] <- fitted$stats
    rm(fitted)
    gc(FALSE)
  }
  list(
    panels = lapply(panels, data.table::rbindlist, use.names = TRUE),
    window_stats = data.table::rbindlist(window_stats, use.names = TRUE)
  )
}

# Pipeline setup ----------------------------------------------------------

out_dir <- Sys.getenv("FACTOR_DM_OUT_DIR", unset = "../Data/Processed")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
cache_path <- file.path(out_dir, "factor_adjusted_dm_benchmarks.RDS")
dm_path <- paste0(
  "../Data/Processed/", globalSettings$universe$dataVersion, " LongShort.RData"
)

minimum_observations <- 60L
raw_t_threshold <- 2
alpha_t_threshold <- 2

incl_signals <- restrictInclSignals(
  restrictType = globalSettings$inclusion$restrictType,
  topT = globalSettings$inclusion$topT
)
raw_contract <- readRDS("../Data/Processed/raw_dm_benchmarks.RDS")
if (is.null(raw_contract$metadata$accounting_t2$pair_fingerprint_sha256)) {
  stop(
    "raw_dm_benchmarks.RDS lacks the canonical accounting-t2 fingerprint. ",
    "Rerun 3a_PrepDMBenchmarks.R before factor adjustment."
  )
}
dm_summary <- readRDS("../Data/Processed/dmcomp_sumstats.RDS")$insampsum
base_pairs <- select_accounting_t2_pairs(
  dm_summary,
  t_threshold = raw_t_threshold,
  pubnames = raw_contract$published$pubname
)
base_fingerprint <- accounting_t2_pair_fingerprint(base_pairs)
stopifnot(
  identical(
    base_fingerprint,
    raw_contract$metadata$accounting_t2$pair_fingerprint_sha256
  ),
  nrow(base_pairs) == raw_contract$metadata$accounting_t2$pair_count
)

factors <- readRDS("../Data/Raw/FamaFrenchFactors.RData") %>%
  transmute(
    date = yearm, mktrf = mktrf, smb = smb, hml = hml, umd = umd
  ) %>%
  setDT()

# Published side -----------------------------------------------------------

czret <- readRDS("../Data/Processed/czret_keeponly.RDS") %>%
  filter(signalname %in% incl_signals) %>%
  left_join(as_tibble(factors), by = "date") %>%
  setDT()
setorder(czret, signalname, eventDate)

published_raw <- czret[
  date >= sampstart & date <= sampend,
  .(
    rbar_t = {
      n <- sum(!is.na(ret)); m <- mean(ret, na.rm = TRUE); s <- sd(ret, na.rm = TRUE)
      if (n > 1L && s > 0) m / s * sqrt(n) else NA_real_
    }
  ),
  by = signalname
]

capm_is <- czret[
  date >= sampstart & date <= sampend,
  {
    z <- factor_fit(ret, cbind(mktrf), minimum_observations)
    .(beta_capm_is = z$slopes[1L], abar_capm_tv_se = z$alpha_se)
  }, by = signalname
]
capm_oos <- czret[
  date > sampend,
  .(beta_capm_oos = factor_fit(
    ret, cbind(mktrf), minimum_observations)$slopes[1L]),
  by = signalname
]
czret <- merge(czret, capm_is, by = "signalname", all.x = TRUE)
czret <- merge(czret, capm_oos, by = "signalname", all.x = TRUE)
czret[, beta_capm_tv := fcase(
  date >= sampstart & date <= sampend, beta_capm_is,
  date > sampend, beta_capm_oos,
  default = NA_real_
)]
czret[, abnormal_capm_tv := ret - beta_capm_tv * mktrf]
capm_alpha <- czret[
  date >= sampstart & date <= sampend,
  .(abar_capm_tv = mean(abnormal_capm_tv, na.rm = TRUE)), by = signalname
]
capm_alpha[capm_is, on = "signalname", abar_capm_tv_t := fifelse(
  !is.na(i.abar_capm_tv_se) & i.abar_capm_tv_se > 0,
  abar_capm_tv / i.abar_capm_tv_se, NA_real_
)]
czret <- merge(czret, capm_alpha, by = "signalname", all.x = TRUE)
czret[, abnormal_capm_tv_normalized := fifelse(
  abs(abar_capm_tv) > 1e-10,
  100 * abnormal_capm_tv / abar_capm_tv,
  NA_real_
)]

ff4_is <- czret[
  date >= sampstart & date <= sampend,
  {
    z <- factor_fit(ret, cbind(mktrf, smb, hml, umd), minimum_observations)
    .(beta_ff4_is = z$slopes[1L], s_ff4_is = z$slopes[2L],
      h_ff4_is = z$slopes[3L], u_ff4_is = z$slopes[4L],
      abar_ff4_tv_se = z$alpha_se)
  }, by = signalname
]
ff4_oos <- czret[
  date > sampend,
  {
    z <- factor_fit(ret, cbind(mktrf, smb, hml, umd), minimum_observations)
    .(beta_ff4_oos = z$slopes[1L], s_ff4_oos = z$slopes[2L],
      h_ff4_oos = z$slopes[3L], u_ff4_oos = z$slopes[4L])
  }, by = signalname
]
czret <- merge(czret, ff4_is, by = "signalname", all.x = TRUE)
czret <- merge(czret, ff4_oos, by = "signalname", all.x = TRUE)
for (coefficient in c("beta", "s", "h", "u")) {
  target <- paste0(coefficient, "_ff4_tv")
  czret[, (target) := fcase(
    date >= sampstart & date <= sampend,
    get(paste0(coefficient, "_ff4_is")),
    date > sampend,
    get(paste0(coefficient, "_ff4_oos")),
    default = NA_real_
  )]
}
czret[, abnormal_ff4_tv := ret - (
  beta_ff4_tv * mktrf + s_ff4_tv * smb +
    h_ff4_tv * hml + u_ff4_tv * umd
)]
ff4_alpha <- czret[
  date >= sampstart & date <= sampend,
  .(abar_ff4_tv = mean(abnormal_ff4_tv, na.rm = TRUE)), by = signalname
]
ff4_alpha[ff4_is, on = "signalname", abar_ff4_tv_t := fifelse(
  !is.na(i.abar_ff4_tv_se) & i.abar_ff4_tv_se > 0,
  abar_ff4_tv / i.abar_ff4_tv_se, NA_real_
)]
czret <- merge(czret, ff4_alpha, by = "signalname", all.x = TRUE)
czret[, abnormal_ff4_tv_normalized := fifelse(
  abs(abar_ff4_tv) > 1e-10,
  100 * abnormal_ff4_tv / abar_ff4_tv,
  NA_real_
)]

published_stats <- Reduce(
  function(x, y) merge(x, y, by = "signalname", all = TRUE),
  list(
    published_raw, capm_is, capm_oos, capm_alpha,
    ff4_is, ff4_oos, ff4_alpha
  )
)
published_stats[, `:=`(
  eligible_raw_t2 = !is.na(rbar_t) & rbar_t > raw_t_threshold,
  eligible_capm_t2 = !is.na(rbar_t) & rbar_t > raw_t_threshold &
    !is.na(abar_capm_tv_t) & abar_capm_tv_t > alpha_t_threshold,
  eligible_ff4_t2 = !is.na(rbar_t) & rbar_t > raw_t_threshold &
    !is.na(abar_ff4_tv_t) & abar_ff4_tv_t > alpha_t_threshold
)]

# Data-mined side ----------------------------------------------------------

return_store <- load_selected_dm_return_matrix(dm_path, base_pairs)
dm_result <- build_broad_factor_adjusted_dm(
  base_pairs, return_store, factors,
  minimum_observations = minimum_observations,
  alpha_threshold = alpha_t_threshold
)

build_model_contract <- function(model) {
  eligible_col <- paste0("eligible_", model, "_t2")
  published_col <- paste0("abnormal_", model, "_tv_normalized")
  eligible_signals <- published_stats[get(eligible_col), signalname]
  published_panel <- czret[
    signalname %in% eligible_signals & date >= sampstart,
    .(
      pubname = signalname, eventDate, calendarDate = date,
      published_return = get(published_col)
    )
  ]
  dm_panel <- dm_result$panels[[model]][!is.na(dm_return)]
  dm_panel[, calendarDate := NULL]
  paired <- merge(
    published_panel, dm_panel,
    by = c("pubname", "eventDate"), all = FALSE
  )
  setorder(paired, pubname, eventDate)
  list(
    panel = as_tibble(paired),
    published_panel = as_tibble(published_panel),
    eligible_published_signals = sort(eligible_signals)
  )
}

capm <- build_model_contract("capm")
ff4 <- build_model_contract("ff4")

window_diagnostics <- dm_result$window_stats[, .(
  base_candidates = .N,
  capm_eligible_candidates = sum(capm_eligible, na.rm = TRUE),
  ff4_eligible_candidates = sum(ff4_eligible, na.rm = TRUE)
), by = .(sampstart, sampend)]
publication_windows <- unique(base_pairs[, .(pubname, sampstart, sampend)])
model_pair_counts <- publication_windows[
  window_diagnostics,
  on = c("sampstart", "sampend"), allow.cartesian = TRUE
][, c(
  capm = sum(capm_eligible_candidates),
  ff4 = sum(ff4_eligible_candidates)
)]

result <- list(
  capm = capm,
  ff4 = ff4,
  published_stats = as_tibble(published_stats),
  window_diagnostics = as_tibble(window_diagnostics),
  metadata = list(
    schema_version = 3L,
    base_universe = "accounting_t2",
    base_pair_count = nrow(base_pairs),
    base_predictor_count = uniqueN(base_pairs$pubname),
    base_sample_window_count = uniqueN(base_pairs[, .(sampstart, sampend)]),
    base_pair_fingerprint_sha256 = base_fingerprint,
    coefficient_regimes = c("original sample", "post-sample"),
    minimum_factor_observations = minimum_observations,
    raw_t_threshold = raw_t_threshold,
    alpha_t_threshold = alpha_t_threshold,
    normalization = "each series by its own original-sample alpha mean",
    alpha_t_statistic = "OLS intercept estimate over its OLS standard error",
    factor_models = list(
      capm = "Mkt-RF",
      ff4 = c("Mkt-RF", "SMB", "HML", "UMD")
    ),
    model_counts = list(
      capm = c(
        predictors = n_distinct(capm$panel$pubname),
        eligible_pairs = model_pair_counts[["capm"]]
      ),
      ff4 = c(
        predictors = n_distinct(ff4$panel$pubname),
        eligible_pairs = model_pair_counts[["ff4"]]
      )
    ),
    input_files = c(
      "../Data/Processed/dmcomp_sumstats.RDS",
      "../Data/Processed/raw_dm_benchmarks.RDS",
      dm_path,
      "../Data/Processed/czret_keeponly.RDS",
      "../Data/Raw/FamaFrenchFactors.RData"
    )
  )
)

stopifnot(
  !anyDuplicated(as.data.frame(result$capm$panel)[c("pubname", "eventDate")]),
  !anyDuplicated(as.data.frame(result$ff4$panel)[c("pubname", "eventDate")]),
  identical(
    result$metadata$base_pair_fingerprint_sha256,
    raw_contract$metadata$accounting_t2$pair_fingerprint_sha256
  )
)
saveRDS(result, cache_path)
message("Wrote ", cache_path)
rm(return_store)
