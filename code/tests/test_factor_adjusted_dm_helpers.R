# Test window-batched factor-adjustment helpers against direct regressions.
#
# How to run: from flex-mining/, run
#   Rscript tests/test_factor_adjusted_dm_helpers.R
# Inputs: the embedded factor-adjustment functions in 3c_FactorAdjustedDMPrep.R
# Outputs: none; exits nonzero on failure.

library(data.table)
required_functions <- c(
  "factor_fit", "factor_model_fits", "factor_abnormal_returns",
  "factor_alpha_stats"
)
for (expression in parse("3c_FactorAdjustedDMPrep.R")) {
  if (is.call(expression) &&
      as.character(expression[[1]]) %in% c("<-", "=") &&
      is.symbol(expression[[2]]) &&
      as.character(expression[[2]]) %in% required_functions) {
    eval(expression, envir = globalenv())
  }
}
stopifnot(all(vapply(required_functions, exists, logical(1), mode = "function")))

set.seed(418)
n <- 180L
factors <- cbind(
  mktrf = rnorm(n), smb = rnorm(n), hml = rnorm(n), umd = rnorm(n)
)
slopes <- rbind(
  c(0.7, -0.2, 0.1, 0.3),
  c(-0.4, 0.5, 0.2, -0.1),
  c(1.2, 0.1, -0.3, 0.4)
)
y <- factors %*% t(slopes) + matrix(rnorm(n * 3L, sd = 0.7), ncol = 3L)
y[1:25, 2] <- NA_real_
y[seq(4, n, by = 13), 3] <- NA_real_

# Single-series fits: slopes, intercept, and the intercept's standard error.
capm_direct <- summary(lm(y[, 1] ~ factors[, "mktrf"]))$coefficients
capm_fit <- factor_fit(y[, 1], factors[, "mktrf", drop = FALSE])
ff4_direct <- summary(lm(y[, 3] ~ factors))$coefficients
ff4_fit <- factor_fit(y[, 3], factors)
stopifnot(
  isTRUE(all.equal(capm_fit$slopes, unname(capm_direct[2L, 1L]))),
  isTRUE(all.equal(capm_fit$alpha, unname(capm_direct[1L, 1L]))),
  isTRUE(all.equal(capm_fit$alpha_se, unname(capm_direct[1L, 2L]))),
  isTRUE(all.equal(ff4_fit$slopes, unname(ff4_direct[2:5, 1L]))),
  isTRUE(all.equal(ff4_fit$alpha_se, unname(ff4_direct[1L, 2L]))),
  ff4_fit$nobs == sum(!is.na(y[, 3])),
  is.na(factor_fit(y[1:59, 1], factors[1:59, "mktrf", drop = FALSE])$slopes)
)

fitted <- factor_model_fits(y, factors, minimum_observations = 60L)
direct <- lapply(seq_len(ncol(y)), function(j) {
  complete <- complete.cases(y[, j], factors)
  summary(lm(y[complete, j] ~ factors[complete, ]))$coefficients
})
stopifnot(
  isTRUE(all.equal(
    fitted$slopes, t(vapply(direct, function(x) x[-1L, 1L], numeric(4L))),
    tolerance = 1e-10, check.attributes = FALSE
  )),
  isTRUE(all.equal(
    fitted$alpha, vapply(direct, function(x) x[1L, 1L], numeric(1L)),
    tolerance = 1e-10
  )),
  isTRUE(all.equal(
    fitted$alpha_se, vapply(direct, function(x) x[1L, 2L], numeric(1L)),
    tolerance = 1e-10
  ))
)

# The alpha t-statistic must be the OLS intercept t-statistic, not the
# abnormal-return series mean over its own standard deviation.
abnormal <- factor_abnormal_returns(y, factors, fitted$slopes)
stats <- factor_alpha_stats(abnormal, fitted$alpha_se)
direct_stats <- rbindlist(lapply(seq_len(ncol(y)), function(j) {
  x <- abnormal[, j]
  data.table(
    alpha_mean = mean(x, na.rm = TRUE),
    alpha_sd = sd(x, na.rm = TRUE),
    alpha_se = direct[[j]][1L, 2L],
    alpha_n = sum(!is.na(x)),
    alpha_t = direct[[j]][1L, 3L]
  )
}))
stopifnot(isTRUE(all.equal(
  stats, direct_stats, tolerance = 1e-10, check.attributes = FALSE
)))
# The superseded formula is close enough to look right and different enough to
# move the t > 2 screen, so guard against a silent revert.
naive_t <- stats$alpha_mean / stats$alpha_sd * sqrt(stats$alpha_n)
stopifnot(all(naive_t > stats$alpha_t), max(naive_t / stats$alpha_t) > 1.01)

stopifnot(inherits(
  try(factor_alpha_stats(abnormal, fitted$alpha_se[-1L]), silent = TRUE),
  "try-error"
))

# Fewer than 60 observations must not produce coefficients.
short <- y
short[60:n, 1] <- NA_real_
short_fit <- factor_model_fits(short, factors, minimum_observations = 60L)
stopifnot(
  all(is.na(short_fit$slopes[1, ])), short_fit$nobs[1] == 59L,
  is.na(short_fit$alpha[1]), is.na(short_fit$alpha_se[1])
)

message("Window-batched factor-adjustment helper tests passed.")
