# Review of the factor-adjusted calculations

Timestamp: 2026-08-17 11:51 EDT
Branch: `check/factor-adj` (findings only; no code changed)

Audit of `3c_FactorAdjustedDMPrep.R` and its consumers, prompted by the history
of problems in this code (see `FOUND_BUGS_in_risk_adjusted_code.md` and
`0816_factor-adjusted-pipeline-fix.md`). Snapshot of the code and production
caches as inspected on the date above, not maintained documentation.

## What was verified as correct

The vectorized machinery in `3c` was checked against a naive per-strategy `lm`
loop on real data: one full sample window (Jul 1986 -- Dec 2000, 174 in-sample
and 288 post-sample months), 400 randomly drawn mined ratios from that window's
1,474 candidates, both models.

| Quantity | Max abs. difference vs. direct `lm` |
|---|---|
| `alpha_mean` (CAPM / FF4) | 4.4e-16 / 2.7e-15 |
| `alpha_t` (CAPM / FF4) | 1.3e-15 / 5.1e-15 |
| `dm_return` (normalized aggregate) | 1.1e-13 |

`alpha_n`, the NA patterns, `n_pairs_available`, the eligible-pair counts
(331 CAPM, 254 FF4), the pre-sample masking, and the event-date mapping all
matched exactly.

Also checked and clean:

- **Units.** Mined returns, published returns, and the FF factors are all in
  percent (sd 3.75, 4.32, 4.48). No 100x mismatch. Long-short returns are
  self-financing, so regressing on `mktrf` without subtracting `rf` is right.
- **Return-matrix keys.** `(sweight, signalid, yearm)` is unique across all
  35,123,882 mined return rows, so `load_selected_dm_return_matrix()`'s scatter
  into the matrix cannot silently overwrite a series.
- **Event-date convention.** The DM side's `round(12 * (yearm - sampend))`
  agrees with czret's lubridate `interval(sampend, date) %/% months(1)` on all
  127,497 published rows, so the merge key in `build_model_contract()` is sound.
- **Window candidates.** 773,106 `(window, sweight, dmname)` rows, zero
  duplicates, so the `unique(..., by = c("sweight", "dmname"))` in
  `fit_dm_window_models()` currently discards nothing.
- **Universe contract.** The saved `factor_adjusted_dm_benchmarks.RDS`
  fingerprint still equals the `raw_dm_benchmarks.RDS` accounting-t2
  fingerprint (`16ef030e...`), 1,068,052 pairs both. The August fix holds.
- **No event-time truncation.** All 147 CAPM and 132 FF4 eligible predictors
  have post-sample DM coverage, and total OOS months match the raw benchmark
  exactly (41,182). The 60-month minimum on the post-sample regression drops
  nobody.
- **Published-sample attrition** (157 raw-eligible -> 147 CAPM -> 132 FF4) is
  the alpha-t screen that `benchmark-logic.md` B1/C1 explicitly specifies, not
  an accident.

## Finding 1: the alpha t-statistic is not an alpha t-statistic

`factor_alpha_stats()` (`3c:106-126`) computes
`mean(abnormal) / sd(abnormal) * sqrt(n)`. This treats the estimated factor
loadings as known and uses n-1 rather than n-p-1 degrees of freedom, so it
understates the standard error of alpha. The published side repeats the same
formula at `3c:408-419` and `3c:457-468`.

Measured against `summary(lm)`'s intercept t on real data:

| Sample | median t_approx / t_OLS | flips at t > 2 |
|---|---:|---:|
| CAPM, 400 mined ratios | 1.017 | 6 / 400 |
| FF4, 400 mined ratios | 1.102 | 52 / 400 |
| CAPM, published signals | 1.007 | 0 / 198 |
| FF4, published signals | 1.075 | 6 / 198 |

So the FF4 mined universe is roughly 13% larger than a proper alpha test would
admit. The six published predictors admitted to the FF4 columns with a true FF4
alpha t below 2: DelEqu (1.94), DolVol (1.96), Investment (1.98),
MomOffSeason06YrPlus (1.90), MomOffSeason11YrPlus (1.94), NetPayoutYield (1.93).

`benchmark-logic.md` C1 states "in-sample FF4-alpha t > 2", which is not the
test being run. The screen is applied symmetrically to the published and mined
sides, so the comparison is not invalidated -- the disclosed screen and the
implemented screen simply differ.

The correct standard error is cheap in the existing vectorized path:
`factor_model_slopes()` already solves the per-series `xtx`, so
`(X'X)^-1[1,1]` is in hand, and RSS follows from `y'y - coef' X'y`.

## Finding 2: pre-July-1963 months drop out silently

`FamaFrenchFactors.RData` runs Jul 1963 -- Dec 2024 with no interior gaps. The
mined panel starts Jul 1951, so 144 of its 882 months have no factor row; and
5,223 published in-sample rows across 29 predictors fall before Jul 1963. Those
months are excluded from beta estimation, from the alpha, and from the
normalization denominator, with no warning emitted and nothing recorded in
`metadata`.

| Predictor | In-sample months | Months with factors |
|---|---:|---:|
| DivSeason | 1,014 | 582 |
| DivYieldST | 493 | 174 |
| Beta | 474 | 66 |
| LRreversal | 642 | 234 |
| STreversal | 642 | 294 |

For these 29 predictors the Table 6 CAPM/FF4 columns are computed over a
materially shorter in-sample window than the Raw column's `rbar`. This is the
same class of raw-versus-adjusted incomparability as the August bug, at much
smaller scale. Nothing is dropped outright: the smallest factor-available
in-sample window is Beta's 66 months, six above the 60-month minimum.

## Finding 3: `fit_dm_window_models()` trusts its caller

It reads the window from `window_pairs$sampstart[1L]` / `sampend[1L]`
(`3c:233-234`) after deduplicating by `(sweight, dmname)`. Handed pairs from
two windows it would silently mis-window every series. The current data is
clean (see above), so this is latent. A
`stopifnot(uniqueN(window_pairs[, .(sampstart, sampend)]) == 1L)` is free.

## Finding 4: reversed regressions in `SA01_RiskVsMispricingPlots.R`

A separate script, same subject matter. Its local helper

```r
extract_beta <- function(x, y) { model <- lm(y ~ x); coef(model)[2] }   # SA01:107
```

has its parameters swapped relative to every call site, all of which pass
`(return, factor)`:

- `SA01:239` `extract_beta(ret, mktrf*100)` fits `lm(mktrf ~ ret)`
- `SA01:265` `extract_beta(retOrig, mktrf)` -- same reversal
- `SA01:250` `roll_lm(ret, mktrf*100, width = 60)` -- `roll::roll_lm(x, y, width)`,
  so also market-on-strategy

The resulting loadings are attenuated by `var(mkt)/var(ret)`. For AM: -0.171 as
coded versus -0.449 intended. Everything downstream inherits it --
`abnormal_all`, `abnormal_roll`, `abnormal_all_normalized_v2`, and the
`abar_all_not_norm_t` screen behind the Tge1/Tge2/Tge3 figures, all written to
`../Results/Fig_PublicationsOverTime*`. The identically named function at
`3c:18` takes `(ret, mktrf)` and fits `lm(ret ~ mktrf)` correctly, which is what
makes SA01 look like a slip rather than intent.

The `mktrf*100` scaling at `SA01:239`, `:241`, `:250`, `:252` is harmless --
the factor rescaling cancels between the slope and the fitted value -- but it
is confusing next to the un-scaled `mktrf` at `:265`/`:267`.

## Housekeeping

`factor_adjusted_dm_benchmarks.RDS` (Aug 16 03:28) predates
`dmcomp_sumstats.RDS` and `raw_dm_benchmarks.RDS` (Aug 16 11:50 / 11:53), and
the saved raw metadata still carries the pre-`common_gates` specification field
names (`minimum_stocks_per_leg` rather than `common_gates`). The fingerprints
agree, so the universes match; the cached factor contract was nonetheless built
against an older `3a`.

## Fixes applied (same day, same branch)

Findings 1, 3, and 4 were fixed. Finding 2 (pre-1963 truncation) was left
alone.

**Finding 1.** `extract_beta()` and `extract_ff4_coeffs()` were replaced by one
`factor_fit()` that returns slopes, the intercept, and the intercept's OLS
standard error; `factor_model_slopes()` became `factor_model_fits()` and now
also returns the intercept and its standard error, from the `xtx` it already
solves plus `RSS = y'y - coef'X'y`. `factor_alpha_stats()` takes those standard
errors and divides by them. Both the published and the mined side screen on the
OLS intercept t. `fit_dm_window_models()` now asserts that the mean in-sample
abnormal return equals the fitted intercept, which held for all 127 windows.
`schema_version` went 2 -> 3 (`published_stats` gains `abar_capm_tv_se` and
`abar_ff4_tv_se`; every alpha t changes), and `S4b_RVsDM_ByGroup.R`'s guard was
bumped with it so stale caches fail loudly.

Validation run to a temporary `FACTOR_DM_OUT_DIR`, versus the production cache:

| | old | new |
|---|---:|---:|
| CAPM eligible predictors | 147 | 147 |
| CAPM eligible pairs | 1,016,398 | 1,011,118 (-0.5%) |
| FF4 eligible predictors | 132 | 129 |
| FF4 eligible pairs | 861,810 | 815,492 (-5.4%) |
| FF4 mean OOS published | 68.17 | 66.00 |
| FF4 mean OOS data-mined | 60.93 | 60.40 |

The three predictors that leave the FF4 columns are DolVol, Investment, and
NetPayoutYield. FF4 outperformance narrows from 7.24 to 5.60. CAPM is
essentially unchanged. The A1 pair fingerprint is untouched.

**Finding 3.** `fit_dm_window_models()` now errors if handed more than one
sample window, or a mined ratio twice in one window, instead of silently
deduplicating and taking the window from row 1.

**Finding 4.** `SA01`'s `extract_beta()` was renamed to `(ret, factor_return)`
and fits `lm(ret ~ factor_return)`; the `roll_lm()` call was reordered to
`roll_lm(mktrf, ret, width = 60)`. The cosmetic `mktrf*100` scaling was dropped
at the same time -- it cancelled between slope and fitted value, so that part
does not move the abnormal returns, but `beta_all` and `beta_roll` are now
interpretable loadings. Verified against direct and rolling `lm`.

`tests/test_factor_adjusted_dm_helpers.R` was rewritten for the new API and now
pins `alpha_t` to `summary(lm)`'s intercept t, with an explicit guard that the
superseded formula is both larger and different enough to move the screen.

**Not yet done:** the production
`../Data/Processed/factor_adjusted_dm_benchmarks.RDS` was not overwritten, so
Figure 2(a) and Tables 6, 7, and IA.8 still carry the old FF4 universe. Rerun
`3c` and the Section 2 and 4 renderers to propagate. SA01's figures likewise
still carry the reversed loadings until that appendix is rerun.

## Verification scripts

Both were written to the session scratchpad rather than the repo and are not
preserved:

1. Loads the function definitions out of `3c` by parsing it, rebuilds one
   window from `dmcomp_sumstats.RDS` and the mined panel, and diffs
   `fit_dm_window_models()` against a per-strategy `lm` loop.
2. Recomputes the alpha t-statistic both ways for the same window and for every
   published signal, and counts screen flips.
