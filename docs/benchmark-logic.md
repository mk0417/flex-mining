# benchmark-logic.md

Lays out, precisely, how "we form data-mined benchmarks" to published strategies
using the 29,000 accounting ratios, plus minor alternative-mining and appendix
variants.

There are three core steps:

## 1. DM Sumstats 

- Calculate in-sample summary statistics for each (published signal, accounting ratio) pair

## 2. DM Benchmarks

For each published signal, select accounting ratios as benchmarks using
in-sample statistics. Signals and mined ratios are oriented so their in-sample
mean returns are positive. The core benchmarks use accounting ratios; minor
robustness benchmarks also cover ticker mining and appendix specifications.

### Common accounting-ratio gates

- \>= 10 stocks per leg every in-sample month
- \>= 60 in-sample months
- a complete final in-sample year
- orientation from the in-sample mean
- correlations signed using that orientation

### A. Raw-mean-return benchmarks

These benchmarks select and evaluate accounting ratios using raw long-short
returns. They comprise two separate branches:

- **Significant-return branch**
    - **A1. Significant:** raw in-sample |t| > 2.
    - **A2. Significant, uncorrelated:** A1 and signed correlation with the
      published strategy <= 0.10.
- **Matched-statistics branch**
    - **A3. Matched statistics:** oriented raw in-sample |t| and mean return
      are each within 10% of the published strategy.
    - **A4. Matched statistics, uncorrelated:** A3 and signed correlation with
      the published strategy <= 0.10.
    - A3 and A4 do not additionally impose the A1 |t| > 2 screen.

Only A2 and A4 apply a correlation screen. Only A1 feeds the factor-adjusted
benchmark families below.

### B. CAPM-alpha benchmark

- **B1. Significant CAPM alpha**
    - Begin with the A1 accounting-ratio universe.
    - Estimate CAPM beta separately in- and post-sample.
    - Replace raw returns with CAPM alpha.
    - Retain data-mined ratios with in-sample CAPM-alpha t > 2.
    - Retain published comparisons when the published strategy has both raw
      in-sample t > 2 and CAPM-alpha t > 2.

### C. FF4-alpha benchmark

- **C1. Significant FF4 alpha**
    - Begin with the A1 accounting-ratio universe.
    - Estimate FF4 loadings separately in- and post-sample.
    - Replace raw returns with FF4 alpha, where FF4 is FF3 plus momentum.
    - Retain data-mined ratios with in-sample FF4-alpha t > 2.
    - Retain published comparisons when the published strategy has both raw
      in-sample t > 2 and FF4-alpha t > 2.

### D. Minor robustness benchmarks

These one-off variants support alternative-mining and appendix checks rather
than the core benchmark hierarchy above.

- **D1. Top 5% accounting:** top 5% of accounting ratios by raw in-sample
   |t|, without an additional |t| > 2 requirement.
- **D2. Top 5% ticker:** top 5% of ticker-mined strategies by raw in-sample
   |t|, without an additional |t| > 2 requirement.
- **D3. Spanning splits:** begin with A1; classify ratios as spanned using
   either signed correlation > 0.50 or principal-components adjusted R2 > 0.25
   (with more than 30 PCA observations); and divide unspanned ratios by whether
   their raw |t| exceeds the published strategy's t-statistic.

### Code and results crosswalk

`3a_PrepDMBenchmarks.R`, `3c_FactorAdjustedDMPrep.R`, and the cited appendix
scripts implement the following benchmarks:

- **A1. Significant.** Defined using `select_accounting_t2_pairs()`; saved as
   `raw_dm_benchmarks.RDS$accounting_t2`. This is also the base universe for
   B1 and C1.
    - Figure 1: published versus raw data-mined returns.
    - Figure 2(b): raw data mining in the two restricted publication samples.
    - Table 6: Long-Short Return columns.
    - Table 7: Long-Short Return columns.
- **A2. Significant, uncorrelated.** Defined using A1 plus the
   signed-correlation screen; saved as
   `raw_dm_benchmarks.RDS$accounting_t2_uncorr`.
    - Figure 2(c): broad “Data-Mined, Excluding Correlated” series.
    - Table 3(a), columns (1)--(3): scaled-return regressions without time fixed
      effects.
    - Table 4(a), columns (1)--(3): scaled-return regressions with time fixed
      effects.
- **A3. Matched statistics.** Defined using `select_matched_dm_pairs()`; saved as
   the `matched_ret_*` columns in `raw_dm_benchmarks.RDS$matched`. This is also
   the intermediate universe from which A4 is formed.
    - Table 8: book-to-market matches.
    - Table 9: momentum matches.
    - Table 10: size matches.
- **A4. Matched statistics, uncorrelated.** Defined using
   `build_matched_uncorr_pair_data()`; saved as the `matched_uncorr_ret_*`
   columns in `raw_dm_benchmarks.RDS$matched`.
    - Figure 2(c): “DM, Excl. Corr, Matched Stats” series.
    - Table 3(b), columns (4)--(6): unscaled-return regressions without time
      fixed effects.
    - Table 4(b), columns (4)--(6): unscaled-return regressions with time fixed
      effects.
- **B1. Significant CAPM alpha.** Defined from A1 with sample-specific CAPM
   betas and the alpha-t screen; saved as
   `factor_adjusted_dm_benchmarks.RDS$capm`.
    - Figure 2(a): published and data-mined CAPM-alpha series.
    - Table 6: CAPM Alpha columns.
    - Table 7: CAPM Alpha columns.
- **C1. Significant FF4 alpha.** Defined from A1 with sample-specific FF4
   loadings and the alpha-t screen; saved as
   `factor_adjusted_dm_benchmarks.RDS$ff4`.
    - Figure 2(a): published and data-mined FF3+Mom-alpha series.
    - Table 6: FF3+Mom Alpha columns.
    - Table 7: FF3+Mom Alpha columns.
- **D1. Top 5% accounting.** Defined using `SelectDMStrats()`; saved as
   `raw_dm_benchmarks.RDS$accounting_top5`.
    - Figure 2(d): top-5% accounting-ratio series.
- **D2. Top 5% ticker.** Defined using `SelectDMStrats()`; saved as
   `raw_dm_benchmarks.RDS$ticker_top5`.
    - Figure 2(d): top-5% ticker-mining series.
- **D3. Spanning splits.** Defined in `SA11_DMSpanPCAPrep.R`; saved as
    `dm_span_analysis.RDS`.
    - Figure B.4(a): principal-component spanning splits.
    - Figure B.4(b): correlation spanning splits.
- **Other main-text panels.** These do not instantiate one of the benchmark
  screens above.
    - Table 1(a): mined-universe return summary.
    - Table 1(b): mined-universe principal-components summary.
    - Table 2: mined decay by economic theme.
    - Table 5: published-signal sample description.

The 10% t-stat and mean-return tolerances are defaults of
`select_matched_dm_pairs()` and therefore apply whenever A3 or A4 is
constructed. Some older variables named `matchRet` refer instead to benchmark
A1, D1, or D2; the name alone does not imply matched statistics.
`ret_for_plot0.RDS` and `ret_for_plot1.RDS` are compatibility artifacts; new
consumers should use the saved outputs above.


## 3. Evaluation: 

Compare academic signals to benchmarks post-sample, using two basic data formats

1. Event time trajectories: 
    - Here, we can take advantage of the full panel of DM benchmarks.
    - Answers: how does performance evolve after the in-sample periods end?    

2.  Calendar time pairs:
    - Here, for a given academic signal, we average all DM benchmarks to form a single portfolio that is an alternative to the academic one.
    - Allows for McLean-Pontiff style regressions without breaking the computer.
