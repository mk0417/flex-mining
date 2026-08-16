# benchmark-logic.md

Lays out, precisely, how "we form data-mined benchmarks" to published strategies using the 29,000 accounting ratios.

There are three core steps:

## 1. DM Sumstats 

- Calculate in-sample summary statistics for each (published signal, accounting ratio) pair

## 2. DM Benchmarks

For each published signal, select accounting ratios that to be valid benchmarks based on some in-sample statistical criteria. Throughout the below, we assume signals are signed.

Common gates (all criteria):

- \>= 10 stocks per leg every in-sample month
- \>= 60 in-sample months
- a complete final in-sample year
- orientation from the in-sample mean
- correlations are signed vs the published strategy

**Benchmarks** are defined using the following screens:

1. raw |t| > 2
    - every significant ratio
2. raw |t| > 2 & cor(pub, dm) < 0.1
    - significant but decorrelated *(aspirational)*
3. top 5% by |t|
    - most extreme mined ratios (accounting and ticker universes)
4. matched: |t| and mean return each within 10% of published
    - a statistical twin
5. matched & cor(pub, dm) < 0.1
    - twin on stats, not comovement

Factor-adjusted: the t>2 universe (benchmark 1) with returns replaced by factor-model alpha.

6. sample-specific betas (in- vs post-sample), CAPM & FF4
    - time-varying; Section 4
7. full-sample betas, CAPM & FF3
    - Internet Appendix


## 3. Evaluation: 

Compare academic signals to benchmarks post-sample, using two basic data formats

1. Event time trajectories: 
    - Here, we can take advantage of the full panel of DM benchmarks.
    - Answers: how does performance evolve after the in-sample periods end?    

2.  Calendar time pairs:
    - Here, for a given academic signal, we average all DM benchmarks to form a single portfolio that is an alternative to the academic one.
    - Allows for McLean-Pontiff style regressions without breaking the computer.