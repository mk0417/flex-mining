# Run configuration for the flex-mining pipeline.
#
# How to run: this file is data only -- no packages, no functions. It is sourced
#   by 0_Environment.R (so every chapter script inherits globalSettings) and by
#   MAIN.R (which reads runStages to decide which stages to run). The two
#   drivers that only need a versioned path (3_Precompute.R,
#   S5_BestPredictors.R) source it directly to read
#   globalSettings$universe$dataVersion.
# Inputs:  none.
# Outputs: defines globalSettings, runStages; sets the RNG seed.
#
# Keep this file free of library() calls and function definitions so it stays
# cheap to source standalone. Reference data (compnames, colors) and the R
# session bootstrap live in 0_Environment.R.
#
# Consequential run choices are authored as one sub-list per concern and then
# assembled into the nested globalSettings at the bottom. The grouping follows
# the pipeline and the benchmark taxonomy in docs/benchmark-logic.md, so
# settings read as globalSettings$<group>$<name>.

# Pipeline stage switches (read by MAIN.R) ---------------------------------
# Each stage runs as its own Rscript subprocess; see MAIN.R.
runStages <- list(
  download_and_clean       = FALSE,  # Re-pull ../Data/Raw; changes the vintage
  data_mining              = FALSE,  # Chapter 2; hours
  precompute               = TRUE,   # Chapter 3; slow reusable analysis
  research_vs_data_mining  = TRUE,   # Section 2, plus the introduction figure
  learning                 = TRUE,   # Section 3
  heterogeneity            = TRUE,   # Section 4
  best_predictors          = TRUE,   # Section 5
  appendices               = TRUE,   # Appendices
  export_data_to_csv       = TRUE    # Chapter 9
)

# Data vintage and mined-universe construction (Chapter 2) -----------------
universeSettings <- list(
  dataVersion = 'CZ-style-v8b',

  # signal construction
  form           = c('v1/v2', 'diff(v1)/lag(v2)'), # 'pdiff(v1/v2)', 'pdiff(v1)', 'diff(v1/v2)', 'pdiff(v1)-pdiff(v2)')
  denom_min_fobs = 0.25, # minimum fraction of non-missing observations in 1963
  # portfolio construction
  longshort_form = 'ls_extremes',
  portnum        = c(10),
  sweight        = c('ew','vw'),
  trim           = NA_real_,  # NA or some quantile e.g. .005
  # data availability and trading
  backfill_dropyears = 0, # number of years to drop for backfill bias adj (the CZ repo lacks this adjustment)
  reup_months        = 6, # stocks are traded using new data at end of these months
  data_avail_lag     = 6, # months
  toostale_months    = 18, # months after datadate to keep signal for
  delist_adj         = 'ghz', # 'none' or 'ghz'
  crsp_filter        = NA_character_ # use NA_character_ for no filter
)

# Published-signal inclusion and sampling ---------------------------------
inclusionSettings <- list(
  restrictType = 'topT', # 'topT' or NULL for all signals
  topT         = 2, # number of top t-stat signals to keep from each paper
  signalnum    = Inf # number of signals to sample or Inf for all
)

# Benchmark definitions (see docs/benchmark-logic.md, step 2) --------------
#
# Benchmarks are named with the labels used in docs/benchmark-logic.md, which is
# the authoritative description of each screen and of the exhibits it feeds:
#
#   (all)                               gates (see below)
#   A1  Significant                     t_min, t_max
#   A2  Significant, uncorrelated       A1 + corr_max
#   A3  Matched statistics              matched_uncorr_{t,r}_reltol
#   A4  Matched statistics, uncorr.     A3 + corr_max
#   B1  Significant CAPM alpha          A1 universe + alpha-t screen (3c)
#   C1  Significant FF4 alpha           A1 universe + alpha-t screen (3c)
#   D1  Top 5% accounting               t_rankpct_min, set locally (see below)
#   D2  Top 5% ticker                   t_rankpct_min, set locally (see below)
#   D3  Spanning splits                 thresholds hardcoded in
#                                       Appendices/SA11_DMSpanPCAPrep.R
benchmarkSettings <- list(
  # Common accounting-ratio gates, applied to every mined ratio before any
  # benchmark-specific screen. These correspond one-to-one with the "Common
  # accounting-ratio gates" list in docs/benchmark-logic.md and are imposed in
  # exactly one place, apply_common_gates() in helpers/matching.R. No selector,
  # chapter script, or appendix should re-implement them.
  gates = list(
    minNumStocks       = 20, # >= minNumStocks/2 stocks in each leg every in-sample month
    nmonth_min         = 60, # minimum in-sample history (months)
    nlastyear_required = 12  # months required in the final in-sample calendar year
  ),

  # A1 significant screen: raw |t| > t_min. Also the base universe for A2, and
  # for the factor-adjusted B1 and C1.
  t_min = 2,
  t_max = Inf,

  # Top x% of data-mined |t-stats|. DO NOT CHANGE: 100 (= off) is what A1--A4
  # and the EZ-themes code require. D1 and D2 build their top-5% universe from a
  # local copy of the screen (see 3a_PrepDMBenchmarks.R, top5_screen), so editing
  # this global would silently alter every other benchmark instead.
  t_rankpct_min = 100,

  # A2 and A4 correlation screen: keep pairs whose signed correlation
  # cor * sign(rbar) is <= this value. Used by both the broad "excluding
  # correlated" benchmark (A2) and the performance-matched one (A4).
  corr_max = 0.10,

  # A3 and A4 matched statistics: keep mined ratios whose in-sample t-stat and
  # mean return are each within this relative tolerance of the published signal.
  # A3 does not additionally impose the A1 |t| > 2 screen.
  matched_uncorr_t_reltol = 0.10,
  matched_uncorr_r_reltol = 0.10,

  # absolute (level) DM-vs-published tolerances. Currently OFF (Inf); no
  # benchmark uses them, and the active matching uses the relative tolerances
  # above.
  t_tol    = .1*Inf, # tolerance in t-statistics (DM vs OP)
  r_tol    = .3*Inf, # tolerance in mean return (DM vs OP)
  t_reltol = .1*Inf, # relative (to OP) tolerance in t-statistics (DM vs OP)
  r_reltol = .3*Inf  # relative (to OP) tolerance in mean return (DM vs OP)
)

# Evaluation window (see docs/benchmark-logic.md, step 3) -----------------
evaluationSettings <- list(
  minShareTG2 = .1,  # Include strategies with t-stat > 2 in at least X % of published time periods
  TG2Set = '1994-2020' # 1994-2020: DM strategies evaluated over 1994-2020
                       # Matches:   all sample matching periods
                       # Rolling1994-2020: DM strategies evaluated on rolling t-stats in 1994-2020
)

# Journals and section-specific grouping ----------------------------------
journalSettings <- list(
  # Finance and Accounting journals
  finlistAll  = c('JF','RFS','JFE','JFQA','MS', 'ROF', 'JEmpFin', 'JFM'),
  acctlistAll = c('AR','RAS','JAR','JAE', 'CAR', 'BAR', 'JBFA'),
  # Top 3 journals for main analysis
  top3Finance = c('JF', 'RFS', 'JFE'),
  top3Accounting = c('AR', 'JAR', 'JAE'),
  nmonth_min  = 120 # minimum number of months to keep DM signal in EZ themes code
)

# Compute and debugging ---------------------------------------------------
computeSettings <- list(
  prep_data = T,
  num_cores = 4,  # Note: ~5 GB RAM per core required.
  shortlist = F,
  interactive_mode = FALSE  # Set to TRUE for interactive execution
)

# Assemble the nested settings consumed across the pipeline ---------------
globalSettings <- list(
  universe   = universeSettings,
  inclusion  = inclusionSettings,
  benchmark  = benchmarkSettings,
  evaluation = evaluationSettings,
  journals   = journalSettings,
  compute    = computeSettings
)

# Set seed for random sampling (affects cached sampling)
set.seed(1337)
