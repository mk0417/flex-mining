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
benchmarkSettings <- list(
  # common gates applied to every mined ratio before any screen
  minNumStocks     = 20, # min stocks per month over the in-sample period (ie minNumStocks/2 in each leg)
  match_nmonth_min = 60, # minimum pair-level in-sample history (months)

  # significant screen (benchmarks 1 and 2): raw |t| > t_min
  t_min = 2,
  t_max = Inf,

  # top-5% screen (benchmark 3)
  t_rankpct_min = 100, # top x% of data-mined |t-stats|; 100% = off (set to 5 for the top-5% benchmark)

  # uncorrelated cutoff (benchmarks 2 and 5). Used by both the broad "excluding
  # correlated" benchmark and the performance-matched benchmark: keep pairs
  # whose signed correlation cor * sign(rbar) is <= this value.
  matched_uncorr_corr_max = 0.10,

  # performance-matched benchmark (benchmarks 4 and 5): keep mined ratios whose
  # in-sample t-stat and mean return are each within this relative tolerance of
  # the published signal.
  matched_uncorr_t_reltol = 0.10,
  matched_uncorr_r_reltol = 0.10,

  # absolute (level) DM-vs-published tolerances. Currently OFF (Inf); the active
  # matching uses the relative tolerances above.
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
