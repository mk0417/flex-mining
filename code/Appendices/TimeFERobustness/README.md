# Time-fixed-effects robustness appendix

This directory contains the robustness pipeline comparing the paper's
post-publication decay estimates with McLean--Pontiff (MP), Jensen--Kelly--
Pedersen (JKP), and multiple Chen--Zimmermann (CZ) portfolio constructions.

Run commands from any directory:

```sh
Rscript Appendices/TimeFERobustness/run.R --help
Rscript Appendices/TimeFERobustness/run.R preflight
Rscript Appendices/TimeFERobustness/run.R download
Rscript Appendices/TimeFERobustness/run.R build
Rscript Appendices/TimeFERobustness/run.R exhibits
```

`download` is deliberately separate from `build`. It downloads the pinned
Open Source Asset Pricing release and obtains the three CRSP-derived signals
used in that release. `build` can also need one WRDS connection when its
detailed CRSP return/market-equity cache is absent. In the sandbox, obtain the
user's permission before either command makes a WRDS connection.

## Layout

Six R files, isolated from the rest of the repository: nothing here reads a
main-pipeline cache, and no main-pipeline script sources anything here.

| File | Role |
| --- | --- |
| `run.R` | Driver and preflight; runs each stage as its own Rscript |
| `setup.R` | `timefeSettings`, packages, folders, and the helpers with more than one consumer |
| `download.R` | Downloads the CZ signal-level panel (`download`) |
| `estimate.R` | The MP, JKP, alternative-construction, and date sections |
| `cz_terciles.R` | CZ signal-level tercile portfolios and their regressions |
| `exhibits.R` | Renders the six TeX exhibits, then checks them |

There are only two reasons a stage is its own file, and both are practical.
`download` downloads gigabytes and can need WRDS, so it has to be separately
invocable. `cz_terciles.R` holds a 2.5 GB Arrow signal panel and the CRSP
monthly file in memory, so it runs in its own process and releases that memory
before the exhibits are rendered. Everything else that shares inputs shares a
file: `estimate.R` reads the JKP metadata and returns once for its three JKP
sections, and reuses its screened CZ panel across three more.

`setup.R` holds only what more than one file needs — the settings, the
assertion helper, the CZ documentation reader, and the decay pipeline. Input
readers with a single consumer live in that consumer, which is why all the JKP
and zip-reading plumbing sits in `estimate.R`.

Every panel is built by composing the same helpers, so one screen and one
regression definition serve every reported number:

```r
add_event_indicators()   # post-sample and post-publication indicators
add_in_sample_stats()    # per-signal in-sample mean, t-statistic, months
quality_screen()         # keep signals with in-sample t above the threshold
scale_by_signal_mean()   # or grand_mean_scale()
decay_rows()             # estimate with and without month FE, one row each
```

Returns are percent per month throughout, whatever units the source library
publishes.

## The quality screen

Every specification keeps only signals whose in-sample long-short return has a
signed t-statistic above `timefeSettings$screen$t_min`. This is not a
presentational choice. Without it the scaled JKP panel produces a large
*positive* post-sample coefficient, the opposite sign of the decay found
everywhere else, because a handful of cited JKP factors have an in-sample mean
within a few basis points of zero and `retScaled` divides by that mean. A
one-time investigation of this (leave-one-factor-out regressions, winsorizing,
trimming, and t-statistic thresholds) found the artifact concentrated in
specific factors rather than a few months: `debt_me` alone, with an in-sample
mean of 2.3 bps/month and a t-statistic of 0.23, accounted for most of it.
Winsorizing and trimming shrink it; screening on the in-sample t-statistic
removes it, because it excludes the mechanism rather than the symptom. The
finding is recorded here rather than kept as a stage, since it is settled.

## Storage

The default paths follow this repository's data layout:

- external inputs: `../Data/Raw/TimeFERobustness/`;
- constructed caches and regression CSVs:
  `../Data/Processed/TimeFERobustness/`;
- generated TeX exhibits: `../Results/TimeFERobustness/`.

For isolated validation, override these roots with `TIMEFE_RAW_DIR`,
`TIMEFE_PROCESSED_DIR`, and `TIMEFE_RESULTS_DIR`. The release and upstream
commits are pinned in `setup.R`.

This stage is opt-in in the repository's `config.R`; ordinary appendix
rebuilds do not run it.
