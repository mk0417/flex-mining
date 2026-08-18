# Time-fixed-effects robustness appendix

This directory contains the robustness pipeline comparing the paper's
post-publication decay estimates with McLean--Pontiff (MP), Jensen--Kelly--
Pedersen (JKP), and multiple Chen--Zimmermann (CZ) portfolio constructions.

Run commands from any directory:

```sh
Rscript Appendices/SA15_TimeFERobustness/run.R --help
Rscript Appendices/SA15_TimeFERobustness/run.R preflight
Rscript Appendices/SA15_TimeFERobustness/run.R acquire
Rscript Appendices/SA15_TimeFERobustness/run.R build
Rscript Appendices/SA15_TimeFERobustness/run.R check
```

`acquire` is deliberately separate from `build`. It downloads the pinned
Open Source Asset Pricing release and obtains the three CRSP-derived signals
used in that release. `build` can also need one WRDS connection when its
detailed CRSP return/market-equity cache is absent. In the sandbox, obtain the
user's permission before either command makes a WRDS connection.

The default paths follow this repository's data layout:

- external inputs: `../Data/Raw/TimeFERobustness/`;
- constructed caches and regression CSVs:
  `../Data/Processed/TimeFERobustness/`;
- generated TeX exhibits: `../Results/TimeFERobustness/`.

For isolated validation, override these roots with `TIMEFE_RAW_DIR`,
`TIMEFE_PROCESSED_DIR`, and `TIMEFE_RESULTS_DIR`. The release and upstream
commits are pinned in `R/config.R`.

This stage is opt-in in `config.R`; ordinary appendix rebuilds do not run it.
