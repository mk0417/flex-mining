# Exhibit map

Run all commands from `code/`. `MAIN.R` runs the enabled stages as separate
`Rscript` processes. The default stages are `precompute`, `exhibits`, and
`time_fe_robustness`; data acquisition and data mining are disabled in
`config.R`. The paper reads live files from `../Results/`, and
`paper/check-exhibits.sh --sync` copies referenced files into `paper/exhibits/`.

## Pipeline

| Driver | Children | Purpose |
| --- | --- | --- |
| `3_Precompute.R` | `3a`, `3b`, `3c`, `3d_DMCorrelationsPCA.R`, `3e_DMSpanPCA.R` | Build reusable caches. The two PCA scripts run last in separate processes. |
| `S2_ResearchVsDataMining.R` | `S2a`, `S2b`, `S2c_DMCorrelationsPCATables.R`, `S2d`, `S2e` | Introduction and Section 2. |
| `S3_Learning.R` | `S3a`, `S3b` | Section 3 regressions. |
| `S4_Quality.R` | `S4a_ByJournal.R`, `S4b_RenownedMatches.R` | Section 4 journal rankings and renowned matches. |
| `SA_Appendices.R` | `SA03`, `SA11_DMSpanPCAPlots.R`, `SA12`, `SA13`, `SA14` | Appendix exhibits. |
| `Appendices/TimeFERobustness/run.R` | time-FE module | Time-FE robustness table. |

## Main text

| Exhibit | Result in `../Results/` | Producer |
| --- | --- | --- |
| Figure 1 | `Fig_DM_t_min_2_se_indicators_calendar.pdf` | `S2a_ResearchVsDMPlots.R` |
| Table 1, panel A | `dm-sortsFull.tex` | `S2b_DataMiningSummaryTables.R` |
| Table 1, panel B | `DM_pca.tex` | `S2c_DMCorrelationsPCATables.R`, from `3d` cache |
| Table 2 | `theme_ez_decay.tex` | `S2d_EZThemes.R` |
| Figure 2, panels A–D | `Fig2a_FactorAdj.pdf`, `Fig2b_PubSampleLimits.pdf`, `Fig2c_MatchedExclCorr.pdf`, `Fig2d_AltMining.pdf` | `S2e_Fig2Plots.R` |
| Section 3 regressions | `Table_MPStyleRegsNoTimeFE.tex`, `Table_MPStyleRegsTimeFE.tex` | `S3b_MPStyleDecayTables.R` |
| Section 4 journal ranking | `Table_FactorAdjusted_TimeVarying_DisciplineJournal_ff4_t2.tex` | `S4a_ByJournal.R` |
| Section 4 renowned matches | `inspect-summary.tex` | `S4b_RenownedMatches.R` |

## Appendices

| Exhibit | Result in `../Results/` | Producer |
| --- | --- | --- |
| Individual-DM regressions | `Table_MPStyleRegsIndividualDM.tex` | `Appendices/SA13_MPStyleRegsIndividualDM.R` |
| Accounting-only regressions | `Table_MPStyleRegsNoTimeFE_AccountingOnly.tex`, `Table_MPStyleRegsTimeFE_AccountingOnly.tex` | `Appendices/SA14_MPStyleRegsAccountingOnly.R` |
| Time-FE robustness | `TimeFERobustness/timefe-robustness.tex` | `Appendices/TimeFERobustness/exhibits.R` |
| Post-2003 DM summary | `dm-sortsPost2003.tex` | `S2b_DataMiningSummaryTables.R` |
| DM correlations | `quantilesCorDM.tex` | `S2c_DMCorrelationsPCATables.R`, from `3d` cache |
| Themes by sample end | `theme_ez_decayinSampEnd{1990,2000,2010}.tex` | `Appendices/SA12_EZThemesRobustness.R` |
| Unspanned DM | `Fig_DM_unspan_match_t_g_{cor,PCA}.pdf` | `Appendices/SA11_DMSpanPCAPlots.R`, from `3e` cache |
| Structural breaks | `samp_split_summary.tex`, `break_vs_sampend.pdf` | `Appendices/SA03_StructuralBreak.R` |
| Renowned-match detail | `inspect-{BMdec,Mom12m,Size}.tex` | `S4b_RenownedMatches.R` |

`S4a_ByJournal.R` also writes sample-specific audit CSV and TeX files under
`../Results/FactorAdjusted/TstatFilter/`. They are validation artifacts and
are not included by the paper. `S2e_Fig2Plots.R` can write diagnostic RDS
files when `FIG2_DATA_OUTPUT_DIR` is set.

`LatexPreview/exhibits.tex` assembles the live outputs into a review PDF.
Build it with `./LatexPreview/build.sh` from `code/`; the build does not run R.
