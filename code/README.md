# flex-mining
Code for replicating ["Peer-reviewed theory does not help predict the cross-section of stock returns."](https://arxiv.org/pdf/2212.10317.pdf)

Monthly returns of 30,000 long-short strategies can be found at [this Google Drive link](https://drive.google.com/drive/folders/1SZe_aF4ZNvK4ZRx2jQUE1j19KQvBaqWr).

The origins of predictability and quotes are found in [DataInput/SignalsTheoryChecked.csv](https://github.com/chenandrewy/flex-mining/blob/main/DataInput/SignalsTheoryChecked.csv)

This project uses the active R installation's default library paths and does not manage a project-local package environment.

Current results were produced with **R 4.5.3** and packages from the Posit P3M snapshot dated **2026-07-15**, plus `pcaMethods` from Bioconductor. See [docs/environment.md](docs/environment.md) for the package versions and how to regenerate the list.

## Pipeline

Run scripts from the repository root. `MAIN.R` exposes an independent switch
for each data stage and paper section:

1. `1_Download_and_Clean.R` acquires a new external-data vintage and cleans it.
2. `2_DataMining.R` constructs and matches the mined strategies; this takes
   roughly two hours.
3. `3_Precompute.R` builds reusable correlations, PCA results, summary data,
   and plot panels under `../Data/Processed`.
4. `S2_ResearchVsDataMining.R` renders the introduction figure and Section 2
   exhibits.
5. `S3_Learning.R` renders the Section 3 learning tables from cached regression
   models.
6. `S4_Heterogeneity.R` renders the Section 4 heterogeneity exhibits.
7. `S5_BestPredictors.R` renders the Section 5 predictor examples.
8. `SA_Appendices.R` renders appendix-only exhibits, excluding SA11.
9. `9_ExportDataToCsv.R` exports the mined-strategy data for sharing.
10. `SA_AppendicesPCA.R` renders the SA11 correlation and PCA robustness
    exhibits; it takes about an hour and is the heaviest appendix stage.
11. `Appendices/TimeFERobustness/run.R` optionally rebuilds the large,
    externally sourced time-fixed-effects robustness appendix.

Steps 4-9 share the single `exhibits` switch in `runStages`; the slow or
externally sourced stages keep their own. Any driver above can be run on its
own with `Rscript <driver>.R` while iterating on one exhibit.

The paper-section stages treat processed data as read-only. For a formatting-only
figure or table change, run only the corresponding section. Chapter 1 overwrites
`../Data/Raw` with a new, non-recoverable WRDS/Google Drive vintage, so its
`MAIN.R` switch is off by default and should be enabled deliberately.

The time-FE robustness stage is also off by default. See
`Appendices/TimeFERobustness/README.md` for its pinned inputs, storage
layout, commands, and WRDS warning.
