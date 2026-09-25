# Plan: clean up `code/` and reorganize it around the new paper draft

Date: 2026-09-24. Branch: `draft/separate-papers`. Status: plan only, nothing
changed yet. Decisions recorded 2026-09-24: delete the retired scripts; move
SA11 into precompute.

## Why

The new draft is narrower than the arXiv v7 paper that the pipeline was built
around. The draft's sections are:

| Section | Label | Content |
| --- | --- | --- |
| 1 | `sec:intro` | Introduction, Figure 1 |
| 2 | `sec:pubvs` | Research vs data mining (DM summary, themes, Figure 2) |
| 3 | `sec:learn` | Learning from academics vs the data (MP-style regressions) |
| 4 | `sec:quality` | Highest-quality research: journal ranking, then B/M, momentum, size |
| Appendix | `sec:robust` | Decay-regression robustness (individual DM, accounting-only, time FE) |
| IA | `sec:ia-*` | Post-2003 DM, DM correlations, themes by sample end, unspanned DM, structural breaks, full match tables |

The draft no longer has the old heterogeneity section (risk vs mispricing,
theory vs no theory, model counts, word counts, predictor counts by
theory/journal). The code still builds all of those exhibits, and `MAIN.R`,
`docs/exhibit_map.md`, and the driver names still follow the old section
layout.

`paper/check-exhibits.sh` reports 28 exhibits referenced and 30 present. The
two orphans are `Fig_DecayVsJournal_Means.pdf` and
`Table_FactorAdjusted_TimeVarying_AnyModelVsNoModel_ff4_t2.tex`.

## What the draft uses, by script

The table below was verified by matching the output basenames each script
writes against the `\input`/`\includegraphics` calls in `paper/`.

| Script | Outputs the draft uses | Outputs the draft does not use | Verdict |
| --- | --- | --- | --- |
| `1_*`, `2_*`, `2a`, `2c`, `3_*`, `3a`–`3c` | upstream caches | — | keep |
| `S2a_ResearchVsDMPlots.R` | `Fig_DM_t_min_2_se_indicators_calendar.pdf` | many `Fig_DM_*` variants, `Fig_DM_Roll*`, `.md` sinks | keep, trim |
| `S2b_DataMiningSummaryTables.R` | `dm-sortsFull.tex`, `dm-sortsPo∫st2003.tex` | — | keep |
| `S2d_EZThemes.R` | `theme_ez_decay.tex` | `theme_ez_slides.tex`, `numer_list.csv`, `temp.tex` | keep, trim |
| `S2e_Fig2Plots.R` | `Fig2a`–`Fig2d` | — | keep |
| `S3a`, `S3b` | `Table_MPStyleRegs{NoTimeFE,TimeFE}.tex` | — | keep |
| `S4a_DataCounts.R` | none | `ApproachVsJournalsPart1-3.tex`, `SignalsByTheoryAndJournal.tex`, `table_risk_vs_mispricing.tex` | retire |
| `S4b_RVsDM_ByGroup.R` | `..._DisciplineJournal_ff4_t2.tex` | theory/model table, `AnyModelVsNoModel` table | keep, trim |
| `S5a_InspectTables.R` | `inspect-summary.tex`, `inspect-{BMdec,Mom12m,Size}.tex` | `inspect-realestate.tex` | keep, trim |
| `SA01_RiskVsMispricingPlots.R` | none | `Fig_Risk_via_*`, `Fig_NSignalEventTime` | retire |
| `SA02_RegDecayTable.R` | none | `RegressionMultiColumns*.csv` | retire |
| `SA03_StructuralBreak.R` | `samp_split_summary.tex`, `break_vs_sampend.pdf` | `temp.tex` | keep |
| `SA04_DecayVsWordcountPlot.R` | none | `Fig_DecayVsWords*.pdf` | retire |
| `SA05_DecayVsModelcountPlot.R` | none | `Fig_DecayVsModel_*` | retire |
| `SA06_DecayVsJournal.R` | none (orphan) | `Fig_DecayVsJournal_Means.pdf` | retire |
| `SA11_*` (4 scripts) | `DM_pca.tex` (main Table 1), `quantilesCorDM.tex`, `Fig_DM_unspan_match_t_g_{cor,PCA}.pdf` | — | keep |
| `SA12_EZThemesRobustness.R` | `theme_ez_decayinSampEnd{1990,2000,2010}.tex` | `theme_corr_academic.tex` | keep, trim |
| `SA13`, `SA14` | individual-DM and accounting-only regression tables | — | keep |
| `TimeFERobustness/` | `timefe-robustness.tex` | the six older per-panel tables, if still written | keep |
| `9_ExportDataToCsv.R` | not a paper exhibit | — | keep (data release) |

Inputs that only retired scripts use:

- `DataIntermediate/TextClassification.csv`: read only by SA04.
- `DataIntermediate/{anom_words,risk_words,text_with_pdf_name}.csv`: nothing
  live reads them. They belong to the archived text analysis.
- `DataInput/SignalsTheoryChecked_withPV.csv`: nothing reads it.
- `DataInput/SignalsTheoryChecked.csv` stays. S2a, S2d, S2e, S4b, and SA03
  still read it.

## Decisions

- **Delete the retired scripts** outright rather than archiving them. Git
  history keeps them, and the commit that deletes them names each script and
  what it built, so `git log --diff-filter=D` finds them later.
- **Move the SA11 correlation/PCA work into the precompute stage** (Phase 3),
  so main-text Table 1 no longer depends on an optional appendix stage.
- The S4b theory/model and model-vs-no-model tables are not retired scripts.
  S4b stays; Phase 2 only stops it writing those two fragments. The
  introduction's `tbc` note about restoring heterogeneity results can bring
  them back.

## Branching

Do all phases, including the SA11 move, on `draft/code-cleanup`, branched off
`draft/separate-papers`.

- Not off `main`: `main` still holds the arXiv v7 paper, which uses outputs of
  the scripts this plan deletes (S4a counts tables, SA01 risk plots, SA04 word
  count, SA06 journal decay). Deleting them there would leave `main` unable to
  rebuild its own paper.
- The `draft/` prefix keeps the branch local; the pre-push hook refuses
  `draft/*`. That is right because it carries the unpushed draft commits.
- Merge `draft/code-cleanup` back into `draft/separate-papers` when Phase 5
  passes. Keep paper edits on `draft/separate-papers` meanwhile, and merge it
  into `draft/code-cleanup` if the exhibit list changes.
- When the draft goes to `main`, keep the code-cleanup commits separate from
  the paper squash (or at least keep the `git mv` rename commit on its own),
  so file history survives the renames.
- The deleted scripts remain in `main`'s history and in the v7 paper source.
  If the theory material becomes a separate paper built from this repo,
  recover them from there.

## Phase 1: delete what the draft does not use

1. `git rm` `S4a_DataCounts.R` and `Appendices/SA01`, `SA02`, `SA04`, `SA05`,
   `SA06`.
2. `git rm` the inputs only they used: `DataIntermediate/TextClassification.csv`,
   and the unreferenced `DataIntermediate/{anom_words,risk_words,text_with_pdf_name}.csv`
   and `DataInput/SignalsTheoryChecked_withPV.csv`.
3. Remove those scripts from `S4_Heterogeneity.R` and `SA_Appendices.R`.
4. Remove the two orphans from `paper/exhibits/`
   (`./check-exhibits.sh` should then report no orphans).
5. Check that the helpers still have callers:
   `grep -rn '<function>' code --include='*.R'` for each helper function the
   retired scripts used. Drop helpers left with no caller.

Verify: `Rscript SA_Appendices.R --preflight-only`, then
`Rscript S4_Heterogeneity.R` and `Rscript SA_Appendices.R`.

## Phase 2: trim stray outputs from the kept scripts

Each item removes a write whose output the draft does not use. None of them
changes a paper exhibit.

- `S2a`: keep the calendar-SE figure. Drop the other `Fig_DM_*` variants
  unless the slides or an appendix need them. Check `LatexPreview/exhibits.tex`
  first.
- `S2d`: drop `theme_ez_slides.tex` and `temp.tex`.
- `SA03`, `SA12`: drop `temp.tex`. SA12: drop `theme_corr_academic.tex`
  (`LatexPreview/exhibits.tex` still references it, so update that too).
- `S4b`: stop writing the theory/model and `AnyModelVsNoModel` fragments.
  Update `tests/test_s4b_phase2_expected.R` to match.
- `S5a`: drop `inspect-realestate.tex`. Keep `InspectMatch.xlsx`, which
  `write_tex_summary()` reads.
- `TimeFERobustness/exhibits.R`: check whether it still writes the six
  per-panel tables alongside `timefe-robustness.tex`. If it does, and nothing
  reads them, drop them.

Verify: snapshot `../Results/` before and after, and diff every file the paper
references. They should all be byte-identical, apart from timestamp comments.

## Phase 3: reorganize the drivers around the new sections

Align the driver names with the paper so that "which script builds Section N"
is obvious.

| Current | Proposed | Notes |
| --- | --- | --- |
| `S2_ResearchVsDataMining.R` + `S2a/b/d/e` | unchanged | Section 2 still matches |
| `S3_Learning.R` + `S3a/b` | unchanged | Section 3 still matches |
| `S4_Heterogeneity.R` + `S4b` | `S4_Quality.R` + `S4a_ByJournal.R` | journal-ranking table only |
| `S5_BestPredictors.R` + `S5a` | folded into `S4_Quality.R` as `S4b_RenownedMatches.R` | Section 4.2 in the draft |
| `SA_Appendices.R` | unchanged name, shorter list | SA03, SA12, SA13, SA14, plus the SA11 span plots |
| `SA_AppendicesPCA.R` | deleted | its four scripts move as below |

Keep the `SAnn` numbers of the surviving appendix scripts. Renumbering them
to match appendix order would churn history for little gain, and the appendix
order is still moving.

### Move SA11 correlation/PCA into precompute

`DM_pca.tex` (main-text Table 1, Panel B) comes from
`SA11_DMCorrelationsPCATables.R`, which today runs only under the optional
`appendices_pca` stage (about an hour). Split the four SA11 scripts the same
way the rest of the pipeline splits caches from exhibits:

| Current | New | Stage |
| --- | --- | --- |
| `Appendices/SA11_DMCorrelationsPCAPrep.R` | `3d_DMCorrelationsPCA.R` | precompute |
| `Appendices/SA11_DMSpanPCAPrep.R` | `3e_DMSpanPCA.R` | precompute |
| `Appendices/SA11_DMCorrelationsPCATables.R` | `S2c_DMCorrelationsPCATables.R` | exhibits (S2), since it writes Table 1b; it also writes IA `quantilesCorDM.tex` |
| `Appendices/SA11_DMSpanPCAPlots.R` | `Appendices/SA11_DMSpanPCAPlots.R` (unchanged) | exhibits (`SA_Appendices.R`) |

Steps:

1. `git mv` the three moved scripts. Update their headers ("How to run"
   currently points at `SA_AppendicesPCA.R").
2. Add `run_script("3d_...")` and `run_script("3e_...")` to the end of
   `3_Precompute.R`, after `3c`. Keep them as separate subprocesses so each
   returns its memory. `3e` is the heaviest step in the pipeline (see
   `docs/runtimes_and_ram.md`), so run it last.
3. Add `S2c` to `S2_ResearchVsDataMining.R` and add
   `dm_correlation_quantiles.RDS` and `dm_pca_table.RDS` to its
   `required_files`. Add `SA11_DMSpanPCAPlots.R` to `SA_Appendices.R`, with
   `dm_span_analysis.RDS` in its `required_files`.
4. Delete `SA_AppendicesPCA.R`, the `appendices_pca` switch in `config.R`, and
   its block in `MAIN.R`.
5. The precompute stage grows from ~40-55 min to ~1h45. Its peak memory
   should not change: the SA11 prep already runs as its own process at the same
   `num_cores`. Confirm this on the first full run and update
   `docs/runtimes_and_ram.md`.

Renames touch `MAIN.R`, the drivers' `run_script()` lists, the script headers,
`docs/exhibit_map.md`, `docs/runtimes_and_ram.md`, and the README. Do them as a
single `git mv` commit, separate from any content change, so history follows
the files.

## Phase 4: docs

- Rewrite `code/docs/exhibit_map.md` for the new exhibit numbering. The
  current map still lists v7 exhibits: Tab 5 counts, Tab 6 theory/model,
  IA risk plots, word count, journal decay, and the hand-transcribed IA.7.
  `DM_pca.tex` is now main-text Table 1b, and `timefe-robustness.tex` is
  wired into the appendix.
- Update the `MAIN.R` header, the `SA_Appendices.R` header, and the README
  pipeline description.
- Update `LatexPreview/exhibits.tex` to the draft's exhibit list.
- Refresh `docs/intermediate-data-inventory.md`, where it names retired
  consumers.

## Phase 5: full verification

1. Run `Rscript MAIN.R` with the default stages (precompute, exhibits,
   time-FE robustness). The full precompute is needed once to confirm that
   `3d`/`3e` write the caches `S2c` and the SA11 plots read.
2. Run `paper/check-exhibits.sh --sync`, then `paper/build.sh`. Confirm the
   PDF compiles with no undefined references.
3. Run the `code/tests/` scripts.

## Out of scope, noticed in passing

- In `paper/research_vs.tex`, the model section still references
  `sec:hetero`, which the draft no longer defines.
- The abstract and conclusion still describe the theory-category results that
  the draft dropped.

Both are paper edits, not code edits. They are listed here so they are not
lost.
