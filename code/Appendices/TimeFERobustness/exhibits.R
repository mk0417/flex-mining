# Render the LaTeX exhibits of the time-FE robustness appendix, then check
# them and the results behind them.
#
# How to run: from flex-mining/, normally through
#   Rscript Appendices/TimeFERobustness/run.R tables
# Inputs:  the regression CSVs under
#          ../Data/Processed/TimeFERobustness/output
# Outputs: one TeX fragment under ../Results/TimeFERobustness
#            - timefe-robustness.tex     (the single merged robustness table)
#
# The merged table folds what used to be five separate exhibits (the S6
# summary, the CZ alternative-construction longtable, and the MP/JKP/CZ
# normalization and replication tables) into one panelled longtable. Its panels
# run from the constructions closest to the original papers (MP and CZ's own
# portfolios), out through alternative CZ portfolio constructions, and finally
# to the JKP-style terciles that sit furthest from either source paper.
#
# The checks at the end run in the same process because they verify exactly
# what was just written, and read the same result tables.

source("Appendices/TimeFERobustness/setup.R")

exhibit_dir <- timefeSettings$paths$exhibits
result_files <- setNames(
  file.path(timefeSettings$paths$output, c(
    "mp-regressions.csv", "jkp-regressions.csv", "cz-alternative-specs.csv",
    "cz-signal-based-regressions.csv"
  )),
  c("mp", "jkp", "alt", "signal")
)

# Table-only choices live here. Analysis stages use stable internal names;
# labels, units, captions, panel order, and rounding are presentation details.
tableSettings <- list(
  labels = list(
    fixed_effects = c(
      predictor = "Predictor FE",
      `predictor + month` = "Predictor + time FE"
    ),
    units = c(bps_per_month = "bp/month"),
    # Panel labels for the constructions that do not come from the CZ
    # alternative-specification file (which carries its own labels below).
    merged_panels = c(
      mp = "MP published sample",
      cz_original = "CZ original portfolios",
      jkp_ew = "JKP equal-weighted terciles",
      cz_terciles = "CZ reconstructed as equal-weighted terciles",
      cz_terciles_matched = "CZ terciles, JKP-metadata-matched signals"
    ),
    # The three ruled blocks of the merged table, ordered from the portfolios
    # closest to the source papers to the JKP-style terciles furthest from them.
    blocks = c(
      "Exact published portfolios",
      "Alternative CZ portfolio constructions",
      "Rebuilt as JKP-style terciles"
    ),
    alternative_panels = c(
      op = "Original portfolios",
      deciles_ew = "Deciles, equal weighted",
      deciles_vw = "Deciles, value weighted",
      quintiles_ew = "Quintiles, equal weighted",
      quintiles_vw = "Quintiles, value weighted",
      ex_nyse_p20_me = "Exclude below NYSE 20th pct. size",
      nyse = "NYSE stocks only",
      ex_price5 = "Exclude price below $5"
    ),
    replication_rows = c(cz = "CZ terciles")
  ),
  tables = list(
    merged = list(
      filename = "timefe-robustness.tex",
      digits = 1L
    )
  )
)

check(
  all(file.exists(result_files)),
  paste0("Missing %s. Run `Rscript Appendices/TimeFERobustness/run.R ",
         "build` before generating the LaTeX exhibits."),
  paste(result_files[!file.exists(result_files)], collapse = ", ")
)

mp <- fread(result_files[["mp"]])
jkp <- fread(result_files[["jkp"]])
alt <- fread(result_files[["alt"]])
signal_results <- fread(result_files[["signal"]])

fixed_effect_label <- function(id) {
  label <- unname(tableSettings$labels$fixed_effects[id])
  check(length(label) == 1L && !is.na(label),
        "No table label is configured for fixed effects '%s'.", id)
  label
}

result_cell <- function(estimate, standard_error, digits, se_approx = FALSE) {
  estimate_text <- formatC(estimate, digits = digits, format = "f")
  if (is.na(standard_error)) {
    return(paste0(estimate_text, " (SE unavailable)"))
  }
  se_text <- formatC(standard_error, digits = digits, format = "f")
  if (isTRUE(se_approx)) {
    se_text <- paste0("$\\approx ", se_text, "$")
  }
  paste0(estimate_text, " (", se_text, ")")
}

select_row <- function(data, specification, fixed_effects, label) {
  spec_value <- specification
  fe_value <- fixed_effects
  row <- data[
    specification == spec_value & fixed_effects == fe_value
  ]
  check(nrow(row) == 1L,
        "Expected one row for specification '%s' and FE '%s'; found %d.",
        specification, fixed_effects, nrow(row))
  row[, display_label := label]
  row
}

# Put coefficients estimated in percent per month onto the same "in-sample
# mean = 100" scale as the scaled specifications, by dividing through the
# normalization mean the analysis stage recorded with the row.
normalize_raw_rows <- function(rows) {
  scaled_cols <- c(
    "post_sample", "post_sample_se", "additional_post_publication",
    "additional_post_publication_se", "total_post_publication_change",
    "total_post_publication_change_se"
  )
  check(
    "normalization_mean_bps" %in% names(rows) &&
      all(is.finite(rows$normalization_mean_bps)),
    "Raw rows lack an in-sample normalization mean."
  )
  rows[, (scaled_cols) := lapply(
    .SD, function(value) 1e4 * value / normalization_mean_bps
  ), .SDcols = scaled_cols]
  rows
}

# ---------------------------------------------------------------------------
# Assemble the rows behind each panel of the merged table.
# ---------------------------------------------------------------------------

# MP published sample: percent-per-month coefficients rescaled onto the
# in-sample-mean-100 scale, with the approximate time-FE standard error the
# published tables report.
mp_normalized <- normalize_raw_rows(rbindlist(list(
  select_row(
    mp, "mp_published_table_ii_col1", "predictor",
    fixed_effect_label("predictor")
  ),
  select_row(
    mp, "mp_published_table_iii_col4", "predictor + month",
    fixed_effect_label("predictor + month")
  )
)))
mp_normalized[
  fixed_effects == "predictor + month",
  additional_post_publication_se := 100 * 0.122 / 0.582
]
mp_normalized[
  , additional_post_publication_se_approx :=
    fixed_effects == "predictor + month"
]

# JKP quality-screened equal-weighted terciles baseline.
jkp_benchmark <- jkp[
  specification == "baseline_quality_t2",
  .(
    fixed_effects, post_sample, post_sample_se,
    additional_post_publication, additional_post_publication_se,
    mean_in_sample_bps, factors
  )
]

# CZ signal-level terciles rebuilt to mirror the JKP construction.
two_fe_rows <- function(data, specification, dataset_label) {
  rows <- rbindlist(lapply(
    c("predictor", "predictor + month"),
    function(fe) select_row(
      data, specification, fe, fixed_effect_label(fe)
    )
  ), use.names = TRUE, fill = TRUE)
  rows[, display_label := paste0(dataset_label, ", ", display_label)]
  rows[]
}

cz_rep_rows <- function(weighting_id, specification) {
  wanted_weighting <- weighting_id
  rows <- two_fe_rows(
    signal_results[weighting_id == wanted_weighting],
    specification,
    tableSettings$labels$replication_rows[["cz"]]
  )
  rows[]
}

# ---------------------------------------------------------------------------
# Merged longtable.
# ---------------------------------------------------------------------------

# Reduce any panel's rows to the common column set the merged writer expects,
# ordering the two fixed-effect rows and re-deriving their row labels so every
# panel reads identically regardless of where its numbers came from.
merged_panel <- function(rows, panel_label, block) {
  dt <- copy(rows)
  dt[, fe_order := match(fixed_effects, c("predictor", "predictor + month"))]
  check(!anyNA(dt$fe_order) && nrow(dt) == 2L,
        "Merged panel '%s' must contain exactly the two FE rows.", panel_label)
  setorder(dt, fe_order)
  dt[, display_label := vapply(fixed_effects, fixed_effect_label, character(1))]
  if (!"additional_post_publication_se_approx" %in% names(dt)) {
    dt[, additional_post_publication_se_approx := FALSE]
  }
  dt[, `:=`(panel_label = panel_label, block = block)]
  dt[, .(
    block, panel_label, fe_order, display_label,
    post_sample, post_sample_se,
    additional_post_publication, additional_post_publication_se,
    additional_post_publication_se_approx,
    mean_in_sample_bps, factors
  )]
}

merged_labels <- tableSettings$labels$merged_panels
alt_labels <- tableSettings$labels$alternative_panels

alt_panel <- function(name, block) {
  merged_panel(alt[data_name == name], unname(alt_labels[[name]]), block)
}

merged_rows <- rbindlist(list(
  # Block 1: the exact portfolios published by MP and CZ.
  merged_panel(mp_normalized, merged_labels[["mp"]], 1L),
  merged_panel(alt[data_name == "op"], merged_labels[["cz_original"]], 1L),
  # Block 2: alternative CZ portfolio constructions.
  alt_panel("deciles_ew", 2L),
  alt_panel("deciles_vw", 2L),
  alt_panel("quintiles_ew", 2L),
  alt_panel("quintiles_vw", 2L),
  alt_panel("ex_nyse_p20_me", 2L),
  alt_panel("nyse", 2L),
  alt_panel("ex_price5", 2L),
  # Block 3: JKP-style terciles, furthest from the original papers.
  merged_panel(jkp_benchmark, merged_labels[["jkp_ew"]], 3L),
  merged_panel(
    cz_rep_rows("ew", "baseline_quality_t2"),
    merged_labels[["cz_terciles"]], 3L
  ),
  merged_panel(
    cz_rep_rows("ew", "baseline_quality_t2_metadata_matched"),
    merged_labels[["cz_terciles_matched"]], 3L
  )
), use.names = TRUE)
merged_rows[, panel_order := rleid(panel_label)]

escape_tex <- function(text) gsub("\\$", "\\\\$", text)

write_merged_longtable <- function(config, rows, blocks, note) {
  digits <- config$digits
  unit <- tableSettings$labels$units[["bps_per_month"]]
  header_row <- paste0(
    "Data and specification & Post-sample & ",
    "\\shortstack{Additional\\\\post-publication} & ",
    "\\shortstack{Mean in-sample\\\\return (", unit, ")} & Signals \\\\"
  )
  cell <- function(estimate, se, approx) {
    result_cell(estimate, se, digits, se_approx = approx)
  }

  panel_ids <- rows[, unique(panel_order)]
  block_first <- rows[, .(first_panel = min(panel_order)), by = block]

  body <- unlist(lapply(panel_ids, function(pid) {
    panel_rows <- rows[panel_order == pid]
    blk <- panel_rows$block[1L]
    is_block_start <- pid == block_first[block == blk, first_panel]
    out <- character(0)
    if (is_block_start) {
      if (blk > 1L) out <- c(out, "\\addlinespace[0.4em]", "\\midrule")
      out <- c(
        out,
        paste0("\\multicolumn{5}{@{}l}{\\textbf{", blocks[blk], "}} \\\\"),
        "\\addlinespace[0.15em]"
      )
    } else {
      out <- c(out, "\\addlinespace[0.35em]")
    }
    out <- c(out, paste0(
      "\\multicolumn{5}{@{}l}{\\textit{Panel ", LETTERS[pid], ": ",
      escape_tex(panel_rows$panel_label[1L]), "}} \\\\"
    ))
    data_lines <- vapply(seq_len(nrow(panel_rows)), function(i) {
      paste0(
        panel_rows$display_label[i], " & ",
        cell(panel_rows$post_sample[i], panel_rows$post_sample_se[i], FALSE),
        " & ",
        cell(
          panel_rows$additional_post_publication[i],
          panel_rows$additional_post_publication_se[i],
          panel_rows$additional_post_publication_se_approx[i]
        ), " & ",
        formatC(panel_rows$mean_in_sample_bps[i], digits = 1L, format = "f"),
        " & ", panel_rows$factors[i],
        " \\\\"
      )
    }, character(1))
    c(out, data_lines)
  }), use.names = FALSE)

  lines <- c(
    "\\begingroup",
    "\\small",
    "\\begin{longtable}{@{}lcccc@{}}",
    "\\toprule",
    header_row,
    "\\midrule",
    "\\endfirsthead",
    paste0(
      "\\multicolumn{5}{c}{\\tablename\\ \\thetable{} -- continued from ",
      "previous page} \\\\"
    ),
    "\\toprule",
    header_row,
    "\\midrule",
    "\\endhead",
    "\\midrule",
    "\\multicolumn{5}{r}{Continued on next page} \\\\",
    "\\endfoot",
    "\\bottomrule",
    "\\endlastfoot",
    body,
    "\\end{longtable}",
    paste0("{\\footnotesize\\emph{Note:} ", note, "\\par}"),
    "\\endgroup"
  )
  writeLines(lines, file.path(exhibit_dir, config$filename), useBytes = TRUE)
}

merged_note <- paste0(
  "Each panel reports two rows, predictor fixed effects and predictor plus ",
  "time fixed effects. Columns give the post-sample change and the additional ",
  "post-publication change in mean returns (standard errors in parentheses), ",
  "and the mean in-sample long-short return. All rows except MP are scaled by ",
  "each signal's in-sample mean return; the MP rows are scaled by the grand ",
  "mean across signals. $\\approx$ marks an approximate standard error."
)

merged_config <- tableSettings$tables$merged
write_merged_longtable(
  merged_config,
  merged_rows,
  tableSettings$labels$blocks,
  merged_note
)

message("Saved LaTeX exhibit to ", exhibit_dir)

# =========================================================================
# Checks
# =========================================================================
#
# These are structural invariants of the generated results and exhibits -- no
# duplicate rows, complete panels, a one-to-one crosswalk, non-empty exhibit
# files -- so they hold across OpenAP releases. Exact factor counts are not
# pinned here: they move with every data vintage, and the numbers themselves
# are already guarded upstream (schema, sign, and quintile-correlation audits
# in estimate.R and cz_terciles.R).

output_dir <- timefeSettings$paths$output

# The regression tables are already in hand from the rendering above.
jkp_results <- jkp
cz_signal_results <- signal_results
check(
  jkp_results[, .N, by = .(specification, fixed_effects)][N > 1L, .N] == 0L,
  "JKP results contain duplicate specification/fixed-effect rows."
)
check(
  !any(startsWith(jkp_results$specification, "matched_")),
  "Deprecated matched-sample specifications remain in JKP results."
)
check(
  "normalization_mean_bps" %in% names(jkp_results) &&
    jkp_results[
      specification == "baseline_quality_t2_grand_mean_scaled",
      all(is.finite(normalization_mean_bps))
    ],
  "JKP grand-mean results lack numeric normalization metadata."
)
check(
  all(c("weighting_id", "mean_in_sample_bps") %in%
        names(cz_signal_results)),
  "CZ signal results lack weighting or in-sample-mean metadata."
)
check(
  cz_signal_results[
    , .N, by = .(weighting_id, specification, fixed_effects)
  ][N > 1L, .N] == 0L,
  "CZ signal results contain duplicate weighting/specification/FE rows."
)
check(
  setequal(unique(cz_signal_results$weighting_id), c("ew", "vw_cap", "vw")),
  "CZ signal results do not contain EW, capped-VW, and VW constructions."
)
replication_specs <- data.table(
  weighting_id = c("ew", "ew", "vw_cap", "vw"),
  specification = c(
    "baseline_quality_t2",
    "baseline_quality_t2_grand_mean_scaled",
    "baseline_quality_t2",
    "baseline_quality_t2"
  )
)
replication_rows <- merge(
  cz_signal_results,
  replication_specs,
  by = c("weighting_id", "specification")
)
check(
  nrow(replication_rows) == 8L &&
    setequal(
      unique(replication_rows$fixed_effects),
      c("predictor", "predictor + month")
    ) &&
    all(is.finite(replication_rows$mean_in_sample_bps)) &&
    all(is.finite(replication_rows$min_in_sample_mean_bps)),
  "The four-panel JKP replication lacks complete CZ target rows."
)

matched_pairs <- fread(file.path(output_dir, "jkp-cz-matched-pairs.csv"))
matched_signal_rows <- cz_signal_results[
  weighting_id == "ew" &
    specification == "baseline_quality_t2_metadata_matched"
]
check(
  nrow(matched_pairs) > 0L &&
    uniqueN(matched_pairs$signalname) == nrow(matched_pairs) &&
    uniqueN(matched_pairs$cz_signalname) == nrow(matched_pairs),
  "The metadata-matched JKP-CZ crosswalk must be a non-empty one-to-one map."
)
check(
  nrow(matched_signal_rows) == 2L &&
    setequal(
      matched_signal_rows$fixed_effects,
      c("predictor", "predictor + month")
    ) &&
    all(is.finite(matched_signal_rows$mean_in_sample_bps)) &&
    all(is.finite(matched_signal_rows$min_in_sample_mean_bps)),
  "The metadata-matched CZ signal-level rows are missing or incomplete."
)

merged_file <- file.path(exhibit_dir, "timefe-robustness.tex")
check(file.exists(merged_file), "The merged robustness exhibit is missing.")
check(file.info(merged_file)$size > 0L, "The merged robustness exhibit is empty.")

merged_exhibit <- readLines(
  file.path(exhibit_dir, "timefe-robustness.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-L]:", merged_exhibit)) == 12L &&
    !any(grepl("Panel M:", merged_exhibit)) &&
    sum(startsWith(merged_exhibit, "Predictor FE &")) == 12L &&
    sum(startsWith(merged_exhibit, "Predictor + time FE &")) == 12L,
  "The merged robustness table must contain exactly twelve two-row panels."
)
check(
  all(vapply(
    tableSettings$labels$blocks,
    function(header) any(grepl(header, merged_exhibit, fixed = TRUE)),
    logical(1)
  )),
  "The merged robustness table must contain its three block headers."
)

message("Result structure and generated exhibits check out.")
