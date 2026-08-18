# Render the LaTeX exhibits of the time-FE robustness appendix, then check
# them and the results behind them.
#
# How to run: from flex-mining/, normally through
#   Rscript Appendices/TimeFERobustness/run.R tables
# Inputs:  the regression and date-comparison CSVs under
#          ../Data/Processed/TimeFERobustness/output
# Outputs: six TeX fragments under ../Results/TimeFERobustness
#
# The checks at the end run in the same process because they verify exactly
# what was just written, and read the same result tables.

source("Appendices/TimeFERobustness/setup.R")

exhibit_dir <- timefeSettings$paths$exhibits
result_files <- setNames(
  file.path(timefeSettings$paths$output, c(
    "mp-regressions.csv", "jkp-regressions.csv", "cz-alternative-specs.csv",
    "jkp-alternative-specs.csv", "jkp-cz-full-date-comparison.csv",
    "cz-signal-based-regressions.csv"
  )),
  c("mp", "jkp", "alt", "jkp_alt", "dates", "signal")
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
    mp_cz_panels = c(
      mp = "Panel A: MP, scaled by grand mean",
      cz_signal = "Panel B: CZ, scaled by signal mean",
      cz_grand = "Panel C: CZ, scaled by grand mean",
      cz_mp = "Panel D: CZ, MP signals, scaled by grand mean"
    ),
    jkp_panels = c(
      jkp_signal = "Panel A: JKP equal weighted, scaled by signal mean",
      jkp_grand = "Panel B: JKP equal weighted, scaled by grand mean",
      jkp_cap = paste0(
        "Panel C: JKP capped value weighted, scaled by signal mean"
      ),
      jkp_value = "Panel D: JKP value weighted, scaled by signal mean"
    ),
    jkp_rep_panels = c(
      ew_signal = paste0(
        "Panel A: Equal weighted, scaled by signal mean"
      ),
      ew_grand = paste0(
        "Panel B: Equal weighted, scaled by grand mean"
      ),
      vw_cap_signal = paste0(
        "Panel C: Capped value weighted, scaled by signal mean"
      ),
      vw_signal = paste0(
        "Panel D: Value weighted, scaled by signal mean"
      ),
      metadata_matched = paste0(
        "Panel E: Equal weighted, metadata-matched signals, ",
        "scaled by signal mean"
      )
    ),
    alternative_panels = c(
      jkp_baseline = "JKP quality-screened baseline",
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
    mp_cz = list(
      filename = "mp-cz-normalized.tex",
      caption = "MP and quality-screened CZ results, normalized returns",
      tex_label = "tab:mp-cz-normalized",
      digits = 1L
    ),
    jkp = list(
      filename = "jkp-cz-normalized.tex",
      caption = "Quality-screened JKP weighting and normalization results",
      tex_label = "tab:jkp-cz-normalized",
      digits = 1L
    ),
    alternatives = list(
      filename = "cz-alternative-specs.tex",
      caption = paste0(
        "Post-sample decay across alternative CZ portfolio constructions"
      ),
      tex_label = "tab:cz-alternative-specs",
      digits = 1L
    ),
    jkp_rep_using_cz = list(
      filename = "jkp-rep-using-cz.tex",
      caption = paste0(
        "Replication of JKP portfolio constructions using ",
        "quality-screened CZ signal-level terciles"
      ),
      tex_label = "tab:jkp-rep-using-cz",
      digits = 1L
    ),
    s6_summary = list(
      filename = "s6-timefe-summary.tex",
      caption = paste0(
        "Robustness of post-publication decay to time fixed effects"
      ),
      digits = 1L
    ),
    dates = list(
      filename = "jkp-cz-date-comparison.tex",
      caption = paste0(
        "Date metadata and between-period lengths in quality-screened ",
        "JKP and CZ libraries"
      ),
      tex_label = "tab:jkp-cz-date-comparison"
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
jkp_alt <- fread(result_files[["jkp_alt"]])
date_comparison <- fread(result_files[["dates"]])
signal_results <- fread(result_files[["signal"]])

fixed_effect_label <- function(id) {
  label <- unname(tableSettings$labels$fixed_effects[id])
  check(length(label) == 1L && !is.na(label),
        "No table label is configured for fixed effects '%s'.", id)
  label
}

result_cell <- function(estimate, standard_error, digits, se_approx = FALSE,
                        siunitx = FALSE) {
  estimate_text <- formatC(estimate, digits = digits, format = "f")
  if (is.na(standard_error)) {
    return(paste0(estimate_text, " (SE unavailable)"))
  }
  se_text <- formatC(standard_error, digits = digits, format = "f")
  if (isTRUE(se_approx)) {
    se_text <- paste0("$\\approx ", se_text, "$")
  }
  if (siunitx) {
    uncertainty_digits <- formatC(
      round(standard_error * 10^digits),
      digits = 0L,
      format = "f"
    )
    return(paste0(estimate_text, "(", uncertainty_digits, ")"))
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

write_exhibit <- function(filename, caption, label, rows, digits,
                          panel_column = NULL, align_numbers = FALSE) {
  in_sample_cell <- function(value) {
    if (is.na(value)) "--" else formatC(value, digits = 1L, format = "f")
  }
  row_line <- function(i) {
    paste0(
      rows$display_label[i], " & ",
      result_cell(
        rows$post_sample[i], rows$post_sample_se[i], digits,
        siunitx = align_numbers
      ), " & ",
      result_cell(
        rows$additional_post_publication[i],
        rows$additional_post_publication_se[i],
        digits,
        if ("additional_post_publication_se_approx" %in% names(rows)) {
          rows$additional_post_publication_se_approx[i]
        } else {
          FALSE
        },
        siunitx = align_numbers
      ), " & ",
      formatC(rows$mean_in_sample_bps[i], digits = 1L, format = "f"),
      " & ", in_sample_cell(rows$min_in_sample_mean_bps[i]),
      " & ", rows$factors[i],
      " \\\\"
    )
  }
  if (is.null(panel_column)) {
    body <- vapply(seq_len(nrow(rows)), row_line, character(1))
  } else {
    panels <- unique(rows[[panel_column]])
    body <- unlist(lapply(seq_along(panels), function(panel_index) {
      panel <- panels[panel_index]
      panel_rows <- which(rows[[panel_column]] == panel)
      c(
        if (panel_index > 1L) "\\addlinespace[0.35em]",
        paste0(
          "\\multicolumn{6}{@{}l}{\\textit{", panel, "}} \\\\"
        ),
        vapply(panel_rows, row_line, character(1))
      )
    }), use.names = FALSE)
  }

  column_header <- if (align_numbers) {
    paste0(
      "Data and specification & {Post-sample} & ",
      "{\\shortstack{Additional\\\\post-publication}} & ",
      "\\multicolumn{2}{c}{In-Sample Return} & {Signals} \\\\"
    )
  } else {
    paste0(
      "Data and specification & Post-sample & ",
      "\\shortstack{Additional\\\\post-publication} & ",
      "\\multicolumn{2}{c}{In-Sample Return} & Signals \\\\"
    )
  }
  subheader <- if (align_numbers) {
    " & {} & {} & {Mean} & {Min} & {} \\\\"
  } else {
    " & & & Mean & Min & \\\\"
  }

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", label, "}"),
    "\\small",
    if (align_numbers) {
      paste0(
        "\\begin{tabular}{@{}l ",
        "S[table-format=-2.1(3),uncertainty-mode=full] ",
        "S[table-format=-2.1(3),uncertainty-mode=full] ",
        "S[table-format=2.1] S[table-format=2.1] ",
        "S[table-format=3.0]@{}}"
      )
    } else {
      "\\begin{tabular}{@{}lccccc@{}}"
    },
    "\\toprule",
    column_header,
    "\\cmidrule(lr){4-5}",
    subheader,
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}"
  )
  lines <- c(lines, "\\end{table}")
  writeLines(lines, file.path(exhibit_dir, filename), useBytes = TRUE)
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
mp_normalized[, panel_label := tableSettings$labels$mp_cz_panels[["mp"]]]

cz_all_signal_normalized <- rbindlist(list(
  select_row(
    jkp, "cz_all_scaled_reference", "predictor",
    fixed_effect_label("predictor")
  ),
  select_row(
    jkp, "cz_all_scaled_reference", "predictor + month",
    fixed_effect_label("predictor + month")
  )
))
cz_all_signal_normalized[
  , panel_label := tableSettings$labels$mp_cz_panels[["cz_signal"]]
]

cz_all_grand_normalized <- rbindlist(list(
  select_row(
    jkp, "cz_all_grand_mean_scaled_reference", "predictor",
    fixed_effect_label("predictor")
  ),
  select_row(
    jkp, "cz_all_grand_mean_scaled_reference", "predictor + month",
    fixed_effect_label("predictor + month")
  )
))
cz_all_grand_normalized[
  , panel_label := tableSettings$labels$mp_cz_panels[["cz_grand"]]
]

cz_mp_normalized <- normalize_raw_rows(rbindlist(list(
  select_row(
    mp, "cz_mp_matched_2013_unscaled_pub_dec", "predictor",
    fixed_effect_label("predictor")
  ),
  select_row(
    mp, "cz_mp_matched_2013_unscaled_pub_dec", "predictor + month",
    fixed_effect_label("predictor + month")
  )
)))
cz_mp_normalized[
  , panel_label := tableSettings$labels$mp_cz_panels[["cz_mp"]]
]

mp_cz_normalized <- rbindlist(
  list(
    mp_normalized, cz_all_signal_normalized,
    cz_all_grand_normalized, cz_mp_normalized
  ),
  use.names = TRUE, fill = TRUE
)
mp_cz_config <- tableSettings$tables$mp_cz
write_exhibit(
  mp_cz_config$filename,
  mp_cz_config$caption,
  mp_cz_config$tex_label,
  mp_cz_normalized,
  digits = mp_cz_config$digits,
  panel_column = "panel_label"
)

jkp_normalized <- rbindlist(list(
  select_row(
    jkp, "baseline_quality_t2", "predictor",
    fixed_effect_label("predictor")
  ),
  select_row(
    jkp, "baseline_quality_t2", "predictor + month",
    fixed_effect_label("predictor + month")
  )
))
jkp_normalized[
  , panel_label := tableSettings$labels$jkp_panels[["jkp_signal"]]
]

jkp_grand_mean_normalized <- rbindlist(list(
  select_row(
    jkp, "baseline_quality_t2_grand_mean_scaled", "predictor",
    fixed_effect_label("predictor")
  ),
  select_row(
    jkp, "baseline_quality_t2_grand_mean_scaled", "predictor + month",
    fixed_effect_label("predictor + month")
  )
))
jkp_grand_mean_normalized[
  , panel_label := tableSettings$labels$jkp_panels[["jkp_grand"]]
]

jkp_weighting_normalized <- copy(
  jkp_alt[data_name %in% c("vw_cap", "vw")]
)
jkp_weighting_normalized[, display_label := fifelse(
  fixed_effects == "predictor",
  fixed_effect_label("predictor"),
  fixed_effect_label("predictor + month")
)]
jkp_weighting_normalized[, panel_label := fifelse(
  data_name == "vw_cap",
  tableSettings$labels$jkp_panels[["jkp_cap"]],
  tableSettings$labels$jkp_panels[["jkp_value"]]
)]

jkp_table_rows <- rbindlist(
  list(
    jkp_normalized, jkp_grand_mean_normalized,
    jkp_weighting_normalized
  ),
  use.names = TRUE, fill = TRUE
)
jkp_config <- tableSettings$tables$jkp
write_exhibit(
  jkp_config$filename,
  jkp_config$caption,
  jkp_config$tex_label,
  jkp_table_rows,
  digits = jkp_config$digits,
  panel_column = "panel_label"
)

jkp_benchmark <- jkp[
  specification == "baseline_quality_t2",
  .(
    fixed_effects, post_sample, post_sample_se,
    additional_post_publication, additional_post_publication_se,
    mean_in_sample_bps, factors
  )
]
jkp_benchmark[, `:=`(
  data_name = "jkp_baseline",
  label = tableSettings$labels$alternative_panels[["jkp_baseline"]]
)]

alt_long <- rbindlist(
  list(jkp_benchmark, alt),
  use.names = TRUE,
  fill = TRUE
)
alt_long[, label := unname(tableSettings$labels$alternative_panels[data_name])]
check(!anyNA(alt_long$label),
      "At least one alternative portfolio lacks a configured table label.")
panel_order <- c("jkp_baseline", unique(alt$data_name))
alt_long[, panel_order := match(data_name, panel_order)]
alt_long[, fe_order := match(
  fixed_effects, c("predictor", "predictor + month")
)]
setorder(alt_long, panel_order, fe_order)

alt_cell <- function(estimate, standard_error) {
  paste0(
    formatC(
      estimate,
      digits = tableSettings$tables$alternatives$digits,
      format = "f"
    ), " (",
    formatC(
      standard_error,
      digits = tableSettings$tables$alternatives$digits,
      format = "f"
    ), ")"
  )
}

# Reusable writer for a panelled longtable of post-sample/publication
# coefficients across alternative data constructions. `long_data` must
# already be ordered by `panel_order` (1, 2, 3, ...) and carry `label`,
# `fixed_effects`, `post_sample(_se)`, `additional_post_publication(_se)`,
# `mean_in_sample_bps`, and `factors`.
write_alt_longtable <- function(
    filename, caption, tex_label, long_data) {
  n_panels <- max(long_data$panel_order)
  header_row <- paste0(
    "Data and specification & Post-sample & ",
    "\\shortstack{Additional\\\\post-publication} & ",
    "\\shortstack{Mean in-sample\\\\return (",
    tableSettings$labels$units[["bps_per_month"]], ")} & Signals \\\\"
  )
  lines <- c(
    "\\begingroup",
    "\\small",
    "\\begin{longtable}{@{}lcccc@{}}",
    paste0("\\caption{", caption, "}"),
    paste0("\\label{", tex_label, "}\\\\"),
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
    unlist(lapply(seq_len(n_panels), function(panel_index) {
      panel_rows <- long_data[panel_order == panel_index]
      panel_label <- gsub("\\$", "\\\\$", unique(panel_rows$label))
      c(
        if (panel_index > 1L) "\\addlinespace[0.35em]",
        paste0(
          "\\multicolumn{5}{@{}l}{\\textit{Panel ",
          LETTERS[panel_index], ": ", panel_label, "}} \\\\"
        ),
        vapply(seq_len(nrow(panel_rows)), function(i) {
          fe_label <- fixed_effect_label(panel_rows$fixed_effects[i])
          paste0(
            fe_label, " & ",
            alt_cell(
              panel_rows$post_sample[i],
              panel_rows$post_sample_se[i]
            ),
            " & ",
            alt_cell(
              panel_rows$additional_post_publication[i],
              panel_rows$additional_post_publication_se[i]
            ),
            " & ",
            formatC(
              panel_rows$mean_in_sample_bps[i],
              digits = 1L,
              format = "f"
            ),
            " & ", panel_rows$factors[i],
            " \\\\"
          )
        }, character(1))
      )
    }), use.names = FALSE),
    "\\end{longtable}",
    "\\endgroup"
  )
  writeLines(lines, file.path(exhibit_dir, filename), useBytes = TRUE)
}

alternative_config <- tableSettings$tables$alternatives
write_alt_longtable(
  alternative_config$filename,
  alternative_config$caption,
  alternative_config$tex_label,
  alt_long
)

jkp_rep_config <- tableSettings$tables$jkp_rep_using_cz

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

jkp_rep_rows <- rbindlist(list(
  cz_rep_rows("ew", "baseline_quality_t2")[
    , panel_label := tableSettings$labels$jkp_rep_panels[["ew_signal"]]
  ],
  cz_rep_rows("ew", "baseline_quality_t2_grand_mean_scaled")[
    , panel_label := tableSettings$labels$jkp_rep_panels[["ew_grand"]]
  ],
  cz_rep_rows("vw_cap", "baseline_quality_t2")[
    , panel_label := tableSettings$labels$jkp_rep_panels[["vw_cap_signal"]]
  ],
  cz_rep_rows("vw", "baseline_quality_t2")[
    , panel_label := tableSettings$labels$jkp_rep_panels[["vw_signal"]]
  ],
  cz_rep_rows("ew", "baseline_quality_t2_metadata_matched")[
    , panel_label :=
      tableSettings$labels$jkp_rep_panels[["metadata_matched"]]
  ]
), use.names = TRUE, fill = TRUE)

write_exhibit(
  jkp_rep_config$filename,
  jkp_rep_config$caption,
  jkp_rep_config$tex_label,
  jkp_rep_rows,
  digits = jkp_rep_config$digits,
  panel_column = "panel_label",
  align_numbers = TRUE
)

# Compact six-panel summary for slide S6. Exact panels retain regression
# standard errors; the CZ-alternatives panel reports the median and full range
# across all eight portfolio constructions.
s6_config <- tableSettings$tables$s6_summary

s6_bold_if_time_fe <- function(value, fixed_effects) {
  if (identical(fixed_effects, "predictor + month")) {
    paste0("\\textbf{", value, "}")
  } else {
    value
  }
}

s6_exact_panel <- function(data, panel_label) {
  wanted_panel_label <- panel_label
  rows <- copy(data)
  check(nrow(rows) == 2L, "S6 panel '%s' must contain exactly two rows.",
        panel_label)
  rows[, fe_order := match(
    fixed_effects, c("predictor", "predictor + month")
  )]
  check(!anyNA(rows$fe_order), "S6 panel '%s' has an unknown FE label.",
        panel_label)
  setorder(rows, fe_order)

  approximate <- if (
    "additional_post_publication_se_approx" %in% names(rows)
  ) {
    rows$additional_post_publication_se_approx
  } else {
    rep(FALSE, nrow(rows))
  }

  rows[, .(
    panel_label = wanted_panel_label,
    display_label = vapply(fixed_effects, fixed_effect_label, character(1)),
    post_sample_cell = vapply(seq_len(.N), function(i) {
      result_cell(
        post_sample[i], post_sample_se[i], s6_config$digits
      )
    }, character(1)),
    additional_cell = vapply(seq_len(.N), function(i) {
      value <- result_cell(
        additional_post_publication[i],
        additional_post_publication_se[i],
        s6_config$digits,
        se_approx = approximate[i]
      )
      s6_bold_if_time_fe(value, fixed_effects[i])
    }, character(1)),
    signals_cell = as.character(factors)
  )]
}

s6_range_cell <- function(values) {
  sprintf(
    "%.1f [%.1f, %.1f]",
    median(values), min(values), max(values)
  )
}

s6_alternative_rows <- alt[, .(
  post_sample_cell = s6_range_cell(post_sample),
  additional_cell = s6_range_cell(additional_post_publication),
  signals_cell = sprintf("%d--%d", min(factors), max(factors))
), by = fixed_effects]
s6_alternative_rows[, fe_order := match(
  fixed_effects, c("predictor", "predictor + month")
)]
setorder(s6_alternative_rows, fe_order)
s6_alternative_rows[, additional_cell := vapply(
  seq_len(.N),
  function(i) s6_bold_if_time_fe(additional_cell[i], fixed_effects[i]),
  character(1)
)]
s6_alternative_rows[, `:=`(
  panel_label = sprintf(
    "CZ alternative portfolios (%d constructions): median [min, max]",
    uniqueN(alt$data_name)
  ),
  display_label = vapply(fixed_effects, fixed_effect_label, character(1))
)]
s6_alternative_rows <- s6_alternative_rows[, .(
  panel_label, display_label, post_sample_cell,
  additional_cell, signals_cell
)]

s6_rows <- rbindlist(list(
  s6_exact_panel(mp_normalized, "MP published sample"),
  s6_exact_panel(cz_all_signal_normalized, "CZ original portfolios"),
  s6_alternative_rows,
  s6_exact_panel(jkp_normalized, "JKP equal-weighted terciles"),
  s6_exact_panel(
    cz_rep_rows("ew", "baseline_quality_t2"),
    "CZ reconstructed as equal-weighted terciles"
  ),
  s6_exact_panel(
    cz_rep_rows("ew", "baseline_quality_t2_metadata_matched"),
    "CZ terciles, JKP-metadata-matched signals"
  )
), use.names = TRUE)

s6_panel_counts <- s6_rows[, .N, by = panel_label]
check(nrow(s6_panel_counts) <= 6L && all(s6_panel_counts$N == 2L),
      "S6 summary must have at most six panels and two rows per panel.")

write_s6_table <- function(filename, caption, rows) {
  panels <- unique(rows$panel_label)
  body <- unlist(lapply(seq_along(panels), function(panel_index) {
    panel <- panels[panel_index]
    panel_rows <- rows[panel_label == panel]
    c(
      if (panel_index == 4L) {
        "\\midrule"
      } else if (panel_index > 1L) {
        "\\addlinespace[0.3em]"
      },
      paste0(
        "\\multicolumn{4}{@{}l}{\\textit{Panel ",
        LETTERS[panel_index], ": ", panel, "}} \\\\"
      ),
      vapply(seq_len(nrow(panel_rows)), function(i) {
        paste0(
          panel_rows$display_label[i], " & ",
          panel_rows$post_sample_cell[i], " & ",
          panel_rows$additional_cell[i], " & ",
          panel_rows$signals_cell[i], " \\\\"
        )
      }, character(1))
    )
  }), use.names = FALSE)

  lines <- c(
    "\\begin{table}[!htbp]",
    "\\centering",
    paste0("\\textbf{", caption, "}\\par"),
    "\\vspace{0.4em}",
    "\\footnotesize",
    "\\begin{tabular}{@{}lccc@{}}",
    "\\toprule",
    paste0(
      "Fixed effects & Post-sample & ",
      "\\shortstack{Additional\\\\post-publication} & Signals \\\\"
    ),
    "\\midrule",
    body,
    "\\bottomrule",
    "\\end{tabular}",
    "\\end{table}"
  )
  writeLines(lines, file.path(exhibit_dir, filename), useBytes = TRUE)
}

write_s6_table(
  s6_config$filename,
  s6_config$caption,
  s6_rows
)

metadata_cell <- function(field_name, dataset_name) {
  row <- date_comparison[
    field == field_name & dataset == dataset_name
  ]
  paste0(
    formatC(row$mean, digits = 1L, format = "f"), " (",
    formatC(row$median, digits = 0L, format = "f"), ")"
  )
}

between <- date_comparison[field == "between_length"]
date_config <- tableSettings$tables$dates

date_lines <- c(
  "\\begin{table}[!htbp]",
  "\\centering",
  paste0("\\caption{", date_config$caption, "}"),
  paste0("\\label{", date_config$tex_label, "}"),
  "\\small",
  "\\begin{tabular}{@{}lrrrrr@{}}",
  "\\toprule",
  "\\multicolumn{6}{@{}l}{\\textit{Panel A: Metadata years}} \\\\",
  paste0(
    "Dataset & Signals used & With publication dates & ",
    "\\shortstack{Sample start\\\\mean (median)} & ",
    "\\shortstack{Sample end\\\\mean (median)} & ",
    "\\shortstack{Publication\\\\mean (median)} \\\\"
  ),
  "\\midrule",
  vapply(c("JKP", "CZ"), function(dataset_name) {
    totals <- unique(date_comparison[dataset == dataset_name, .(
      total_factors, published_factors
    )])
    paste0(
      dataset_name, " & ", totals$total_factors, " & ",
      totals$published_factors, " & ",
      metadata_cell("SampleStartYear", dataset_name), " & ",
      metadata_cell("SampleEndYear", dataset_name), " & ",
      metadata_cell("pubYear", dataset_name),
      " \\\\"
    )
  }, character(1)),
  "\\addlinespace[0.6em]",
  paste0(
    "\\multicolumn{6}{@{}l}{\\textit{Panel B: Between-period length ",
    "(publication year minus sample-end year)}} \\\\"
  ),
  paste0(
    "Dataset & Signals used & Mean & SD & P10 / median / P90 & ",
    "Min / max \\\\"
  ),
  "\\midrule",
  vapply(seq_len(nrow(between)), function(i) {
    paste0(
      between$dataset[i], " & ", between$observations[i], " & ",
      formatC(between$mean[i], digits = 1L, format = "f"), " & ",
      formatC(between$sd[i], digits = 1L, format = "f"), " & ",
      formatC(between$p10[i], digits = 0L, format = "f"), " / ",
      formatC(between$median[i], digits = 0L, format = "f"), " / ",
      formatC(between$p90[i], digits = 0L, format = "f"), " & ",
      formatC(between$min[i], digits = 0L, format = "f"), " / ",
      formatC(between$max[i], digits = 0L, format = "f"), " \\\\"
    )
  }, character(1)),
  "\\bottomrule",
  "\\end{tabular}",
  "\\end{table}"
)
writeLines(
  date_lines,
  file.path(exhibit_dir, date_config$filename),
  useBytes = TRUE
)

message("Saved LaTeX exhibits to ", exhibit_dir)

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

exhibits <- file.path(
  exhibit_dir,
  c(
    "mp-cz-normalized.tex", "jkp-cz-normalized.tex",
    "cz-alternative-specs.tex", "jkp-rep-using-cz.tex",
    "s6-timefe-summary.tex", "jkp-cz-date-comparison.tex"
  )
)
check(all(file.exists(exhibits)), "One or more configured exhibits are missing.")
check(all(file.info(exhibits)$size > 0L), "One or more exhibits are empty.")
jkp_exhibit <- readLines(
  file.path(exhibit_dir, "jkp-cz-normalized.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-D]:", jkp_exhibit)) == 4L &&
    !any(grepl("Panel [EF]:", jkp_exhibit)),
  "The JKP exhibit must contain exactly Panels A--D."
)
jkp_rep_exhibit <- readLines(
  file.path(exhibit_dir, "jkp-rep-using-cz.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-E]:", jkp_rep_exhibit)) == 5L &&
    !any(grepl("Panel [F-G]:", jkp_rep_exhibit)) &&
    !any(startsWith(jkp_rep_exhibit, "JKP,")),
  "The JKP replication exhibit must contain exactly five CZ-only panels."
)
s6_exhibit <- readLines(
  file.path(exhibit_dir, "s6-timefe-summary.tex"),
  warn = FALSE
)
check(
  sum(grepl("Panel [A-F]:", s6_exhibit)) == 6L &&
    !any(grepl("Panel [G-Z]:", s6_exhibit)) &&
    sum(startsWith(s6_exhibit, "Predictor FE &")) == 6L &&
    sum(startsWith(s6_exhibit, "Predictor + time FE &")) == 6L,
  "The S6 summary must contain exactly six two-row panels."
)

message("Result structure and generated exhibits check out.")
