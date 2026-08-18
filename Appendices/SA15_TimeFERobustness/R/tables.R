# Generate the LaTeX regression exhibits included by the private note.
#
# Run from the repository root through the single entry point:
#   Rscript Appendices/SA15_TimeFERobustness/run.R tables
#
# Inputs: regression and date-comparison CSVs in the configured output folder.
# Outputs: TeX fragments under ../Results/TimeFERobustness by default.

pdf(NULL)
source("Appendices/SA15_TimeFERobustness/R/config.R")

suppressPackageStartupMessages(library(data.table))

MP_RESULTS <- file.path(TIMEFE_OUTPUT_DIR, "mp-regressions.csv")
JKP_RESULTS <- file.path(TIMEFE_OUTPUT_DIR, "jkp-regressions.csv")
ALT_RESULTS <- file.path(TIMEFE_OUTPUT_DIR, "cz-alternative-specs.csv")
JKP_ALT_RESULTS <- file.path(TIMEFE_OUTPUT_DIR, "jkp-alternative-specs.csv")
DATE_RESULTS <- file.path(
  TIMEFE_OUTPUT_DIR, "jkp-cz-full-date-comparison.csv"
)
SIGNAL_RESULTS <- file.path(
  TIMEFE_OUTPUT_DIR, "cz-signal-based-regressions.csv"
)
EXHIBIT_DIR <- TIMEFE_EXHIBIT_DIR

# Table-only choices live here. Analysis files use stable internal names;
# labels, units, captions, panel order, and rounding are presentation details.
TABLE_CONFIG <- list(
  labels = list(
    fixed_effects = c(
      predictor = "Predictor FE",
      `predictor + month` = "Predictor + time FE",
      `pred FE` = "Predictor FE",
      `+time FE` = "Predictor + time FE"
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

if (
  !file.exists(MP_RESULTS) || !file.exists(JKP_RESULTS) ||
  !file.exists(ALT_RESULTS) || !file.exists(JKP_ALT_RESULTS) ||
  !file.exists(DATE_RESULTS) || !file.exists(SIGNAL_RESULTS)
) {
  stop(
    paste0(
      "Run `Rscript Appendices/SA15_TimeFERobustness/run.R build` before generating ",
      "the LaTeX exhibits."
    ),
    call. = FALSE
  )
}

dir.create(EXHIBIT_DIR, recursive = TRUE, showWarnings = FALSE)
mp <- fread(MP_RESULTS)
jkp <- fread(JKP_RESULTS)
alt <- fread(ALT_RESULTS)
jkp_alt <- fread(JKP_ALT_RESULTS)
date_comparison <- fread(DATE_RESULTS)
signal_results <- fread(SIGNAL_RESULTS)

fixed_effect_label <- function(id) {
  label <- unname(TABLE_CONFIG$labels$fixed_effects[id])
  if (length(label) != 1L || is.na(label)) {
    stop(sprintf("No table label is configured for fixed effects '%s'.", id),
         call. = FALSE)
  }
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
  if (nrow(row) != 1L) {
    stop(
      sprintf(
        "Expected one row for specification '%s' and FE '%s'; found %d.",
        specification, fixed_effects, nrow(row)
      ),
      call. = FALSE
    )
  }
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
  writeLines(lines, file.path(EXHIBIT_DIR, filename), useBytes = TRUE)
}

normalize_raw_rows <- function(rows) {
  check_cols <- c(
    "post_sample", "post_sample_se", "additional_post_publication",
    "additional_post_publication_se", "total_post_publication_change",
    "total_post_publication_change_se"
  )
  if (
    !"normalization_mean_pct" %in% names(rows) ||
    any(is.na(rows$normalization_mean_pct))
  ) {
    stop("Raw rows lack an in-sample normalization mean.", call. = FALSE)
  }
  rows[, (check_cols) := lapply(
    .SD, function(value) 100 * value / normalization_mean_pct
  ), .SDcols = check_cols]
  rows
}

grand_mean_bps <- function(data, spec_name) {
  if (!"normalization_mean_bps" %in% names(data)) {
    stop("Results lack numeric normalization_mean_bps metadata.",
         call. = FALSE)
  }
  value <- unique(data[
    specification == spec_name & !is.na(normalization_mean_bps),
    normalization_mean_bps
  ])
  if (length(value) != 1L || !is.finite(value)) {
    stop(
      sprintf("Expected one numeric normalization mean for '%s'.", spec_name),
      call. = FALSE
    )
  }
  value
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
mp_normalized[, mean_in_sample_bps := 100 * normalization_mean_pct]
mp_normalized[, min_in_sample_mean_bps := NA_real_]
mp_normalized[, panel_label := TABLE_CONFIG$labels$mp_cz_panels[["mp"]]]

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
cz_all_signal_normalized[, mean_in_sample_bps := grand_mean_bps(
  jkp, "cz_all_grand_mean_scaled_reference"
)]
cz_all_signal_normalized[
  , panel_label := TABLE_CONFIG$labels$mp_cz_panels[["cz_signal"]]
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
cz_all_grand_normalized[, mean_in_sample_bps := grand_mean_bps(
  jkp, "cz_all_grand_mean_scaled_reference"
)]
cz_all_grand_normalized[
  , panel_label := TABLE_CONFIG$labels$mp_cz_panels[["cz_grand"]]
]

cz_mp_normalized <- normalize_raw_rows(rbindlist(list(
  select_row(
    mp, "cz_mp_matched_2013_unscaled_pub_dec", "pred FE",
    fixed_effect_label("pred FE")
  ),
  select_row(
    mp, "cz_mp_matched_2013_unscaled_pub_dec", "+time FE",
    fixed_effect_label("+time FE")
  )
)))
cz_mp_normalized[, mean_in_sample_bps := 100 * normalization_mean_pct]
cz_mp_normalized[
  , panel_label := TABLE_CONFIG$labels$mp_cz_panels[["cz_mp"]]
]

mp_cz_normalized <- rbindlist(
  list(
    mp_normalized, cz_all_signal_normalized,
    cz_all_grand_normalized, cz_mp_normalized
  ),
  use.names = TRUE, fill = TRUE
)
mp_cz_config <- TABLE_CONFIG$tables$mp_cz
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
jkp_normalized[, mean_in_sample_bps := grand_mean_bps(
  jkp, "baseline_quality_t2_grand_mean_scaled"
)]
jkp_normalized[
  , panel_label := TABLE_CONFIG$labels$jkp_panels[["jkp_signal"]]
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
jkp_grand_mean_normalized[, mean_in_sample_bps := grand_mean_bps(
  jkp, "baseline_quality_t2_grand_mean_scaled"
)]
jkp_grand_mean_normalized[
  , panel_label := TABLE_CONFIG$labels$jkp_panels[["jkp_grand"]]
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
  TABLE_CONFIG$labels$jkp_panels[["jkp_cap"]],
  TABLE_CONFIG$labels$jkp_panels[["jkp_value"]]
)]

jkp_table_rows <- rbindlist(
  list(
    jkp_normalized, jkp_grand_mean_normalized,
    jkp_weighting_normalized
  ),
  use.names = TRUE, fill = TRUE
)
jkp_config <- TABLE_CONFIG$tables$jkp
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
    mean_in_sample_bps = grand_mean_bps(
      jkp, "baseline_quality_t2_grand_mean_scaled"
    ),
    factors
  )
]
jkp_benchmark[, `:=`(
  data_name = "jkp_baseline",
  label = TABLE_CONFIG$labels$alternative_panels[["jkp_baseline"]]
)]

alt_long <- rbindlist(
  list(jkp_benchmark, alt),
  use.names = TRUE,
  fill = TRUE
)
alt_long[, label := unname(TABLE_CONFIG$labels$alternative_panels[data_name])]
if (anyNA(alt_long$label)) {
  stop("At least one alternative portfolio lacks a configured table label.",
       call. = FALSE)
}
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
      digits = TABLE_CONFIG$tables$alternatives$digits,
      format = "f"
    ), " (",
    formatC(
      standard_error,
      digits = TABLE_CONFIG$tables$alternatives$digits,
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
    TABLE_CONFIG$labels$units[["bps_per_month"]], ")} & Signals \\\\"
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
  writeLines(lines, file.path(EXHIBIT_DIR, filename), useBytes = TRUE)
}

alternative_config <- TABLE_CONFIG$tables$alternatives
write_alt_longtable(
  alternative_config$filename,
  alternative_config$caption,
  alternative_config$tex_label,
  alt_long
)

jkp_rep_config <- TABLE_CONFIG$tables$jkp_rep_using_cz

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
    TABLE_CONFIG$labels$replication_rows[["cz"]]
  )
  rows[]
}

jkp_rep_rows <- rbindlist(list(
  cz_rep_rows("ew", "baseline_quality_t2")[
    , panel_label := TABLE_CONFIG$labels$jkp_rep_panels[["ew_signal"]]
  ],
  cz_rep_rows("ew", "baseline_quality_t2_grand_mean_scaled")[
    , panel_label := TABLE_CONFIG$labels$jkp_rep_panels[["ew_grand"]]
  ],
  cz_rep_rows("vw_cap", "baseline_quality_t2")[
    , panel_label := TABLE_CONFIG$labels$jkp_rep_panels[["vw_cap_signal"]]
  ],
  cz_rep_rows("vw", "baseline_quality_t2")[
    , panel_label := TABLE_CONFIG$labels$jkp_rep_panels[["vw_signal"]]
  ],
  cz_rep_rows("ew", "baseline_quality_t2_metadata_matched")[
    , panel_label :=
      TABLE_CONFIG$labels$jkp_rep_panels[["metadata_matched"]]
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
s6_config <- TABLE_CONFIG$tables$s6_summary

s6_bold_if_time_fe <- function(value, fixed_effects) {
  if (fixed_effects %in% c("predictor + month", "+time FE")) {
    paste0("\\textbf{", value, "}")
  } else {
    value
  }
}

s6_exact_panel <- function(data, panel_label) {
  wanted_panel_label <- panel_label
  rows <- copy(data)
  if (nrow(rows) != 2L) {
    stop(
      sprintf("S6 panel '%s' must contain exactly two rows.", panel_label),
      call. = FALSE
    )
  }
  rows[, fe_order := match(
    fixed_effects,
    c("predictor", "pred FE", "predictor + month", "+time FE")
  )]
  if (anyNA(rows$fe_order)) {
    stop(sprintf("S6 panel '%s' has an unknown FE label.", panel_label),
         call. = FALSE)
  }
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
if (nrow(s6_panel_counts) > 6L || any(s6_panel_counts$N != 2L)) {
  stop("S6 summary must have at most six panels and two rows per panel.",
       call. = FALSE)
}

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
  writeLines(lines, file.path(EXHIBIT_DIR, filename), useBytes = TRUE)
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
date_config <- TABLE_CONFIG$tables$dates

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
      metadata_cell("sample_start", dataset_name), " & ",
      metadata_cell("sample_end", dataset_name), " & ",
      metadata_cell("publication", dataset_name),
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
  file.path(EXHIBIT_DIR, date_config$filename),
  useBytes = TRUE
)

message("Saved LaTeX exhibits to ", EXHIBIT_DIR)
