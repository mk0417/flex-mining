# Estimate and render annual-accounting-only versions of Tables 3 and 4.
#
# How to run: from flex-mining/, run
#   Rscript Appendices/SA14_MPStyleRegsAccountingOnly.R
# Inputs:  ../Data/Processed/{raw_dm_benchmarks,czsum_allpredictors}.RDS
#          ../Data/Raw/SignalDoc.csv
# Outputs: ../Results/Table_MPStyleRegs{NoTimeFE,TimeFE}_AccountingOnly.tex

rm(list = ls())
source("0_Environment.R")

# Reuse the main-table renderer without rendering Tables 3 and 4 themselves.
Sys.setenv(MP_TABLE_FUNCTIONS_ONLY = "true")
source("S3b_MPStyleDecayTables.R")
Sys.unsetenv("MP_TABLE_FUNCTIONS_ONLY")

benchmark <- readRDS("../Data/Processed/raw_dm_benchmarks.RDS")
incl_signals <- restrictInclSignals(
  restrictType = globalSettings$inclusion$restrictType,
  topT = globalSettings$inclusion$topT
)

# Match the annual-Compustat definition used by Figure 2(b): start
# from SignalDoc's Accounting category, then remove quarterly, analyst-based,
# discrete, and CRSP-only signals.
annual_accounting_signals <- readRDS(
  "../Data/Processed/czsum_allpredictors.RDS"
) %>%
  left_join(
    data.table::fread("../Data/Raw/SignalDoc.csv") %>%
      transmute(
        Acronym, Cat.Data, Cat.Form,
        Def = tolower(`Detailed Definition`)
      ),
    by = c("signalname" = "Acronym")
  ) %>%
  filter(signalname %in% incl_signals, Cat.Data == "Accounting") %>%
  mutate(
    drop = grepl("quarter", Def) |
      grepl(
        "analyst|meanest|earningssurprise",
        paste(tolower(signalname), Def)
      ) |
      Cat.Form == "discrete" |
      signalname %in% c("ShareIss1Y", "ShareIss5Y")
  ) %>%
  filter(!drop) %>%
  pull(signalname) %>%
  unique()

build_reg_panel <- function(panel, pub_col, dm_col) {
  panel %>%
    filter(pubname %in% annual_accounting_signals) %>%
    transmute(
      pubname, eventDate, calendarDate, sampstart, sampend, pubdate,
      ret = .data[[pub_col]],
      matchRet = .data[[dm_col]],
      postSample = ifelse(calendarDate >= sampend, 1, 0),
      postPub = ifelse(calendarDate >= pubdate, 1, 0)
    ) %>%
    mutate(diffRet = ret - matchRet) %>%
    filter(
      calendarDate >= sampstart,
      complete.cases(ret, matchRet, postSample, postPub)
    )
}

reg_data_a <- build_reg_panel(
  benchmark$accounting_t2_uncorr,
  "published_ret_scaled",
  "dm_ret_scaled"
)
reg_data_b <- build_reg_panel(
  benchmark$matched,
  "published_ret_unscaled",
  "matched_uncorr_ret_unscaled"
)

if (nrow(reg_data_a) == 0L || nrow(reg_data_b) == 0L) {
  stop("An annual-accounting MP-style regression panel is empty.")
}

fit_outcome <- function(lhs, data, time_fe = FALSE) {
  fixed_effects <- if (time_fe) "pubname + calendarDate" else "pubname"
  fixest::feols(
    stats::as.formula(
      paste0(lhs, " ~ postSample + postPub | ", fixed_effects)
    ),
    data = data,
    cluster = ~pubname + calendarDate
  )
}
fits_for <- function(data) list(
  fit_outcome("ret", data), fit_outcome("ret", data, TRUE),
  fit_outcome("matchRet", data), fit_outcome("matchRet", data, TRUE),
  fit_outcome("diffRet", data), fit_outcome("diffRet", data, TRUE)
)

panel_a <- fits_for(reg_data_a)
panel_b <- fits_for(reg_data_b)
n_signals_a <- dplyr::n_distinct(reg_data_a$pubname)
n_signals_b <- dplyr::n_distinct(reg_data_b$pubname)

message(
  "Annual-accounting MP panels: scaled = ", nrow(reg_data_a),
  " observations / ", n_signals_a, " signals; unscaled = ",
  nrow(reg_data_b), " observations / ", n_signals_b, " signals."
)

for (fits in list(panel_a, panel_b)) {
  stopifnot(
    length(unique(vapply(fits[c(1, 3, 5)], stats::nobs, numeric(1)))) == 1L,
    length(unique(vapply(fits[c(2, 4, 6)], stats::nobs, numeric(1)))) == 1L
  )
}

output_dir <- Sys.getenv("MP_ACCOUNTING_TABLE_OUTPUT_DIR", unset = "../Results")
dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
signal_counts <- c(rep(n_signals_a, 3), rep(n_signals_b, 3))

make_combined_table(
  c(panel_a[c(1, 3, 5)], panel_b[c(1, 3, 5)]),
  signal_counts,
  timeFE = FALSE,
  file = file.path(
    output_dir,
    "Table_MPStyleRegsNoTimeFE_AccountingOnly.tex"
  )
)
make_combined_table(
  c(panel_a[c(2, 4, 6)], panel_b[c(2, 4, 6)]),
  signal_counts,
  timeFE = TRUE,
  file = file.path(
    output_dir,
    "Table_MPStyleRegsTimeFE_AccountingOnly.tex"
  )
)
