# Validate the retained factor-adjusted audit files and journal values.
#
# How to run: set the working directory to flex-mining/, run
#   Rscript S4a_ByJournal.R
#   Rscript tests/test_s4b_phase2_expected.R
# Inputs:  the six audit files under ../Results/FactorAdjusted/TstatFilter
# Outputs: no files; exits nonzero on a journal-value or output-contract change

live_dir <- "../Results/FactorAdjusted/TstatFilter"
expected_csv <- list(
  Table_FactorAdjusted_TimeVarying_DisciplineJournal_ff4_t2 = c(
    '"Category","Group","Raw_Return","Raw_Outperformance","CAPM_Return","CAPM_Outperformance","FF4_Return","FF4_Outperformance"',
    '"Discipline","Finance","59 (8)","11 (7)","69 (7)","15 (7)","78 (8)","15 (9)"',
    '"Discipline","Accounting","43 (9)","-5 (10)","48 (9)","-4 (10)","46 (8)","-17 (10)"',
    '"Journal Rank","JF, JFE, RFS","60 (8)","10 (8)","72 (8)","17 (8)","79 (9)","14 (9)"',
    '"Journal Rank","AR, JAR, JAE","43 (9)","-5 (10)","45 (9)","-7 (10)","44 (8)","-20 (10)"',
    '"Journal Rank","Other","53 (9)","8 (9)","56 (9)","6 (9)","65 (11)","9 (13)"'
  )
)
audit_bases <- c(
  "Table_FactorAdjusted_TimeVarying_ff4_t2",
  "Table_FactorAdjusted_TimeVarying_DisciplineJournal_ff4_t2",
  "Table_FactorAdjusted_TimeVarying_AnyModelVsNoModel_ff4_t2"
)

for (base in audit_bases) {
  csv_file <- file.path(live_dir, paste0(base, ".csv"))
  tex_file <- file.path(live_dir, paste0(base, ".tex"))
  if (base %in% names(expected_csv) &&
      !identical(readLines(csv_file, warn = FALSE), expected_csv[[base]])) {
    stop("Phase-two displayed CSV differs: ", base)
  }
  tex <- readLines(tex_file, warn = FALSE)
  if (length(grep("^\\\\begin\\{tabular\\}", tex)) != 1L ||
      length(grep("^\\\\end\\{tabular\\}", tex)) != 1L) {
    stop("TeX is not one complete tabular fragment: ", base)
  }
}

expected_files <- sort(c(
  paste0(audit_bases, ".csv"),
  paste0(audit_bases, ".tex")
))
actual_files <- sort(list.files(live_dir, all.files = FALSE, no.. = TRUE))
if (!identical(actual_files, expected_files)) {
  stop(
    "Factor-adjusted output contract differs. Expected: ",
    paste(expected_files, collapse = ", "), "; found: ",
    paste(actual_files, collapse = ", ")
  )
}

message("The six audit outputs match the corrected phase-two expectations.")
