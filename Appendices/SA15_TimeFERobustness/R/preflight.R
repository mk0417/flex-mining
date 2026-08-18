# Report whether the time-fixed-effects robustness appendix is ready to run.
#
# How to run:
#   Rscript Appendices/SA15_TimeFERobustness/run.R preflight
# Inputs: configured raw and processed input paths.
# Outputs: console report only; this script does not create or modify files.

pdf(NULL)
source("Appendices/SA15_TimeFERobustness/R/config.R")

packages <- c(
  "arrow", "data.table", "digest", "dplyr", "fixest", "httr",
  "OpenSourceAP.DownloadR", "readxl", "RPostgres", "withr"
)
package_ok <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)

panel_parquet <- file.path(
  TIMEFE_OPENAP_PROCESSED_DIR,
  paste0("signed_predictors_all_wide-", TIMEFE_RELEASE, ".parquet")
)
crsp_parquet <- file.path(
  TIMEFE_OPENAP_PROCESSED_DIR, "crsp-monthly-returns-market-equity.parquet"
)
inputs <- c(
  op_portfolios = TIMEFE_OP_CSV,
  signal_documentation = TIMEFE_SIGNAL_DOC_CSV,
  meta_replication_mapping = TIMEFE_META_REPLICATIONS_CSV,
  signal_panel = panel_parquet,
  crsp_cache = crsp_parquet
)
input_ok <- vapply(inputs, function(path) file.exists(path) || dir.exists(path),
                   logical(1))

cat("Packages:\n")
print(data.frame(package = packages, available = unname(package_ok)),
      row.names = FALSE)
cat("\nBuild inputs:\n")
print(data.frame(input = names(inputs), path = unname(inputs),
                 available = unname(input_ok)), row.names = FALSE)

if (!all(package_ok)) {
  stop("Missing required R package(s): ",
       paste(packages[!package_ok], collapse = ", "), call. = FALSE)
}
if (!all(input_ok)) {
  message(
    "Preflight found missing data. Run `acquire` for the Open Source Asset ",
    "Pricing inputs; a missing CRSP cache requires an explicitly authorized ",
    "WRDS connection during `build`."
  )
} else {
  message("Time-FE robustness build inputs are present.")
}
