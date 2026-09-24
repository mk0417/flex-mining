# Appendix driver: SA11 correlation- and PCA-spanning robustness.
#
# How to run: set the working directory to flex-mining/, then
#   Rscript SA_AppendicesPCA.R
# Inputs:  cleaned published-signal data and the chapter-2 mined-strategy file
# Outputs: appendix PCA caches under ../Data/Processed and the SA11 exhibits
#          under ../Results
#
# Split out of SA_Appendices.R because it dominates appendix runtime (~66 min)
# and memory (~23-27 GB at num_cores = 4); see docs/runtimes_and_ram.md. The
# rest of the appendix exhibits take a couple of minutes and stay in
# SA_Appendices.R, so iterating on them no longer pays the PCA cost.

settings_env <- new.env(parent = globalenv())
sys.source("config.R", envir = settings_env)
version_prefix <- file.path("../Data/Processed", settings_env$globalSettings$universe$dataVersion)
required_files <- c(
  "../Data/Processed/czsum_allpredictors.RDS",
  "../Data/Processed/czret_keeponly.RDS",
  paste0(version_prefix, " LongShort.RData")
)
rm(settings_env)
missing_files <- required_files[!file.exists(required_files)]
if (length(missing_files) > 0) {
  stop(
    "Missing appendix PCA input(s): ", paste(missing_files, collapse = ", "),
    ". Run the required upstream chapter first."
  )
}
if ("--preflight-only" %in% commandArgs(trailingOnly = TRUE)) {
  message("Appendix PCA preflight passed.")
  quit(save = "no", status = 0)
}

run_script <- function(path) {
  message("\n--- Appendices (PCA): ", path, " ---")
  status <- system2(file.path(R.home("bin"), "Rscript"), path)
  if (!identical(status, 0L)) {
    stop("Appendix script failed (exit ", status, "): ", path)
  }
}

run_script("Appendices/SA11_DMCorrelationsPCAPrep.R")
run_script("Appendices/SA11_DMCorrelationsPCATables.R")
run_script("Appendices/SA11_DMSpanPCAPrep.R")
run_script("Appendices/SA11_DMSpanPCAPlots.R")
