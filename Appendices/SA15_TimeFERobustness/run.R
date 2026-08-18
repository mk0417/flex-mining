# Driver for the time-fixed-effects robustness appendix.
#
# How to run: from anywhere,
#   Rscript Appendices/SA15_TimeFERobustness/run.R [command]
#   commands: build (default), exhibits, preflight, acquire. Run with --help
#             for one-line descriptions.
# Inputs:  pinned external inputs under ../Data/Raw/TimeFERobustness and caches
#          under ../Data/Processed/TimeFERobustness; `acquire` obtains the
#          missing ones.
# Outputs: analysis CSVs under ../Data/Processed/TimeFERobustness/output and
#          TeX exhibits under ../Results/TimeFERobustness.
#
# Each stage runs as its own Rscript subprocess, as in MAIN.R, so a stage
# cannot inherit state from the one before it.

# The repository root is the working directory for every stage; find it from
# this file's own path so the driver can be run from anywhere.
file_argument <- grep("^--file=", commandArgs(trailingOnly = FALSE),
                      value = TRUE)
if (length(file_argument) != 1L) {
  stop("Could not locate run.R from the Rscript command line.", call. = FALSE)
}
module_dir <- normalizePath(dirname(sub("^--file=", "", file_argument)))
setwd(normalizePath(file.path(module_dir, "..", "..")))

module <- "Appendices/SA15_TimeFERobustness"
estimate <- file.path(module, "estimate.R")
terciles <- file.path(module, "cz_terciles.R")
exhibits <- file.path(module, "exhibits.R")

commands <- list(
  build = c(estimate, terciles, exhibits),
  exhibits = exhibits,
  acquire = file.path(module, "acquire.R")
)
descriptions <- c(
  build     = "Estimate everything and render the exhibits (default)",
  exhibits  = "Re-render the exhibits and re-check them, reusing the results",
  preflight = "Report package and input readiness",
  acquire   = "Acquire the large pinned CZ signal-level input"
)

arguments <- commandArgs(trailingOnly = TRUE)
command <- if (length(arguments)) arguments[[1L]] else "build"

if (command %in% c("help", "--help")) {
  cat("Usage: Rscript Appendices/SA15_TimeFERobustness/run.R [command]\n\n")
  cat(sprintf("  %-12s %s\n", names(descriptions), descriptions), sep = "")
  quit(save = "no", status = 0L)
}

# Preflight reports readiness rather than running anything, so it is handled
# here instead of in a stage of its own. Sourcing setup.R creates the module's
# data folders if they are missing, as the repository's own 0_Environment.R
# does for ../Data.
if (command == "preflight") {
  source(file.path(module, "setup.R"))
  packages <- c(
    "arrow", "data.table", "digest", "dplyr", "fixest", "httr",
    "OpenSourceAP.DownloadR", "readxl", "RPostgres", "withr"
  )
  package_ok <- vapply(packages, requireNamespace, logical(1), quietly = TRUE)
  inputs <- unlist(timefeSettings$files[c(
    "op_portfolios", "signal_doc", "meta_replications", "signal_panel",
    "crsp_cache"
  )])
  input_ok <- file.exists(inputs) | dir.exists(inputs)

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
  if (all(input_ok)) {
    message("Time-FE robustness build inputs are present.")
  } else {
    message(
      "Preflight found missing data. Run `acquire` for the Open Source Asset ",
      "Pricing inputs; a missing CRSP cache requires an explicitly authorized ",
      "WRDS connection during `build`."
    )
  }
  quit(save = "no", status = 0L)
}

if (!command %in% names(commands)) {
  stop(sprintf("Unknown command '%s'. Run with --help for usage.", command),
       call. = FALSE)
}

run_script <- function(path) {
  message("\n--- TimeFERobustness: ", path, " ---")
  status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(path))
  if (!identical(status, 0L)) {
    stop("Stage failed (exit ", status, "): ", path, call. = FALSE)
  }
}

for (script in commands[[command]]) run_script(script)
message("\nTime-FE robustness `", command, "` complete.")
