# Single entry point for the time-fixed-effects robustness appendix.
#
# How to run from any directory:
#   Rscript Appendices/SA15_TimeFERobustness/run.R
#     [build|tables|diagnostics|check|preflight|acquire|all]
# Inputs: pinned external inputs under ../Data/Raw/TimeFERobustness and caches
#   under ../Data/Processed/TimeFERobustness; `acquire` obtains missing inputs.
# Outputs: analysis CSVs under ../Data/Processed/TimeFERobustness/output and
#   TeX exhibits under ../Results/TimeFERobustness.

pdf(NULL)

arguments <- commandArgs(trailingOnly = FALSE)
file_argument <- arguments[grepl("^--file=", arguments)]
if (length(file_argument) != 1L) {
  stop("Could not locate the timeFE runner from the Rscript command line.",
       call. = FALSE)
}

runner_path <- normalizePath(sub("^--file=", "", file_argument))
repo_root <- normalizePath(file.path(dirname(runner_path), "..", ".."))
setwd(repo_root)

source("Appendices/SA15_TimeFERobustness/R/config.R")

commands <- commandArgs(trailingOnly = TRUE)
command <- if (length(commands)) commands[[1L]] else "build"

core_scripts <- file.path(TIMEFE_R_DIR, c(
  "mp.R",
  "jkp.R",
  "cz-alternative-specs.R",
  "jkp-alternative-specs.R",
  "compare-full-dataset-dates.R",
  "cz.R"
))
table_script <- file.path(TIMEFE_R_DIR, "tables.R")
check_script <- file.path(TIMEFE_R_DIR, "checks.R")
diagnostic_script <- file.path(TIMEFE_R_DIR, "diagnostics.R")

tasks <- switch(
  command,
  build = c(core_scripts, table_script, check_script),
  tables = c(table_script, check_script),
  diagnostics = diagnostic_script,
  check = check_script,
  preflight = file.path(TIMEFE_R_DIR, "preflight.R"),
  acquire = file.path(TIMEFE_R_DIR, "acquire.R"),
  all = c(core_scripts, diagnostic_script, table_script, check_script),
  help = character(),
  `--help` = character(),
  stop(sprintf("Unknown command '%s'. Run with --help for usage.", command),
       call. = FALSE)
)

if (command %in% c("help", "--help")) {
  cat(
    "Usage: Rscript Appendices/SA15_TimeFERobustness/run.R [command]\n\n",
    "  build        Run core analyses, tables, and checks (default)\n",
    "  tables       Rewrite only TeX exhibits and run checks\n",
    "  diagnostics  Run the optional outlier investigation\n",
    "  check        Check pinned results and exhibit files\n",
    "  preflight    Report package and input readiness without writing\n",
    "  acquire      Acquire the large pinned CZ signal-level input\n",
    "  all          Run build plus diagnostics\n",
    sep = ""
  )
  quit(save = "no", status = 0L)
}

rscript <- file.path(R.home("bin"), "Rscript")
for (script in tasks) {
  if (!file.exists(script)) {
    stop(sprintf("Pipeline script does not exist: %s", script), call. = FALSE)
  }
  message("\n==> ", script)
  status <- system2(rscript, shQuote(script))
  if (status != 0L) {
    stop(sprintf("Pipeline stopped after %s (status %d).", script, status),
         call. = FALSE)
  }
}

message("\nTime-FE robustness `", command, "` complete.")
