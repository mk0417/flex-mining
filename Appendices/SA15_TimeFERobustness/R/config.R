# Shared paths and pinned input versions for the time-fixed-effects robustness
# appendix.
#
# How to run: source from a script launched by
#   Rscript Appendices/SA15_TimeFERobustness/run.R <command>
# Inputs: optional TIMEFE_{RAW,PROCESSED,RESULTS}_DIR environment overrides.
# Outputs: defines configuration constants only; it does not create files.

TIMEFE_RELEASE <- "2025_10"
TIMEFE_JKP_COMMIT <- "98adb75ddc66d2cc47613dcab745b0ea6260e902"
TIMEFE_CZ_MAPPING_COMMIT <- "e4a1d728caea04e68868614ad938d0293c5d0b11"
TIMEFE_CZ_MAPPING_SHA256 <- paste0(
  "2d95e3ee4d49cdea9e1e55cd653e8131bd53766566c49eb6f",
  "a357e4b9dcc61bf"
)

TIMEFE_MODULE_DIR <- "Appendices/SA15_TimeFERobustness"
TIMEFE_R_DIR <- file.path(TIMEFE_MODULE_DIR, "R")

TIMEFE_RAW_DIR <- Sys.getenv(
  "TIMEFE_RAW_DIR", unset = "../Data/Raw/TimeFERobustness"
)
TIMEFE_PROCESSED_DIR <- Sys.getenv(
  "TIMEFE_PROCESSED_DIR", unset = "../Data/Processed/TimeFERobustness"
)
TIMEFE_RESULTS_DIR <- Sys.getenv(
  "TIMEFE_RESULTS_DIR", unset = "../Results/TimeFERobustness"
)

TIMEFE_JKP_RAW_DIR <- file.path(TIMEFE_RAW_DIR, "jkp")
TIMEFE_OPENAP_RAW_DIR <- file.path(TIMEFE_RAW_DIR, "opensourceap")
TIMEFE_OPENAP_PROCESSED_DIR <- file.path(
  TIMEFE_PROCESSED_DIR, "opensourceap"
)
TIMEFE_OUTPUT_DIR <- file.path(TIMEFE_PROCESSED_DIR, "output")
TIMEFE_EXHIBIT_DIR <- TIMEFE_RESULTS_DIR

TIMEFE_OP_CSV <- file.path(
  TIMEFE_OPENAP_RAW_DIR, paste0("op-", TIMEFE_RELEASE, ".csv")
)
TIMEFE_SIGNAL_DOC_CSV <- file.path(
  TIMEFE_OPENAP_RAW_DIR, paste0("SignalDoc-", TIMEFE_RELEASE, ".csv")
)
TIMEFE_META_REPLICATIONS_CSV <- file.path(
  TIMEFE_OPENAP_RAW_DIR, "Comparison_to_MetaReplications.csv"
)

timefe_source <- function(filename) {
  source(file.path(TIMEFE_R_DIR, filename), local = FALSE)
}
