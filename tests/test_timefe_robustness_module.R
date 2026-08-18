# Test the portable configuration and source integrity of the time-FE appendix.
#
# How to run: from flex-mining/, `Rscript tests/test_timefe_robustness_module.R`
# Inputs:  Appendices/SA15_TimeFERobustness source files.
# Outputs: console assertion summary; no files are written.

pdf(NULL)

module_dir <- "Appendices/SA15_TimeFERobustness"
r_dir <- file.path(module_dir, "R")
files <- c(file.path(module_dir, "run.R"), list.files(
  r_dir, pattern = "[.]R$", full.names = TRUE
))

stopifnot(length(files) == 14L, all(file.exists(files)))
invisible(lapply(files, parse))

source(file.path(r_dir, "config.R"))
stopifnot(
  identical(TIMEFE_RELEASE, "2025_10"),
  nchar(TIMEFE_JKP_COMMIT) == 40L,
  nchar(TIMEFE_CZ_MAPPING_COMMIT) == 40L,
  nchar(TIMEFE_CZ_MAPPING_SHA256) == 64L,
  identical(TIMEFE_OUTPUT_DIR,
            "../Data/Processed/TimeFERobustness/output"),
  identical(TIMEFE_EXHIBIT_DIR, "../Results/TimeFERobustness")
)

timefe_source("helpers.R")
stopifnot(
  identical(parse_first_year(c("Smith 1999", NA_character_)),
            c(1999L, NA_integer_)),
  identical(parse_period(c("1963--1982", "2001 to 2010"), "start"),
            c(1963L, 2001L)),
  identical(parse_period(c("1963--1982", "2001 to 2010"), "end"),
            c(1982L, 2010L))
)

message("test_timefe_robustness_module.R: all assertions passed")
