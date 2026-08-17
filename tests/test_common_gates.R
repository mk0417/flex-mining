# Test that the common accounting-ratio gates are imposed as documented.
#
# How to run: from flex-mining/, run
#   Rscript tests/test_common_gates.R
# Inputs: config.R and helpers/matching.R (no data files).
# Outputs: none; exits nonzero on failure.

library(data.table)
source("config.R")
# helpers/matching.R expects the tidyverse verbs its other functions use. Only
# the gate helpers are exercised here, so evaluate the file with the pipe
# operator available and ignore the rest.
suppressMessages(library(dplyr))
source("helpers/matching.R")

gates <- globalSettings$benchmark$gates
stopifnot(
  identical(
    sort(names(gates)),
    sort(c("minNumStocks", "nmonth_min", "nlastyear_required"))
  ),
  gates$minNumStocks == 20,
  gates$nmonth_min == 60,
  gates$nlastyear_required == 12
)

# One row per gate, each failing exactly one requirement, plus a clean row.
base_row <- data.table(
  pubname = "pub", sweight = "ew", dmname = "clean", sampstart = 1970,
  sampend = 1990, rbar = 0.5, tstat = 3, cor = 0.4,
  min_nstock_long = 10, min_nstock_short = 10, nmonth = 120, nlastyear = 12
)
make_row <- function(name, ...) {
  row <- copy(base_row)
  row[, dmname := name]
  changes <- list(...)
  for (column in names(changes)) row[[column]] <- changes[[column]]
  row
}
input <- rbindlist(list(
  base_row,
  make_row("short_long_leg", min_nstock_long = 9),
  make_row("short_short_leg", min_nstock_short = 9),
  make_row("short_history", nmonth = 59),
  make_row("partial_final_year", nlastyear = 11),
  make_row("no_orientation", rbar = 0),
  make_row("missing_mean", rbar = NA_real_)
))

gated <- apply_common_gates(input, gates)
stopifnot(
  identical(gated$dmname, "clean"),
  # exactly at the thresholds is passing, not failing
  nrow(apply_common_gates(make_row("edge", min_nstock_long = 10,
                                   nmonth = 60, nlastyear = 12), gates)) == 1L
)

# Orientation comes from the in-sample mean and signs the correlation.
oriented <- apply_common_gates(
  rbindlist(list(base_row, make_row("negative", rbar = -0.5, cor = 0.4))),
  gates
)
stopifnot(
  identical(oriented[dmname == "clean"]$orientation, 1),
  identical(oriented[dmname == "negative"]$orientation, -1),
  isTRUE(all.equal(oriented[dmname == "clean"]$rho, 0.4)),
  isTRUE(all.equal(oriented[dmname == "negative"]$rho, -0.4)),
  identical(attr(gated, "common_gates"), gates)
)

# An input without a raw correlation is gated but gets no rho.
no_cor <- apply_common_gates(base_row[, .SD, .SDcols = !"cor"], gates)
stopifnot(nrow(no_cor) == 1L, !"rho" %in% names(no_cor))

# The uncorrelated screen keeps only pairs with a measured, low signed
# correlation, and refuses to run on an ungated input.
screen_input <- rbindlist(list(
  make_row("low", cor = 0.05),
  make_row("high", cor = 0.4),
  make_row("negative_high", rbar = -0.5, cor = 0.4),
  make_row("unmeasured", cor = NA_real_)
))
screened <- apply_uncorrelated_screen(apply_common_gates(screen_input, gates), 0.10)
stopifnot(
  setequal(screened$dmname, c("low", "negative_high")),
  inherits(try(apply_uncorrelated_screen(screen_input, 0.10), silent = TRUE),
           "try-error")
)

# Malformed gate settings and ungateable inputs fail loudly.
stopifnot(
  inherits(try(apply_common_gates(input, list(minNumStocks = 20)), silent = TRUE),
           "try-error"),
  inherits(try(apply_common_gates(input, modifyList(gates, list(nmonth_min = -1))),
               silent = TRUE), "try-error"),
  inherits(try(apply_common_gates(base_row[, .(pubname, rbar)], gates),
               silent = TRUE), "try-error")
)

# The input is not modified in place.
stopifnot(nrow(input) == 7L, !"orientation" %in% names(input))

message("test_common_gates.R: all assertions passed")
