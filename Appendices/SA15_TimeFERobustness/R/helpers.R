# Shared helpers for the Jeff-response analysis scripts.
#
# Usage:
#   timefe_source("helpers.R") after sourcing this module's config.R
#
# Inputs: paths and vectors supplied by callers.
# Outputs: none; this file only defines functions.

fail <- function(...) stop(sprintf(...), call. = FALSE)

check <- function(ok, ...) {
  if (!isTRUE(ok)) fail(...)
}

read_zip_csv <- function(path) {
  listing <- unzip(path, list = TRUE)
  csv <- listing$Name[grepl("\\.csv$", listing$Name, ignore.case = TRUE)]
  check(length(csv) == 1L, "%s must contain exactly one CSV.", path)

  extract_dir <- tempfile("jkp-unzip-")
  dir.create(extract_dir)
  on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)

  extracted <- unzip(path, files = csv, exdir = extract_dir)
  check(
    length(extracted) == 1L && file.exists(extracted),
    "Could not extract the CSV from %s.", path
  )
  data.table::fread(extracted)
}

parse_first_year <- function(x) {
  answer <- rep(NA_integer_, length(x))
  good <- !is.na(x) & grepl("[12][0-9]{3}", x)
  answer[good] <- as.integer(
    sub(".*?([12][0-9]{3}).*", "\\1", x[good], perl = TRUE)
  )
  answer
}

parse_period <- function(x, which = c("start", "end")) {
  which <- match.arg(which)
  hits <- gregexpr("[12][0-9]{3}", x)
  years <- regmatches(x, hits)
  position <- if (which == "start") 1L else 2L

  vapply(years, function(value) {
    if (length(value) < position || identical(value, character(0))) {
      NA_integer_
    } else {
      as.integer(value[position])
    }
  }, integer(1))
}
