# Download the Chen-Zimmermann signal-level predictor panel.
#
# How to run: from flex-mining/,
#   Rscript Appendices/SA15_TimeFERobustness/run.R acquire
# Inputs:  the pinned Open Source Asset Pricing release (firm characteristics,
#            signal documentation, original portfolio returns), the pinned
#            meta-replication mapping, and CRSP.MSF via WRDS for the three
#            CRSP-derived signals the bulk file omits
#          WRDS credentials in WRDS_USER/WRDS_PASS or
#            WRDS_USERNAME/WRDS_PASSWORD (otherwise the package prompts)
# Outputs: ../Data/Raw/TimeFERobustness/opensourceap/ (pinned downloads)
#          ../Data/Processed/TimeFERobustness/opensourceap/
#            signed_predictors_all_wide-<release>.parquet/
#
# This stage is separate from the build because it downloads several gigabytes
# and can require a WRDS connection. The Parquet output is a directory-backed
# Arrow dataset: the bulk archive is about 2.3 GB compressed and 8+ GB as CSV,
# so it is streamed into Parquet rather than read into an R data.frame.

source("Appendices/SA15_TimeFERobustness/setup.R")
suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(OpenSourceAP.DownloadR)
})

release <- timefeSettings$pins$release
raw_dir <- timefeSettings$paths$openap_raw
archive_zip <- file.path(
  raw_dir, paste0("signed_predictors_dl_wide-", release, ".zip")
)
archive_csv <- file.path(
  raw_dir, paste0("signed_predictors_dl_wide-", release, ".csv")
)
doc_csv <- timefeSettings$files$signal_doc
op_csv <- timefeSettings$files$op_portfolios
meta_csv <- timefeSettings$files$meta_replications
panel_parquet <- timefeSettings$files$signal_panel

# OpenSourceAP.DownloadR expects the shorter WRDS variable names. Support the
# credential names used elsewhere in the devcontainer without printing them.
if (!nzchar(Sys.getenv("WRDS_USER")) &&
    nzchar(Sys.getenv("WRDS_USERNAME"))) {
  Sys.setenv(WRDS_USER = Sys.getenv("WRDS_USERNAME"))
}
if (!nzchar(Sys.getenv("WRDS_PASS")) &&
    nzchar(Sys.getenv("WRDS_PASSWORD"))) {
  Sys.setenv(WRDS_PASS = Sys.getenv("WRDS_PASSWORD"))
}

# Google Drive downloads can fail under libcurl's HTTP/2 implementation in
# this container. In httr/libcurl, http_version = 2 requests HTTP/1.1.
httr::set_config(httr::config(http_version = 2))

message("Initializing Open Source Asset Pricing release ", release, " ...")
openap <- OpenAP$new(release_year = release)

if (!file.exists(doc_csv) || file.info(doc_csv)$size == 0) {
  message("Downloading signal documentation ...")
  signal_doc <- as.data.table(openap$dl_signal_doc())
  fwrite(signal_doc, doc_csv)
}

if (!file.exists(op_csv) || file.info(op_csv)$size == 0) {
  message("Downloading original CZ portfolio returns ...")
  fwrite(as.data.table(openap$dl_port("op")), op_csv)
}

if (!file.exists(meta_csv) || file.info(meta_csv)$size == 0) {
  mapping_url <- paste0(
    "https://raw.githubusercontent.com/OpenSourceAP/CrossSection/",
    timefeSettings$pins$cz_mapping_commit,
    "/Docs/Comparison_to_MetaReplications.csv"
  )
  message("Downloading pinned meta-replication mapping ...")
  download.file(mapping_url, meta_csv, mode = "wb", quiet = TRUE)
  check(file.exists(meta_csv) && file.info(meta_csv)$size > 0,
        "Could not download the pinned meta-replication mapping.")
}
mapping_sha256 <- digest::digest(meta_csv, algo = "sha256", file = TRUE)
check(
  identical(mapping_sha256, timefeSettings$pins$cz_mapping_sha256),
  "Meta-replication mapping hash mismatch: expected %s, found %s.",
  timefeSettings$pins$cz_mapping_sha256, mapping_sha256
)

# Use the package to resolve the selected release's official bulk-file URL,
# but avoid dl_all_signals(): that method calls base read.csv() on the 8+ GB
# extracted file and exceeds the devcontainer's memory.
if (!file.exists(archive_zip) || file.info(archive_zip)$size == 0) {
  message("Downloading the bulk CZ firm-characteristic archive ...")
  firm_characteristic_url <- openap$get_url("firm_char")
  withr::with_options(
    new = list(timeout = 3600),
    code = download.file(
      firm_characteristic_url, archive_zip, mode = "wb", quiet = FALSE
    )
  )
}

if (!file.exists(archive_csv) || file.info(archive_csv)$size == 0) {
  message("Extracting the bulk CZ firm-characteristic CSV ...")
  archive_listing <- unzip(archive_zip, list = TRUE)
  csv_member <- archive_listing$Name[
    grepl("signed_predictors_dl_wide\\.csv$", archive_listing$Name)
  ]
  check(length(csv_member) == 1L,
        "Expected exactly one signed_predictors_dl_wide.csv in the archive.")
  extracted <- unzip(
    archive_zip, files = csv_member, exdir = raw_dir, overwrite = TRUE
  )
  check(length(extracted) == 1L && file.exists(extracted),
        "Could not extract the bulk firm-characteristic CSV.")
  check(file.rename(extracted, archive_csv),
        "Could not move the extracted firm-characteristic CSV to %s.",
        archive_csv)
}

if (!dir.exists(panel_parquet)) {
  message("Downloading the three CRSP-derived signals from WRDS ...")
  # The bulk file is already signed. dl_signal_crsp3() likewise constructs
  # signed Price, Size, and STreversal, so no second sign transformation is
  # appropriate here.
  crsp_signals <- as.data.table(openap$dl_signal_crsp3())
  crsp_signals[, `:=`(
    permno = as.integer(permno),
    yyyymm = as.integer(yyyymm)
  )]

  message("Streaming the merged CZ signal panel to Parquet ...")
  csv_columns <- names(fread(archive_csv, nrows = 0L))
  csv_types <- setNames(
    lapply(csv_columns, function(column) {
      if (column %in% c("permno", "yyyymm")) int32() else float64()
    }),
    csv_columns
  )
  # Several sparse characteristics are empty in Arrow's inference sample and
  # would otherwise be assigned the unsupported `null` type.
  openap_dataset <- open_csv_dataset(
    archive_csv,
    schema = do.call(schema, csv_types),
    skip = 1L,
    col_names = csv_columns
  )
  merged_dataset <- openap_dataset |>
    left_join(arrow_table(crsp_signals), by = c("permno", "yyyymm"))
  write_dataset(
    merged_dataset,
    path = panel_parquet,
    format = "parquet",
    existing_data_behavior = "error"
  )
}

panel_dataset <- open_dataset(panel_parquet, format = "parquet")
panel_rows <- panel_dataset |>
  summarise(rows = n()) |>
  collect() |>
  pull(rows)
signal_columns <- setdiff(
  panel_dataset$schema$names, c("permno", "yyyymm")
)
message(
  "Saved ", format(panel_rows, big.mark = ","), " firm-month rows and ",
  length(signal_columns), " characteristics to ", panel_parquet
)
message("Saved signal documentation to ", doc_csv)
message("Saved original portfolio returns to ", op_csv)
message("Saved pinned meta-replication mapping to ", meta_csv)
