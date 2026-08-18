# Download the Chen-Zimmermann (CZ) signal-level predictor panel.
#
# Run through: Rscript Appendices/SA15_TimeFERobustness/run.R acquire
#
# Inputs:
#   - Open Source Asset Pricing release 2025_10 firm characteristics
#   - CRSP.MSF via WRDS for Price, Size, and short-term reversal
#   - WRDS credentials in WRDS_USER/WRDS_PASS or
#     WRDS_USERNAME/WRDS_PASSWORD (otherwise the package prompts)
#
# Outputs:
#   ../Data/Raw/TimeFERobustness/opensourceap/ (pinned downloads)
#   ../Data/Processed/TimeFERobustness/opensourceap/
#     signed_predictors_all_wide-2025_10.parquet/
#
# The Parquet output is a directory-backed Arrow dataset. The bulk OpenAP
# archive is about 2.3 GB compressed and 8+ GB as CSV, so this script streams
# it into Parquet rather than loading the entire panel into an R data.frame.

suppressPackageStartupMessages({
  library(arrow)
  library(data.table)
  library(dplyr)
  library(OpenSourceAP.DownloadR)
})

pdf(NULL)
Sys.setenv(TZ = "America/New_York")
source("Appendices/SA15_TimeFERobustness/R/config.R")

RELEASE <- TIMEFE_RELEASE
RAW_DIR <- TIMEFE_OPENAP_RAW_DIR
PROCESSED_DIR <- TIMEFE_OPENAP_PROCESSED_DIR
ARCHIVE_ZIP <- file.path(
  RAW_DIR, paste0("signed_predictors_dl_wide-", RELEASE, ".zip")
)
OPENAP_CSV <- file.path(
  RAW_DIR, paste0("signed_predictors_dl_wide-", RELEASE, ".csv")
)
PANEL_PARQUET <- file.path(
  PROCESSED_DIR, paste0("signed_predictors_all_wide-", RELEASE, ".parquet")
)
DOC_CSV <- TIMEFE_SIGNAL_DOC_CSV
OP_CSV <- TIMEFE_OP_CSV
META_CSV <- TIMEFE_META_REPLICATIONS_CSV

dir.create(RAW_DIR, recursive = TRUE, showWarnings = FALSE)
dir.create(PROCESSED_DIR, recursive = TRUE, showWarnings = FALSE)

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

message("Initializing Open Source Asset Pricing release ", RELEASE, " ...")
openap <- OpenAP$new(release_year = RELEASE)

if (!file.exists(DOC_CSV) || file.info(DOC_CSV)$size == 0) {
  message("Downloading signal documentation ...")
  signal_doc <- as.data.table(openap$dl_signal_doc())
  fwrite(signal_doc, DOC_CSV)
}

if (!file.exists(OP_CSV) || file.info(OP_CSV)$size == 0) {
  message("Downloading original CZ portfolio returns ...")
  fwrite(as.data.table(openap$dl_port("op")), OP_CSV)
}

if (!file.exists(META_CSV) || file.info(META_CSV)$size == 0) {
  mapping_url <- paste0(
    "https://raw.githubusercontent.com/OpenSourceAP/CrossSection/",
    TIMEFE_CZ_MAPPING_COMMIT,
    "/Docs/Comparison_to_MetaReplications.csv"
  )
  message("Downloading pinned meta-replication mapping ...")
  download.file(mapping_url, META_CSV, mode = "wb", quiet = TRUE)
  if (!file.exists(META_CSV) || file.info(META_CSV)$size == 0) {
    stop("Could not download the pinned meta-replication mapping.",
         call. = FALSE)
  }
}
mapping_sha256 <- digest::digest(
  META_CSV, algo = "sha256", file = TRUE
)
if (!identical(mapping_sha256, TIMEFE_CZ_MAPPING_SHA256)) {
  stop(
    "Meta-replication mapping hash mismatch: expected ",
    TIMEFE_CZ_MAPPING_SHA256, ", found ", mapping_sha256, ".",
    call. = FALSE
  )
}

# Use the package to resolve the selected release's official bulk-file URL,
# but avoid dl_all_signals(): that method calls base read.csv() on the 8+ GB
# extracted file and exceeds the devcontainer's memory.
if (!file.exists(ARCHIVE_ZIP) || file.info(ARCHIVE_ZIP)$size == 0) {
  message("Downloading the bulk CZ firm-characteristic archive ...")
  firm_characteristic_url <- openap$get_url("firm_char")
  withr::with_options(
    new = list(timeout = 3600),
    code = download.file(
      firm_characteristic_url, ARCHIVE_ZIP, mode = "wb", quiet = FALSE
    )
  )
}

if (!file.exists(OPENAP_CSV) || file.info(OPENAP_CSV)$size == 0) {
  message("Extracting the bulk CZ firm-characteristic CSV ...")
  archive_listing <- unzip(ARCHIVE_ZIP, list = TRUE)
  csv_member <- archive_listing$Name[
    grepl("signed_predictors_dl_wide\\.csv$", archive_listing$Name)
  ]
  if (length(csv_member) != 1L) {
    stop("Expected exactly one signed_predictors_dl_wide.csv in the archive.",
         call. = FALSE)
  }
  extracted <- unzip(
    ARCHIVE_ZIP, files = csv_member, exdir = RAW_DIR, overwrite = TRUE
  )
  if (length(extracted) != 1L || !file.exists(extracted)) {
    stop("Could not extract the bulk firm-characteristic CSV.",
         call. = FALSE)
  }
  if (!file.rename(extracted, OPENAP_CSV)) {
    stop("Could not move the extracted firm-characteristic CSV to ",
         OPENAP_CSV, ".", call. = FALSE)
  }
}

if (!dir.exists(PANEL_PARQUET)) {
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
  csv_columns <- names(fread(OPENAP_CSV, nrows = 0L))
  csv_types <- setNames(
    lapply(csv_columns, function(column) {
      if (column %in% c("permno", "yyyymm")) int32() else float64()
    }),
    csv_columns
  )
  # Several sparse characteristics are empty in Arrow's inference sample and
  # would otherwise be assigned the unsupported `null` type.
  openap_dataset <- open_csv_dataset(
    OPENAP_CSV,
    schema = do.call(schema, csv_types),
    skip = 1L,
    col_names = csv_columns
  )
  merged_dataset <- openap_dataset |>
    left_join(arrow_table(crsp_signals), by = c("permno", "yyyymm"))
  write_dataset(
    merged_dataset,
    path = PANEL_PARQUET,
    format = "parquet",
    existing_data_behavior = "error"
  )
}

panel_dataset <- open_dataset(PANEL_PARQUET, format = "parquet")
panel_rows <- panel_dataset |>
  summarise(rows = n()) |>
  collect() |>
  pull(rows)
signal_columns <- setdiff(
  panel_dataset$schema$names, c("permno", "yyyymm")
)
message(
  "Saved ", format(panel_rows, big.mark = ","), " firm-month rows and ",
  length(signal_columns), " characteristics to ", PANEL_PARQUET
)
message("Saved signal documentation to ", DOC_CSV)
message("Saved original portfolio returns to ", OP_CSV)
message("Saved pinned meta-replication mapping to ", META_CSV)
