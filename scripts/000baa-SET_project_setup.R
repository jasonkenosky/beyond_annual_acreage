# ==============================================================
#
# Script Title: 000baa-SET_project_setup.R
#
# Purpose: To establish the project paths and to verify the
#    basic project environment (packages, folders, logging,
#    QC output and snapshot output).
#
# Author:  Jason Kenosky
#
# Date Created:  2026-11-20
# Last Updated:  2026-11-25
#
# Description: It is assumed that this is an R Studio project.
#    VIMS source files are stored in data/raw/.
#    All outputs are stored in outputs/
#
# Inputs:  None (data/raw/ is only checked, not read).
#
# Outputs:
#    outputs/logs/  run log, snapshot (latest + run-stamped)
#    outputs/qc/    setup QC table (latest + run-stamped)
#
# Notes:  This script is a test of the setup. If it finishes
#    and prints "SETUP OK", logging, QC and snapshot writing
#    all work and the required packages are installed.
#
# ==============================================================

# --------------------------------------------------------------
# 0. Housekeeping
# --------------------------------------------------------------
# -- Clear environment and garbage --

rm(list = ls())
gc()

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  warnPartialMatchDollar = TRUE
)

SCRIPT_START_TIME <- Sys.time()

# --------------------------------------------------------------
# 1. Load Packages
# --------------------------------------------------------------
packages_required  <- c("fs", "here", "tibble", "glue", "sf",
                        "janitor", "readr", "stringr", "purrr",
                        "ggplot2", "lubridate", "xml2", "digest",
                        "broom", "scales", "patchwork", 
                        "strucchange"
)

missing_required  <- character(0)
for (pkg in packages_required) {
  if (requireNamespace(pkg, quitely = TRUE)) {
    missing_required  <- c(missing_required, pkg)
  }
}

if (length(missing_required) > 0) {
  stop(
    "Required packages are not installed: ",
    paste(missing_requried, collapse = ", "),
    "\nInstall them with:\n renv::install(c(", 
    paste0("", missing_required, "", collapse = ", "),
    "))",
    call. = FALSE
  )
}

suppressPackageStartupMessages( {
  library(fs)
  library(here)
  library(dplyr)
  library(tibble)
  library(glue)
  library(sf)
  library(janitor)
})

# --------------------------------------------------------------
# 2. Paths / configuration
# --------------------------------------------------------------

# -- Authoritative naming  -------------------------------------
SCRIPT_ID <- "000baa-SET"

# -- Project root (here) ---------------------------------------
DIR_ROOT <- here::here()
DIR_DATA <- fs::path(DIR_ROOT, "data")
DIR_RAW  <- fs::path(DIR_DATA, "raw")

# -- Input file paths ------------------------------------------
# None for this script

# -- Script output file paths ----------------------------------
DIR_OUTPUTS <- fs::path(DIR_ROOT, "outputs")

# -- Logging file path -----------------------------------------
DIR_LOGS <- fs::path(DIR_OUTPUTS, "logs")

# -- QC file path ----------------------------------------------
DIR_QC <- fs::path(DIR_OUTPUTS, "qc")

# -- Upstream and inspection files -----------------------------
DIR_TABLES <- fs::path(DIR_OUTPUTS, "tables")

# -- Ensures that folders exist --
fs::dir_create(
  c(DIR_RAW, DIR_OUTPUTS, DIR_LOGS, DIR_QC, DIR_TABLES),
  recurse = TRUE
)

# -- Parameters ------------------------------------------------
# None for this script

# --------------------------------------------------------------
# 3. Source helpers (logger + pipeline)
# --------------------------------------------------------------
FILE_LOG_HELPERS  <- here::here("R", "logger_helpers.R")
FILE_PIPE_HELPERS <- here::here("R", "pipeline_helpers.R")

for (f in c(FILE_LOG_HELPERS, FILE_PIPE_HELPERS)) {
  if (!file.exists(f)) {
    stop("Missing helper: ", f)
  }
  source(f)
}

# --------------------------------------------------------------
# 4. Helpers (script-local only)
# --------------------------------------------------------------
# None for this script.

# --------------------------------------------------------------
# 5. Logging setup
# --------------------------------------------------------------
# -- The log is closed explicitly in section 11. (on.exit() only
#   works inside a function, so it is not used here.) --

log_meta <- start_log(SCRIPT_ID, DIR_LOGS)

RUN_STAMP <- log_meta$run_id
stopifnot(nzchar(RUN_STAMP))

# -- Output filename builders (from helpers) --

# -- LATEST outputs --
FILE_QC_LATEST <- make_latest(DIR_QC, "setup_qc", "csv")
FILE_SNAPSHOT_LATEST <- make_latest(DIR_LOGS, "snapshot", "txt")

# -- RUN-stamped outputs --
FILE_QC_RUN <- make_run(DIR_QC, "setup_qc", "csv", RUN_STAMP)
FILE_SNAPSHOT_RUN <- make_run(DIR_LOGS, "snapshot", "txt", RUN_STAMP)

log_section("SYSTEM INFO")
log_system_info()
log_packages()

log_section("PARAMS")
log_params(list(
  SCRIPT_ID = SCRIPT_ID,
  RUN_STAMP = RUN_STAMP,
  DIR_ROOT  = as.character(DIR_ROOT)
))

log_section("INPUTS / OUTPUTS")
log_inputs_outputs(
  inputs = list(
    "raw_data_dir" = as.character(DIR_RAW)
  ),
  outputs = list(
    "qc_latest"       = as.character(FILE_QC_LATEST),
    "qc_run"          = as.character(FILE_QC_RUN),
    "snapshot_latest" = as.character(FILE_SNAPSHOT_LATEST),
    "snapshot_run"    = as.character(FILE_SNAPSHOT_RUN)
  )
)

# --------------------------------------------------------------
# 6. Inputs (validate)
# --------------------------------------------------------------
log_section("INPUT VALIDATION")

qc <- tibble::tibble()

qc <- dplyr::bind_rows(
  qc,
  qc_row("Project root: ", as.character(DIR_ROOT)),
  qc_row("Raw data directory: ", as.character(DIR_RAW)),
  qc_row("Output directory: ", as.character(DIR_OUTPUTS))
)

# -- Folders exist --
qc <- dplyr::bind_rows(
  qc,
  qc_row("raw_dir_exists", as.character(fs::dir_exists(DIR_RAW))),
  qc_row("logs_dir_exists", as.character(fs::dir_exists(DIR_LOGS))),
  qc_row("qc_dir_exists", as.character(fs::dir_exists(DIR_QC))),
  qc_row("tables_dir_exists", as.character(fs::dir_exists(DIR_TABLES)))
)

# -- Required packages --
for (pkg in packages_required) {
  qc <- dplyr::bind_rows(
    qc,
    qc_row(
      paste0("required_package_version: ", pkg),
      as.character(utils::packageVersion(pkg))
    )
  )
}

if (length(missing_later) > 0) {
  log_info(paste(
    "Packages not yet installed (needed later):",
    paste(missing_later, collapse = ", ")
  ))
}

# -- renv --
qc <- dplyr::bind_rows(
  qc,
  qc_row("renv_lockfile_exists", as.character(file.exists(here::here("renv.lock"))))
)

# -- Raw data folder: count the files (not read) --
raw_files <- fs::dir_ls(DIR_RAW, recurse = TRUE, type = "file")
qc <- dplyr::bind_rows(
  qc,
  qc_row("raw_files_found", as.character(length(raw_files)))
)

if (length(raw_files) == 0) {
  log_info("WARNING: no files found in data/raw/")
  warning("No files found in data/raw/", call. = FALSE)
}

# --------------------------------------------------------------
# 7.  Main process
# --------------------------------------------------------------
log_section("PROCESS")

# Nothing in this setup script.
log_info("No data processing in this script.")

# --------------------------------------------------------------
# 8. Write outputs
# --------------------------------------------------------------
log_section("OUTPUTS")

# No data outputs in this script. 
log_info("No data outputs in this script.")

# --------------------------------------------------------------
# 9. QC outputs
# --------------------------------------------------------------
log_section("QC OUTPUTS")

SCRIPT_END_TIME <- Sys.time()
elapsed_sec <- as.numeric(difftime(SCRIPT_END_TIME,
                                   SCRIPT_START_TIME, units = "secs"))

write_qc(qc, FILE_QC_LATEST, FILE_QC_RUN)

# --------------------------------------------------------------
# 10. Snapshot output
# --------------------------------------------------------------
log_section("RESULT SNAPSHOT")

snapshot <- list(
  packages_required_checked = length(packages_required),
  packages_required_missing = length(missing_required),
  packages_later_missing    = length(missing_later),
  raw_files_found           = length(raw_files),
  qc_rows                   = nrow(qc),
  elapsed_sec               = round(elapsed_sec, 2)
)

write_snapshot(snapshot, FILE_SNAPSHOT_LATEST)
write_snapshot(snapshot, FILE_SNAPSHOT_RUN)

# --------------------------------------------------------------
# 11. Done
# --------------------------------------------------------------
log_section("DONE")

# -- Check that the four output files were written --
stopifnot(
  file.exists(FILE_QC_LATEST),
  file.exists(FILE_QC_RUN),
  file.exists(FILE_SNAPSHOT_LATEST),
  file.exists(FILE_SNAPSHOT_RUN)
)

log_info("Success!!  Hoo-ha.")

stop_log(log_meta$log_con)

log_files <- fs::dir_ls(DIR_LOGS, type = "file")
stopifnot(length(log_files) > 0)

cat("\nSetup OK\n")
cat("QC file:       ", as.character(FILE_QC_LATEST), "\n")
cat("Snapshot file: ", as.character(FILE_SNAPSHOT_LATEST), "\n")
cat("Files in logs/:", length(log_files), "\n")

# --------------------------------------------------------------
# 12. Interpretive notes
# --------------------------------------------------------------
#
# If Setup OK prints to screen, then packages, folders, logging, QC
# output and snapshot output all work. 
