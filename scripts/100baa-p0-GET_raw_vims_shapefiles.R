# ==============================================================
#
# Script Title: 100baa-p0-GET_raw_vims_shapefiles.R
#
# Purpose:
#   To download the raw VIMS yearly SAV survey files directly
#   from the VIMS Mobjack server, unzip each one into its own
#   folder (contents unchanged), and inventory what was
#   downloaded and what each folder contains.
#
# Author: Jason Kenosky
#
# Date Created: 2026-09-11
# Last Updated: 2026-11-25
#
# Description:
#   The yearly VIMS SAV surveys were not flown Bay-wide until
#   1984.  However, the first record in the dataset is for the
#   year 1971.
#
#   This script connects to the VIMS Mobjack server and downloads
#   what is available instead of hardcoding known survey gaps.
#   The script searches the entire year-range (1971 - 2025) and
#   will not fail if a year is missing.
#
#   The survey year is read from the file name:
#     beds84.zip -> 1984, beds00.zip -> 2000, sav2010.zip -> 2010.
#
# Inputs:
#   https://mobjack.vims.edu/sav/gisdata
#
# Outputs:
#   data/raw/vims_shapefiles/[zip name]/   unzipped folders
#   outputs/tables/ download_inventory_latest.csv
#                   download_inventory_[run stamp].csv
#                     one row per survey year attempted
#                   folder_contents_inventory_latest.csv
#                   folder_contents_inventory_[run stamp].csv
#                     one row per file inside each folder
#   outputs/qc/     inventory_qc_latest.csv
#                   inventory_qc_[run stamp].csv
#   outputs/logs/   the run log
#                   inventory_snapshot_latest.txt
#                   inventory_snapshot_[run stamp].txt
#
# Notes:
#   A failed download is not evidence that a year was not
#   surveyed, but it should be known that the year 1988 is not
#   included in the annual SAV survey dataset as it is
#   documented by VIMS that no survey was flown that year.
#
#   Survey year 1984 is considered the official start year of
#   the full Bay-wide annual record.
#
# ==============================================================

# --------------------------------------------------------------
# 0. Housekeeping
# --------------------------------------------------------------
# -- Clear environment and garbage --

rm(list = ls())
gc()

SCRIPT_START_TIME <- Sys.time()

# -- timeout is in seconds; the R default (60) can be too short
#   for the larger zip files. --

options(
  scipen = 999,
  dplyr.summarise.inform = FALSE,
  warnPartialMatchDollar = TRUE,
  timeout = 300
)

# --------------------------------------------------------------
# 1. Check and load packages
# --------------------------------------------------------------
# -- If any are missing the script stops here and says which. --

PKGS_REQUIRED <- c(
  "here", "fs", "dplyr", "tibble", "glue", "stringr"
)

missing_required <- character(0)
for (pkg in PKGS_REQUIRED) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    missing_required <- c(missing_required, pkg)
  }
}

if (length(missing_required) > 0) {
  stop(
    "Required packages are not installed: ",
    paste(missing_required, collapse = ", "),
    "\nInstall them with:\n  install.packages(c(",
    paste0('"', missing_required, '"', collapse = ", "),
    "))",
    call. = FALSE
  )
}

library(here)
library(fs)
library(dplyr)
library(tibble)
library(glue)
library(stringr)

# --------------------------------------------------------------
# 2. Paths / configuration
# --------------------------------------------------------------

# -- Authoritative naming  --
SCRIPT_ID <- "100baa-p0-GET"

# -- Project root (here) --
DIR_ROOT <- here::here()
DIR_DATA <- fs::path(DIR_ROOT, "data")

# -- Input file paths --
VIMS_BASE_URL <- "https://mobjack.vims.edu/sav/gisdata/"

# -- Script output file paths --
DIR_RAW     <- fs::path(DIR_DATA, "raw")
DIR_VIMS    <- fs::path(DIR_RAW, "vims_shapefiles")
DIR_OUTPUTS <- fs::path(DIR_ROOT, "outputs")

# -- Logging file path --
DIR_LOGS <- fs::path(DIR_OUTPUTS, "logs")

# -- QC file path --
DIR_QC <- fs::path(DIR_OUTPUTS, "qc")

# -- Upstream and inspection files --
DIR_TABLES <- fs::path(DIR_OUTPUTS, "tables")

# -- Ensures that folders exist --
fs::dir_create(
  c(DIR_DATA, DIR_OUTPUTS, DIR_LOGS, DIR_QC, DIR_RAW,
    DIR_TABLES, DIR_VIMS),
  recurse = TRUE
)

# -- Parameters ------------------------------------------------
# 1971 is the first year in the record.  2024 is the latest
# survey year with GIS data. The 2025 Annual Report has been 
# released, but so far not the GIS data.  Change
# LAST_SURVEY_YEAR when new surveys are released.

FIRST_SURVEY_YEAR       <- 1971L
LAST_SURVEY_YEAR        <- 2025L
SURVEY_YEARS_TO_ATTEMPT <- FIRST_SURVEY_YEAR:LAST_SURVEY_YEAR

# -- TRUE keeps each .zip next to its unzipped folder --
KEEP_ZIP_FILES <- FALSE

# -- Known fact-O-ronis used only for QC --
BAYWIDE_START_YEAR <- 1984L
NO_SURVEY_YEAR     <- 1988L
LAST_GIS_YEAR      <- 2024L

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

# -- Build the download url for a survey year --
construct_shapefile_url <- function(year_to_attempt) {
  if (year_to_attempt < 2010) {
    two_digit_year <- str_sub(as.character(year_to_attempt),
                              start = 3, end = 4)
    shapefile_file_name <- str_c("beds", two_digit_year, ".zip")
  } else {
    shapefile_file_name <- str_c("sav", year_to_attempt, ".zip")
  }
  shapefile_url <- str_c(VIMS_BASE_URL, shapefile_file_name)
  
  list(file_name = shapefile_file_name, url = shapefile_url)
}

# -- Read the survey year from a single file --.
year_from_file_name <- function(file_name) {
  digits <- str_extract(file_name, "[0-9]+")
  if (is.na(digits)) {
    return(NA_integer_)
  }
  digits_number <- as.integer(digits)
  if (nchar(digits) == 4) {
    return(digits_number)
  }
  if (nchar(digits) == 2) {
    if (digits_number >= 70) {
      return(1900L + digits_number)
    } else {
      return(2000L + digits_number)
    }
  }
  NA_integer_
}

# -- To build or not to build, an empty contents table, is the 
# question we need to ask ourselves everyday.  Or not.  --
contents_template <- tibble::tibble(
  survey_year     = integer(0),
  folder_name     = character(0),
  file_path       = character(0),
  relative_path   = character(0),
  file_name       = character(0),
  file_extension  = character(0),
  file_size_bytes = numeric(0)
)

# -- List files inside unzipped folders (hidden files
#   included, so the inventory shows the folder as-is) --
inventory_extracted_folder <- function(extract_dir, folder_name) {
  all_files <- fs::dir_ls(extract_dir, recurse = TRUE,
                          type = "file", all = TRUE)
  
  if (length(all_files) == 0) {
    return(contents_template)
  }
  
  tibble::tibble(
    survey_year     = year_from_file_name(folder_name),
    folder_name     = folder_name,
    file_path       = as.character(all_files),
    relative_path   = as.character(fs::path_rel(all_files,
                                                start = extract_dir)),
    file_name       = as.character(fs::path_file(all_files)),
    file_extension  = tolower(fs::path_ext(all_files)),
    file_size_bytes = as.numeric(fs::file_size(all_files))
  )
}

download_survey_year <- function(year_to_attempt) {
  shapefile_location <- construct_shapefile_url(year_to_attempt)
  
  folder_name    <- fs::path_ext_remove(shapefile_location$file_name)
  local_zip_path <- fs::path(DIR_VIMS, shapefile_location$file_name)
  extract_dir    <- fs::path(DIR_VIMS, folder_name)
  
  download_timestamp <- Sys.time()
  
  download_result <- tryCatch({
    suppressWarnings(
      download.file(
        url = shapefile_location$url,
        destfile = local_zip_path,
        mode = "wb",
        quiet = TRUE
      )
    )
    "Success! Hoo-ha."
  }, error = function(condition) {
    conditionMessage(condition)
  })
  
  download_successful <- identical(download_result, "Success!")
  
  # -- Size of the download (the .zipfile) --
  zip_size_bytes <- if (download_successful) {
    as.numeric(fs::file_info(local_zip_path)$size)
  } else {
    NA_real_
  }
  
  # -- Unzip into its own folder. A warning during unzip (for
  #   example a damaged zip) is treated the same as a failure. --
  extract_result <- "not attempted"
  
  if (download_successful) {
    extract_result <- tryCatch({
      utils::unzip(local_zip_path, exdir = extract_dir)
      "Success!"
    }, warning = function(condition) {
      conditionMessage(condition)
    }, error = function(condition) {
      conditionMessage(condition)
    })
  }
  
  extract_successful <- identical(extract_result, "Success!")
  
  # -- An empty (or missing) folder is also a failure --
  if (extract_successful) {
    n_files_found <- 0
    if (dir.exists(extract_dir)) {
      n_files_found <- length(
        fs::dir_ls(extract_dir, recurse = TRUE,
                   type = "file", all = TRUE)
      )
    }
    if (n_files_found == 0) {
      extract_successful <- FALSE
      extract_result <- "The zip file contained no files"
    }
  }
  
  # -- Remove a failed download (partial file) --
  if (!download_successful && file.exists(local_zip_path)) {
    fs::file_delete(local_zip_path)
  }
  
  # -- Remove the zip after a good unzip, unless told to keep it --
  if (extract_successful && !KEEP_ZIP_FILES) {
    fs::file_delete(local_zip_path)
  }
  
  zip_kept <- file.exists(local_zip_path)
  
  # -- Collect the error text, if any --
  error_message <- NA_character_
  if (!download_successful) {
    error_message <- download_result
  } else if (!extract_successful) {
    error_message <- extract_result
  }
  
  # -- One record per year to document what happened --
  tibble::tibble(
    survey_year          = year_from_file_name(shapefile_location$file_name),
    attempted_year       = year_to_attempt,
    shapefile_file_name  = shapefile_location$file_name,
    shapefile_url        = shapefile_location$url,
    download_timestamp   = download_timestamp,
    download_outcome     = if (download_successful) "downloaded" else "failed",
    download_file_path   = as.character(local_zip_path),
    zip_size_bytes       = zip_size_bytes,
    zip_kept             = zip_kept,
    extract_outcome      = if (extract_successful) "extracted" else "failed",
    folder_name          = as.character(folder_name),
    extracted_folder_path = if (extract_successful) {
      as.character(extract_dir)
    } else {
      NA_character_
    },
    error_message        = error_message
  )
}

# --------------------------------------------------------------
# 5. Logging setup
# --------------------------------------------------------------
log_meta <- start_log(SCRIPT_ID, DIR_LOGS)

RUN_STAMP <- log_meta$run_id
stopifnot(nzchar(RUN_STAMP))

# -- Output filename builders (from helpers) --

# -- LATEST outputs --
FILE_DOWNLOADS_LATEST <- make_latest(DIR_TABLES, "download_inventory", "csv")
FILE_CONTENTS_LATEST  <- make_latest(DIR_TABLES, "folder_contents_inventory", "csv")
FILE_QC_LATEST        <- make_latest(DIR_QC, "inventory_qc", "csv")
FILE_SNAPSHOT_LATEST  <- make_latest(DIR_LOGS, "inventory_snapshot", "txt")

# -- RUN-stamped outputs --
FILE_DOWNLOADS_RUN <- make_run(DIR_TABLES, "download_inventory", "csv", RUN_STAMP)
FILE_CONTENTS_RUN  <- make_run(DIR_TABLES, "folder_contents_inventory", "csv", RUN_STAMP)
FILE_QC_RUN        <- make_run(DIR_QC, "inventory_qc", "csv", RUN_STAMP)
FILE_SNAPSHOT_RUN  <- make_run(DIR_LOGS, "inventory_snapshot", "txt", RUN_STAMP)

# -- Log system info --
log_section("SYSTEM INFO")
log_system_info()
log_packages()

log_section("PARAMS")
log_params(list(
  FIRST_SURVEY_YEAR = FIRST_SURVEY_YEAR,
  LAST_SURVEY_YEAR  = LAST_SURVEY_YEAR,
  KEEP_ZIP_FILES    = KEEP_ZIP_FILES,
  VIMS_BASE_URL     = VIMS_BASE_URL
))

# -- Log inputs and outputs --
log_section("INPUTS / OUTPUTS")
log_inputs_outputs(
  inputs = list(
    "vims_base_url" = VIMS_BASE_URL
  ),
  outputs = list(
    "download_inventory_latest" = as.character(FILE_DOWNLOADS_LATEST),
    "download_inventory_run"    = as.character(FILE_DOWNLOADS_RUN),
    "folder_contents_latest"    = as.character(FILE_CONTENTS_LATEST),
    "folder_contents_run"       = as.character(FILE_CONTENTS_RUN),
    "inventory_qc_latest"       = as.character(FILE_QC_LATEST),
    "inventory_qc_run"          = as.character(FILE_QC_RUN),
    "snapshot_latest"           = as.character(FILE_SNAPSHOT_LATEST),
    "snapshot_run"              = as.character(FILE_SNAPSHOT_RUN)
  )
)

# --------------------------------------------------------------
# 6. Inputs (validate)
# --------------------------------------------------------------
log_section("INPUT VALIDATION")

qc <- tibble::tibble()

# -- 6.1 Validate the base URL --
if (!nzchar(VIMS_BASE_URL)) {
  log_error("VIMS base URL is empty.")
  stop("VIMS base URL is empty.")
}

qc <- dplyr::bind_rows(
  qc,
  qc_row(
    "vims_base_url_present",
    "TRUE",
    VIMS_BASE_URL
  )
)

# -- 6.2 Validate survey-year parameters --
if (FIRST_SURVEY_YEAR > LAST_SURVEY_YEAR) {
  log_error("FIRST_SURVEY_YEAR cannot be greater than LAST_SURVEY_YEAR.")
  stop("FIRST_SURVEY_YEAR cannot be greater than LAST_SURVEY_YEAR.")
}

qc <- dplyr::bind_rows(
  qc,
  qc_row(
    "survey_year_range_valid",
    "TRUE",
    str_c(FIRST_SURVEY_YEAR, " through ", LAST_SURVEY_YEAR)
  )
)

# -- 6.3 Output folder for the downloads exists --
qc <- dplyr::bind_rows(
  qc,
  qc_row(
    "vims_download_dir_exists",
    as.character(fs::dir_exists(DIR_VIMS)),
    as.character(DIR_VIMS)
  )
)

# -- 6.4 The year-from-file-name function gives the years --
test_names <- c("beds71.zip", "beds84.zip", "beds00.zip",
                "beds09.zip", "sav2010.zip", "sav2024.zip")
test_years <- c(1971L, 1984L, 2000L, 2009L, 2010L, 2024L)
derived_years <- integer(0)
for (test_name in test_names) {
  derived_years <- c(derived_years, year_from_file_name(test_name))
}

if (!identical(derived_years, test_years)) {
  log_error("year_from_file_name() returned unexpected years.")
  stop("year_from_file_name() returned unexpected years.")
}

qc <- dplyr::bind_rows(
  qc,
  qc_row("year_from_file_name_test_passed", "TRUE")
)

# --------------------------------------------------------------
# 7.  Main process
# --------------------------------------------------------------
log_section("PROCESS")

download_attempt_list <- vector(
  "list",
  length(SURVEY_YEARS_TO_ATTEMPT)
)

contents_attempt_list <- list()

for (i in seq_along(SURVEY_YEARS_TO_ATTEMPT)) {
  current_survey_year <- SURVEY_YEARS_TO_ATTEMPT[i]
  
  message("Attempting to download survey year ",
          current_survey_year, " ...")
  
  attempt <- download_survey_year(current_survey_year)
  download_attempt_list[[i]] <- attempt
  
  # -- List the files in the folder, if it was unzipped --
  if (attempt$extract_outcome == "extracted") {
    folder_contents <- inventory_extracted_folder(
      attempt$extracted_folder_path,
      attempt$folder_name
    )
    contents_attempt_list[[length(contents_attempt_list) + 1]] <- folder_contents
  }
  
  log_info(paste0("Survey year ", current_survey_year,
                  ": download ", attempt$download_outcome,
                  ", unzip ", attempt$extract_outcome))
}

download_list <- dplyr::bind_rows(download_attempt_list)

contents_list <- dplyr::bind_rows(contents_template,
                                  contents_attempt_list)

# -- Per-year summary of the folder contents. --
folder_summary <- contents_list |>
  dplyr::group_by(survey_year) |>
  dplyr::summarise(
    n_files_in_folder  = dplyr::n(),
    folder_total_bytes = sum(file_size_bytes),
    extensions_found   = paste(sort(unique(file_extension)),
                               collapse = ", ")
  ) |>
  dplyr::ungroup()

download_list <- download_list |>
  dplyr::left_join(folder_summary, by = "survey_year") |>
  dplyr::arrange(survey_year)

contents_list <- contents_list |>
  dplyr::arrange(survey_year, relative_path)

# -- For QC rows and snapshot --
years_downloaded <- download_list$survey_year[
  download_list$download_outcome == "downloaded"]

years_failed <- download_list$survey_year[
  download_list$download_outcome == "failed"]

years_extracted <- download_list$survey_year[
  download_list$extract_outcome == "extracted"]

years_unzip_failed <- setdiff(years_downloaded, years_extracted)

years_with_shp <- unique(
  contents_list$survey_year[contents_list$file_extension == "shp"]
)
years_without_shp <- setdiff(years_extracted, years_with_shp)

unexpected_failed <- years_failed[
  years_failed >= BAYWIDE_START_YEAR &
    years_failed <= LAST_GIS_YEAR &
    years_failed != NO_SURVEY_YEAR]

# -- Year from the file name must match the year attempted --
n_year_mismatch <- sum(
  download_list$survey_year != download_list$attempted_year,
  na.rm = TRUE
) + sum(is.na(download_list$survey_year))

log_info(paste0("Years downloaded: ", length(years_downloaded)))
log_info(paste0("Years unzipped: ", length(years_extracted)))
log_info(paste0("Years failed: ", length(years_failed)))
log_info(paste0("Files inventoried: ", nrow(contents_list)))
if (length(years_failed) > 0) {
  log_info(paste0("Failed years: ",
                  paste(years_failed, collapse = ", ")))
}

qc <- dplyr::bind_rows(
  qc,
  qc_row(
    "survey_years_attempted",
    as.character(length(SURVEY_YEARS_TO_ATTEMPT))
  ),
  qc_row(
    "survey_years_downloaded",
    as.character(length(years_downloaded))
  ),
  qc_row(
    "survey_years_failed",
    as.character(length(years_failed)),
    if (length(years_failed) > 0) {
      paste(years_failed, collapse = ", ")
    } else {
      "none"
    }
  ),
  qc_row(
    "year_1988_not_downloaded_as_expected",
    as.character(!(NO_SURVEY_YEAR %in% years_downloaded))
  ),
  qc_row(
    "unexpected_failures_1984_to_2024",
    as.character(length(unexpected_failed)),
    if (length(unexpected_failed) > 0) {
      paste(unexpected_failed, collapse = ", ")
    } else {
      "none"
    }
  ),
  qc_row(
    "survey_years_unzipped",
    as.character(length(years_extracted))
  ),
  qc_row(
    "downloaded_but_unzip_failed",
    as.character(length(years_unzip_failed)),
    if (length(years_unzip_failed) > 0) {
      paste(years_unzip_failed, collapse = ", ")
    } else {
      "none"
    }
  ),
  qc_row(
    "unzipped_folders_without_shp_file",
    as.character(length(years_without_shp)),
    if (length(years_without_shp) > 0) {
      paste(years_without_shp, collapse = ", ")
    } else {
      "none"
    }
  ),
  qc_row(
    "files_inventoried",
    as.character(nrow(contents_list))
  ),
  qc_row(
    "year_from_file_name_mismatches",
    as.character(n_year_mismatch)
  )
)

if (length(unexpected_failed) > 0) {
  log_info(paste0("WARNING: unexpected failed years: ",
                  paste(unexpected_failed, collapse = ", ")))
}
if (length(years_unzip_failed) > 0) {
  log_info(paste0("WARNING: downloaded but not unzipped: ",
                  paste(years_unzip_failed, collapse = ", ")))
}
if (length(years_without_shp) > 0) {
  log_info(paste0("WARNING: no .shp file in: ",
                  paste(years_without_shp, collapse = ", ")))
}

# --------------------------------------------------------------
# 8. Write outputs
# --------------------------------------------------------------
log_section("OUTPUTS")

safe_write_csv(download_list, FILE_DOWNLOADS_LATEST)
safe_write_csv(download_list, FILE_DOWNLOADS_RUN)

safe_write_csv(contents_list, FILE_CONTENTS_LATEST)
safe_write_csv(contents_list, FILE_CONTENTS_RUN)

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
  script_id = SCRIPT_ID,
  run_timestamp = format(SCRIPT_END_TIME, "%Y-%m-%d %H:%M:%S"),
  survey_year_range = str_c(
    FIRST_SURVEY_YEAR, " through ", LAST_SURVEY_YEAR
  ),
  years_attempted = length(SURVEY_YEARS_TO_ATTEMPT),
  years_downloaded = length(years_downloaded),
  years_unzipped = length(years_extracted),
  years_failed = length(years_failed),
  unexpected_failures = length(unexpected_failed),
  earliest_year_downloaded =
    if (length(years_downloaded) > 0) {
      min(years_downloaded)
    } else {
      NA_integer_
    },
  latest_year_downloaded =
    if (length(years_downloaded) > 0) {
      max(years_downloaded)
    } else {
      NA_integer_
    },
  total_zip_bytes_downloaded = sum(download_list$zip_size_bytes,
                                   na.rm = TRUE),
  files_inventoried = nrow(contents_list),
  total_unzipped_bytes = sum(contents_list$file_size_bytes,
                             na.rm = TRUE),
  elapsed_sec = round(elapsed_sec, 2)
)

write_snapshot(snapshot, FILE_SNAPSHOT_LATEST)
write_snapshot(snapshot, FILE_SNAPSHOT_RUN)

# --------------------------------------------------------------
# 11. Done
# --------------------------------------------------------------
log_section("DONE")

# -- Confirms that the output files were written --
stopifnot(
  file.exists(FILE_DOWNLOADS_LATEST),
  file.exists(FILE_DOWNLOADS_RUN),
  file.exists(FILE_CONTENTS_LATEST),
  file.exists(FILE_CONTENTS_RUN),
  file.exists(FILE_QC_LATEST),
  file.exists(FILE_QC_RUN),
  file.exists(FILE_SNAPSHOT_LATEST),
  file.exists(FILE_SNAPSHOT_RUN)
)

log_info("Successful Completion. Now dance a jig.")

# -- Close the log --
stop_log(log_meta$log_con)

cat("\nDownloaded:", length(years_downloaded),
    "of", length(SURVEY_YEARS_TO_ATTEMPT), "years\n")
cat("Unzipped:  ", length(years_extracted), "folders\n")
cat("Files inventoried:", nrow(contents_list), "\n")
cat("Failed years:", if (length(years_failed) > 0) {
  paste(years_failed, collapse = ", ")
} else {
  "none"
}, "\n")

# -- Note:  if nothing downloads at all it could mea a connection 
#    or URL problem.  It does not neccessarily meanmissing survey 
#    years. --
if (length(years_downloaded) == 0) {
  stop("No files were downloaded. Check the internet connection ",
       "and VIMS_BASE_URL.")
}

# --------------------------------------------------------------
# 12. Interpretive notes
# --------------------------------------------------------------
#

