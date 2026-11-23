# R/pipeline_helpers.R
# Shared helpers for pipeline scripts.
# Assumes SCRIPT_ID is defined in the calling script.

safe_write_csv <- function(df, path) {
  fs::dir_create(dirname(path), recurse = TRUE)
  readr::write_csv(df, path)
  invisible(path)
}

make_latest <- function(dir, artifact, ext) {
  if (!exists("SCRIPT_ID", inherits = TRUE)) {
    stop("make_latest() requires SCRIPT_ID to be defined.")
  }
  script_id <- get("SCRIPT_ID", inherits = TRUE)
  fs::path(dir, glue::glue("{script_id}_{artifact}_latest.{ext}"))
}

make_run <- function(dir, artifact, ext, stamp) {
  if (!exists("SCRIPT_ID", inherits = TRUE)) {
    stop("make_run() requires SCRIPT_ID to be defined.")
  }
  script_id <- get("SCRIPT_ID", inherits = TRUE)
  fs::path(dir, glue::glue("{script_id}_{artifact}_{stamp}.{ext}"))
}

qc_row <- function(metric, value, notes = NA_character_) {
  if (!exists("SCRIPT_ID", inherits = TRUE)) {
    stop("qc_row() requires SCRIPT_ID to be defined.")
  }
  script_id <- get("SCRIPT_ID", inherits = TRUE)
  
  tibble::tibble(
    timestamp = Sys.time(),
    script_id = script_id,
    metric    = metric,
    value     = as.character(value),
    notes     = notes
  )
}

write_qc <- function(qc_tbl, file_latest, file_run) {
  safe_write_csv(qc_tbl, file_latest)
  safe_write_csv(qc_tbl, file_run)
  invisible(TRUE)
}