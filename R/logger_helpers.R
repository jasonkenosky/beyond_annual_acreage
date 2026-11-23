# ============================================================
# File: logger_helpers.R
# Purpose: Reusable logging utilities for reproducible R scripts
# Author: Jay Kay
# ============================================================

suppressPackageStartupMessages({
  library(glue)
  library(tibble)
  library(readr)
  library(dplyr)
})

# ---- Internal helpers -------------------------------------------------------

.safe_cat <- function(...) {
  tryCatch(cat(...), error = function(e) invisible(NULL))
}

.safe_sink_off <- function(type = c("output", "message"), max_iter = 50L) {
  type <- match.arg(type)

  # Message sinks are the ones that can get weird; never allow infinite loops.
  tryCatch({
    n0 <- sink.number(type = type)
    if (is.na(n0) || n0 <= 0) return(invisible(TRUE))

    i <- 0L
    while (sink.number(type = type) > 0 && i < max_iter) {
      sink(NULL, type = type)
      i <- i + 1L
    }

    # If still not cleared, report once (but don't crash).
    if (sink.number(type = type) > 0) {
      .safe_cat(glue("[WARN  {Sys.time()}] Could not fully clear {type} sinks ",
                     "after {max_iter} attempts. You may need to restart R.\n"))
    }
  }, error = function(e) invisible(NULL))

  invisible(TRUE)
}

.safe_close <- function(con) {
  tryCatch({
    if (!is.null(con) && inherits(con, "connection") && isOpen(con)) close(con)
  }, error = function(e) invisible(NULL))
}

# ---- Create log file and sinks ---------------------------------------------

start_log <- function(script_id, dir_logs) {

  dir.create(dir_logs, showWarnings = FALSE, recursive = TRUE)

  run_id <- format(Sys.time(), "%Y%m%d-%H%M%S")
  log_file <- file.path(dir_logs, glue("{script_id}_{run_id}.log"))

  # Defensive: if a prior run crashed, clear *output* sinks only once.
  # We do not sink messages in this project.
  tryCatch({
    while (sink.number(type = "output") > 0) sink(NULL, type = "output")
  }, error = function(e) invisible(NULL))

  log_con <- file(log_file, open = "wt")

  # Output: log + console
  sink(log_con, type = "output", split = TRUE)

  .safe_cat("--------------------------------------------------\n")
  .safe_cat(glue("Script:      {script_id}\n"))
  .safe_cat(glue("Started:     {Sys.time()}\n"))
  .safe_cat(glue("Run id:      {run_id}\n"))
  .safe_cat(glue("Log file:    {log_file}\n"))
  .safe_cat(glue("Working dir: {getwd()}\n"))
  .safe_cat("--------------------------------------------------\n\n")

  list(
    script_id = script_id,
    run_id = run_id,
    log_file = log_file,
    log_con = log_con
  )
}

# ---- Stop Log -------------------------------------------------------------

stop_log <- function(log_con) {

  # Turn off output sink(s)
  tryCatch({
    while (sink.number(type = "output") > 0) sink(NULL, type = "output")
  }, error = function(e) invisible(NULL))

  # Close connection
  .safe_close(log_con)

  invisible(TRUE)
}

# ---- System info ----------------------------------------------------------

log_system_info <- function() {

  cat("\n")
  cat("--------------------------------------------------\n")
  cat("System information\n")
  cat("--------------------------------------------------\n")

  si <- sessionInfo()

  log_info(glue("R version:    {si$R.version$major}.{si$R.version$minor}"))
  log_info(glue("Platform:     {si$platform}"))
  log_info(glue("Running:      {si$running}"))
  log_info(glue("Timezone:     {Sys.timezone()}"))
  log_info(glue("Locale:       {Sys.getlocale()}"))
  log_info(glue("Working dir:  {getwd()}"))

  # Memory (best-effort, platform-aware)
  mem <- tryCatch({
    gc_out <- gc()
    round(sum(gc_out[, "used"]) / 1024, 2)
  }, error = function(e) NA_real_)

  if (!is.na(mem)) {
    cat(glue("R memory used:    {mem} MB\n"))
  }

  cat("--------------------------------------------------\n\n")
}

# ---- Logging utilities -----------------------------------------------------

log_section <- function(title) {
  .safe_cat("\n\n")
  .safe_cat("==================================================\n")
  .safe_cat(glue("{title}\n"))
  .safe_cat("==================================================\n")
}

log_info <- function(msg, ...)  .safe_cat(glue("[INFO  {Sys.time()}] {msg}", ...), "\n")
log_warn <- function(msg, ...)  .safe_cat(glue("[WARN  {Sys.time()}] {msg}", ...), "\n")
log_error <- function(msg, ...) .safe_cat(glue("[ERROR {Sys.time()}] {msg}", ...), "\n")

log_memory <- function(label = NULL) {
  mem <- tryCatch(sum(gc()[,"used"]) / 1024, error = function(e) NA_real_)
  .safe_cat(glue("[MEM   {Sys.time()}] {label} | {round(mem, 2)} MB"), "\n")
}


# ---- Capture packages ------------------------------------------------------

log_packages <- function() {
  pkgs <- loadedNamespaces()

  pkg_tbl <- tibble::tibble(
    package = pkgs,
    version = vapply(pkgs, function(p) {
      as.character(utils::packageVersion(p))
    }, character(1))
  ) |>
    dplyr::arrange(package)

  log_info("Loaded packages and versions:")
  apply(pkg_tbl, 1, function(x) {
    log_info(glue("  {x[['package']]}  ({x[['version']]})"))
  })

  invisible(TRUE)
}

# ---- Timing helper ---------------------------------------------------------

with_timing <- function(label, expr) {
  t0 <- Sys.time()
  log_info(glue("Starting: {label}"))
  res <- eval(expr)
  elapsed <- round(as.numeric(Sys.time() - t0, units = "secs"), 2)
  log_info(glue("Completed: {label} ({elapsed} sec)"))
  log_memory(label)
  res   # ← THIS LINE MUST EXIST
}

# ---- Task map / intent logging ---------------------------------------------

log_task_map <- function(tasks) {
  # tasks: named character vector or list
  # e.g. list("Geometry ops"="sf", "Graph components"="igraph")
  cat("\n")
  cat("--------------------------------------------------\n")
  cat("Task map (what is doing what)\n")
  cat("--------------------------------------------------\n")

  if (is.null(tasks) || length(tasks) == 0) {
    cat("  (none provided)\n")
  } else {
    nms <- names(tasks)
    for (i in seq_along(tasks)) {
      task <- if (!is.null(nms) && nzchar(nms[i])) nms[i] else paste0("Task ", i)
      who  <- as.character(tasks[[i]])
      cat(sprintf("  - %-28s %s\n", task, who))
    }
  }

  cat("--------------------------------------------------\n\n")
}

log_inputs_outputs <- function(inputs = NULL, outputs = NULL) {
  cat("\n")
  cat("--------------------------------------------------\n")
  cat("Inputs / Outputs\n")
  cat("--------------------------------------------------\n")

  if (!is.null(inputs) && length(inputs) > 0) {
    cat("Inputs:\n")
    for (nm in names(inputs)) cat(glue("  - {nm}: {inputs[[nm]]}\n"))
  } else {
    cat("Inputs:\n  (none listed)\n")
  }

  cat("\n")

  if (!is.null(outputs) && length(outputs) > 0) {
    cat("Outputs:\n")
    for (nm in names(outputs)) cat(glue("  - {nm}: {outputs[[nm]]}\n"))
  } else {
    cat("Outputs:\n  (none listed)\n")
  }

  cat("--------------------------------------------------\n\n")
}

log_params <- function(params) {
  cat("\n")
  cat("--------------------------------------------------\n")
  cat("Parameters\n")
  cat("--------------------------------------------------\n")

  if (is.null(params) || length(params) == 0) {
    cat("  (none provided)\n")
  } else {
    for (nm in names(params)) {
      val <- params[[nm]]
      if (length(val) > 1) val <- paste(val, collapse = ", ")
      cat(glue("  - {nm}: {val}\n"))
    }
  }

  cat("--------------------------------------------------\n\n")
}

# ---- Library loader (optional) ---------------------------------------------

library_quiet <- function(pkgs, attach = TRUE) {
  # pkgs: character vector
  # Loads packages and logs version immediately.
  for (p in pkgs) {
    ok <- require(p, character.only = TRUE, quietly = TRUE, warn.conflicts = FALSE)
    if (!ok) {
      log_error(glue("Failed to load package: {p}"))
      stop("Missing package: ", p)
    }
    if (attach) {
      ver <- tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
      log_info(glue("Loaded package: {p} ({ver})"))
    }
  }
  invisible(TRUE)
}

# ---- Diagnostic CSV ---------------------------------------------------------

write_diagnostic <- function(
    file,
    script_id,
    year,
    n_patches,
    n_matched,
    n_new_lineages,
    time_sec,
    warnings = NA,
    errors = NA
) {
  row <- tibble(
    timestamp       = Sys.time(),
    script_id       = script_id,
    year            = year,
    n_patches       = n_patches,
    n_matched       = n_matched,
    n_new_lineages  = n_new_lineages,
    time_sec        = time_sec,
    warnings        = warnings,
    errors          = errors
  )

  tryCatch({
    if (!file.exists(file)) {
      readr::write_csv(row, file)
    } else {
      readr::write_csv(row, file, append = TRUE)
    }
  }, error = function(e) {
    # Don't ever crash the pipeline because diagnostics failed
    invisible(NULL)
  })
}

#. -- write snapshot ---
write_snapshot <- function(snapshot_list, file) {

  lines <- c(
    "RESULT SNAPSHOT",
    "---------------",
    paste(names(snapshot_list), snapshot_list, sep = ": ")
  )

  writeLines(lines, file)

  log_info(glue("Snapshot written: {file}"))

  invisible(file)
}
