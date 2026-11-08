# Where transcribe() keeps what it has read.
#
# By default nothing is written to the user's computer: the results come back
# to R, and output() saves them. The pages read are still not lost when a run
# stops: every record is also kept in memory for the rest of the R session
# (the "session store" at the end of this file), so running the same line
# again continues instead of paying twice. Only a results folder (save_to)
# survives closing R. It holds
#   <page>.json        one record per page image (status, settings, tokens,
#                      cost, the model's verbatim reply and its parsed data)
#   contract.json      the full instructions and codebook of every contract
#                      used in this folder, keyed by contract id
#   batch-jobs.json    only while a batch job is pending
#   superseded/        earlier results replaced by a redo, one folder per run
#                      (never deleted: they were paid for)
# plus, only with log = TRUE, run_manifest.jsonl, token_usage.txt and
# errors.log. Every write is atomic (see .write_raw_atomic()).

.record_version <- 1L

# The folder's own files, which a page record must never write over.
.control_files <- c("contract.json", "batch-jobs.json")

# A page called contract.jpg would write its record over contract.json (and
# be paid for again on every run, as the record is never read back). Stems
# spelled like a control file, in any case and with any number of leading
# underscores, get one more leading underscore: contract -> _contract,
# _contract -> __contract, so two pages never share a record file.
.record_file <- function(dir, file) {
  stem <- .safe_stem(.stem(file))
  clash <- paste0(tolower(sub("^_+", "", stem)), ".json") %in% .control_files
  stem[clash] <- paste0("_", stem[clash])
  file.path(dir, paste0(stem, ".json"))
}

# The folder suggested (never created) when a long run has none.
.default_results_dir <- function(pages) file.path(attr(pages, "folder") %||% dirname(pages$image[1]), "extraction")

# NULL (or FALSE) = no folder: nothing is written. A folder is used only when
# the user names one, so there is never a question about where to write.
.results_dir <- function(pages, save_to, call = rlang::caller_env()) {
  if (is.null(save_to) || isFALSE(save_to)) return(NULL)
  .check_string(save_to, arg = "save_to", call = call)
  .refuse_library_path(save_to, call = call)
  if (!dir.exists(save_to)) dir.create(save_to, recursive = TRUE)
  dir <- .show_path(save_to)
  if (.Platform$OS.type == "windows" && nchar(dir) + max(nchar(pages$file)) + 6 > 240) {
    .warn(c("The results folder path is very long ({nchar(dir)} characters); Windows may refuse to write some files.",
            "i" = "Move the project to a shorter path, e.g. {.path C:/research/}."))
  }
  # leftovers of an interrupted write
  unlink(list.files(dir, pattern = "^\\.amanuensis-", full.names = TRUE, all.files = TRUE))
  dir
}

.write_record <- function(dir, rec) {
  if (is.null(dir)) return(invisible(NULL))
  .write_json(rec, .record_file(dir, rec$file), pretty = TRUE)
}

# All page records in a folder, keyed by .page_key(file). A damaged record is
# skipped (its page will be redone) with one warning.
.read_records <- function(dir) {
  if (is.null(dir) || !dir.exists(dir)) return(list())
  files <- list.files(dir, pattern = "\\.json$", full.names = TRUE)
  files <- files[!tolower(basename(files)) %in% .control_files]
  out <- list()
  damaged <- character()
  for (f in files) {
    rec <- tryCatch(.read_json(f), error = function(e) NULL)
    if (is.null(rec) || is.null(rec$file) || is.null(rec$status)) {
      damaged <- c(damaged, basename(f))
      next
    }
    out[[.page_key(rec$file)]] <- rec
  }
  if (length(damaged)) {
    .warn(c("{length(damaged)} result file{?s} could not be read and will be redone: {.file {utils::head(damaged, 5)}}."))
  }
  out
}

# ------- contracts ------------------------------------------------------------

.contract <- function(instr, cb) {
  list(id = paste0(instr$sha12, "-", cb$sha12), instructions = instr, codebook = cb)
}

.contract_entry <- function(contract) {
  cb <- contract$codebook
  instr <- contract$instructions
  vars <- cb$variables
  list(
    instructions = instr$full,
    instructions_source = instr$source,
    rules = instr$rules_id %||% (if (isTRUE(instr$rules)) .rules_id else "none"),
    schema = cb$schema,
    codebook = lapply(seq_len(nrow(vars)), function(i) list(
      variable = vars$variable[i], type = vars$type[i], level = vars$level[i],
      description = vars$description[i], values = as.list(vars$values[[i]]))),
    kind = cb$kind, free_text = cb$free_text, record = cb$record, codebook_source = cb$source,
    generator = paste0("amanuensis ", utils::packageVersion("amanuensis")),
    first_used = .utc_now()
  )
}

.read_contracts <- function(dir) {
  path <- file.path(dir, "contract.json")
  if (is.null(dir) || !file.exists(path)) return(list())
  x <- tryCatch(.read_json(path), error = function(e) NULL)
  x$contracts %||% list()
}

.save_contract <- function(dir, contract) {
  if (is.null(dir)) return(invisible())
  all <- .read_contracts(dir)
  if (!is.null(all[[contract$id]])) return(invisible())
  all[[contract$id]] <- .contract_entry(contract)
  .write_json(list(contracts = all), file.path(dir, "contract.json"), pretty = TRUE)
}

# Rebuild codebook and instructions objects from a stored contract entry.
.contract_objects <- function(entry, id) {
  vars <- data.frame(
    variable = vapply(entry$codebook, function(v) v$variable, ""),
    type = vapply(entry$codebook, function(v) v$type, ""),
    level = vapply(entry$codebook, function(v) v$level, ""),
    description = vapply(entry$codebook, function(v) v$description %||% "", ""),
    stringsAsFactors = FALSE)
  vars$values <- lapply(entry$codebook, function(v) as.character(unlist(v$values)))
  ids <- strsplit(id, "-", fixed = TRUE)[[1]]
  cb <- structure(list(variables = vars, schema = entry$schema, kind = entry$kind,
                       free_text = isTRUE(entry$free_text), record = entry$record,
                       source = entry$codebook_source %||% "contract.json", sha12 = ids[2]),
                  class = "amanuensis_codebook")
  instr <- structure(list(text = entry$instructions, rules = !identical(entry$rules, "none"),
                          full = entry$instructions, source = entry$instructions_source %||% "contract.json",
                          sha12 = ids[1]),
                     class = "amanuensis_instructions")
  list(codebook = cb, instructions = instr)
}

# Plain words for what differs between two contract entries.
.describe_change <- function(old, new) {
  out <- character()
  if (!identical(old$instructions, new$instructions)) {
    a <- strsplit(old$instructions %||% "", "\n", fixed = TRUE)[[1]]
    b <- strsplit(new$instructions %||% "", "\n", fixed = TRUE)[[1]]
    n <- length(setdiff(b, a)) + length(setdiff(a, b))
    out <- c(out, paste0("instructions: ", n, " line", if (n != 1) "s", " changed"))
  }
  vo <- vapply(old$codebook %||% list(), function(v) v$variable, "")
  vn <- vapply(new$codebook %||% list(), function(v) v$variable, "")
  added <- setdiff(vn, vo)
  removed <- setdiff(vo, vn)
  if (length(added)) out <- c(out, paste0("codebook: added ", paste(added, collapse = ", ")))
  if (length(removed)) out <- c(out, paste0("codebook: removed ", paste(removed, collapse = ", ")))
  if (!length(added) && !length(removed) && !identical(old$schema, new$schema)) {
    out <- c(out, "codebook: descriptions, types or values changed")
  }
  if (!identical(old$kind, new$kind) || !identical(isTRUE(old$free_text), isTRUE(new$free_text))) {
    out <- c(out, "codebook: table/free text setting changed")
  }
  if (!length(out)) out <- "the package's standard wording changed"
  out
}

# ------- batch jobs -----------------------------------------------------------

.jobs_path <- function(dir) file.path(dir, "batch-jobs.json")

.read_jobs <- function(dir) {
  if (is.null(dir) || !file.exists(.jobs_path(dir))) return(list())
  x <- tryCatch(.read_json(.jobs_path(dir)), error = function(e) {
    .abort(c("The batch job list {.path {(.jobs_path(dir))}} is damaged.",
             "i" = "If you know the job finished or was cancelled, delete that file and run again."))
  })
  x$jobs %||% list()
}

.write_jobs <- function(dir, jobs) {
  path <- .jobs_path(dir)
  if (!length(jobs)) {
    if (file.exists(path)) unlink(path)
    return(invisible())
  }
  .write_json(list(jobs = jobs), path, pretty = TRUE)
}

# Pages in the batch jobs recorded in a folder (not collected yet).
.pending_job_files <- function(dir) {
  jobs <- tryCatch(.read_jobs(dir), error = function(e) list())
  unlist(lapply(jobs, function(j) vapply(j$pages, function(p) p$file %||% "", ""))) %||% character()
}

# ------- superseded records ---------------------------------------------------

.superseded_dir <- function(dir, time = Sys.time()) file.path(dir, "superseded", format(time, "%Y%m%d-%H%M%S"))

# Move the records of these pages out of the way, into superseded/<time>/ (the
# folder is made only when something moves), never deleting them. A record
# already there under the same name (two runs in the same second) is never
# written over. Returns how many records were moved.
.supersede <- function(dir, files, dest = .superseded_dir(dir), quiet = FALSE) {
  if (is.null(dir) || !length(files)) return(invisible(0L))
  moved <- 0L
  for (f in files) {
    from <- .record_file(dir, f)
    if (!file.exists(from)) next
    dir.create(dest, recursive = TRUE, showWarnings = FALSE)
    to <- file.path(dest, basename(from))
    i <- 1L
    while (file.exists(to)) {
      to <- file.path(dest, paste0(.stem(from), "-", i, ".json"))
      i <- i + 1L
    }
    if (file.rename(from, to)) moved <- moved + 1L
  }
  if (!quiet && moved) .inform(c("i" = "Moved {moved} earlier result{?s} to {.path {(.show_path(dest))}} (kept, not deleted)."))
  invisible(moved)
}

.utc_now <- function() format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")

# ------- the session store ------------------------------------------------------

# Every page record of a run without a results folder is also kept here, in
# memory, until R is closed: keyed by the run's fingerprint (contract, model,
# resolution, thinking) and the image's full path. The same line run again
# (after Esc, an error, a budget stop or a limit) reuses these records instead
# of sending the pages again; a page whose image changed size is read again,
# as with a folder. The full path, not the file name, is the key, so two
# folders that both hold p001.jpg never share a record. At most
# .session_max_runs fingerprints are kept (the oldest is dropped), and
# options(amanuensis.session = FALSE) turns the store off.
.session <- new.env(parent = emptyenv())
.session_max_runs <- 20L

.session_on <- function() isTRUE(getOption("amanuensis.session", TRUE))

.session_keep <- function(rec, image) {
  if (!.session_on() || is.null(image) || is.na(image)) return(invisible())
  fp <- .record_fingerprint(rec)
  runs <- .session$runs %||% list()
  if (is.null(runs[[fp]])) {
    runs[[fp]] <- list()
    if (length(runs) > .session_max_runs) runs <- runs[-1]
  }
  runs[[fp]][[.show_path(image)]] <- rec
  .session$runs <- runs
  invisible()
}

# The records this session holds for these pages under fingerprint `fp`,
# keyed by .page_key(file) as records read from a folder are.
.session_records <- function(fp, pages) {
  if (!.session_on()) return(list())
  held <- (.session$runs %||% list())[[fp]]
  if (!length(held)) return(list())
  hit <- match(.show_path(pages$image), names(held))
  out <- held[stats::na.omit(hit)]
  names(out) <- .page_key(pages$file[!is.na(hit)])
  out
}

# For tests, and for anyone who wants this session to forget what it read.
.session_reset <- function() {
  .session$runs <- NULL
  invisible()
}
