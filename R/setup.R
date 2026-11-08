# check_setup(): one look at everything a first session needs, for the setup
# slot of a course or the first day with the package. It spends nothing and
# changes nothing: the key is checked by asking Google for its list of models
# (free), and the only thing written is a one-byte temporary file in the
# working folder, deleted at once. That write is deliberate: file.access()
# cannot be trusted on Windows or in synced folders, which is exactly where
# saving fails.
#
# Each check is one line: OK, NOTE (works, but worth knowing) or PROBLEM
# (something will fail), and every PROBLEM says how to fix it in words a
# first-time R user can follow. The required versions are read from the
# package's own DESCRIPTION, so they cannot drift from what installing
# enforces; the check still matters because a library can change after
# installation (an old pdftools reinstalled by another package's setup).

#' Check that everything is ready
#'
#' Checks, in one go, what the package needs to work on this computer: the R
#' version, the packages it relies on (and whether your copy of magick reads
#' HEIC photos, WEBP, TIFF and JPEG 2000), whether a Gemini API key is set,
#' whether Google can be reached and accepts the key, and whether you can
#' save files in your working folder. Run it after installing the package,
#' after [set_key()], or whenever something does not work.
#'
#' Each check prints one line marked `OK`, `NOTE` (it works, but read the
#' note) or `PROBLEM` (something will fail), followed by how to fix it.
#'
#' Nothing is changed and nothing is paid for. The key check asks Google for
#' its list of models, which is free. To check that you can save files, a
#' tiny temporary file is written in your working folder and deleted at once;
#' that is the only thing written.
#'
#' @param key Check for a Gemini API key? Default `TRUE`. Use `FALSE` when you
#'   will only practise with `model = "example"` or `"mock"`.
#' @param online Contact Google to check that it can be reached and, if a key
#'   is set, that it accepts the key? Default `TRUE`. Use `FALSE` without an
#'   internet connection.
#' @return A data frame with one row per check (columns `check`, `status`,
#'   `detail` and `fix`), invisibly. Printing it shows the lines again.
#' @seealso [set_key()] to store your key.
#' @export
#' @examples
#' # everything except contacting Google
#' check_setup(online = FALSE)
#'
#' \dontrun{
#' check_setup()    # also checks that Google accepts your key
#' }
check_setup <- function(key = TRUE, online = TRUE) {
  if (!is.logical(key) || length(key) != 1 || is.na(key)) {
    .abort("{.arg key} must be {.code TRUE} or {.code FALSE}.", class = "amanuensis_error_input")
  }
  if (!is.logical(online) || length(online) != 1 || is.na(online)) {
    .abort("{.arg online} must be {.code TRUE} or {.code FALSE}.", class = "amanuensis_error_input")
  }
  rows <- c(
    list(.setup_r_version(), .setup_encoding()),
    .setup_packages(),
    if (key) .setup_key(online) else if (online) list(.setup_reachable(NULL)),
    list(.setup_folder(), .setup_synced())
  )
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  class(out) <- c("amanuensis_setup", "data.frame")
  print(out)
  invisible(out)
}

.setup_row <- function(check, status, detail, fix = "") {
  data.frame(check = check, status = status, detail = detail, fix = fix, stringsAsFactors = FALSE)
}

# ------- R itself -------------------------------------------------------------

.setup_r_version <- function() {
  v <- getRversion()
  if (v >= "4.2.0") return(.setup_row("R", "OK", paste("R", v)))
  .setup_row("R", "PROBLEM",
             paste0("R ", v, " is older than 4.2, which the package needs (from 4.2 on, R reads and writes accented text as UTF-8 on every system)."),
             "Install the current R from https://cran.r-project.org, then install amanuensis again.")
}

# Files are always written as UTF-8; a console that is not UTF-8 only shows
# accented names wrongly, so this is a note, not a problem.
.setup_encoding <- function() {
  if (isTRUE(l10n_info()[["UTF-8"]])) return(.setup_row("text encoding", "OK", "UTF-8"))
  fix <- if (.Platform$OS.type == "windows") {
    "Use R 4.2 or newer, which works in UTF-8 on Windows."
  } else {
    paste0("Add the line LANG=en_US.UTF-8 to your .Renviron (file.edit(\"", .show_path(.renviron_path()), "\") opens it), then restart R.")
  }
  .setup_row("text encoding", "NOTE",
             paste0("R is not running in UTF-8 (locale \"", Sys.getlocale("LC_CTYPE"),
                    "\"): accented names may look wrong in the console. Files are still saved correctly."),
             fix)
}

# ------- packages -------------------------------------------------------------

# Minimum versions from the package's DESCRIPTION (Imports), so this check
# says exactly what installing requires.
.setup_required_versions <- function() {
  path <- system.file("DESCRIPTION", package = "amanuensis")
  if (!nzchar(path)) return(character())
  imports <- read.dcf(path, fields = "Imports")[1, 1]
  if (is.na(imports)) return(character())
  deps <- trimws(strsplit(gsub("\\s+", " ", imports), ",")[[1]])
  deps <- deps[nzchar(deps)]
  pkgs <- sub("\\s*\\(.*$", "", deps)
  mins <- ifelse(grepl(">=", deps), trimws(sub("^.*>=\\s*([^) ]+).*$", "\\1", deps)), "")
  stats::setNames(mins, pkgs)
}

.pkg_version <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) return(NA_character_)
  as.character(utils::packageVersion(pkg))
}

.version_ok <- function(have, need) {
  !is.na(have) && (!nzchar(need) || utils::compareVersion(have, need) >= 0)
}

.higher_version <- function(a, b) {
  if (!nzchar(a)) return(b)
  if (utils::compareVersion(a, b) >= 0) a else b
}

.setup_package <- function(pkg, need, why = NULL, extra = "") {
  have <- .pkg_version(pkg)
  install <- paste0("install.packages(\"", pkg, "\"), then restart R.")
  if (is.na(have)) {
    return(.setup_row(pkg, "PROBLEM", "not installed.", paste0("Run ", install)))
  }
  if (!.version_ok(have, need)) {
    detail <- paste0(have, " is older than the ", need, " the package needs.")
    if (!is.null(why)) detail <- paste(detail, why)
    return(.setup_row(pkg, "PROBLEM", detail, paste0("Run ", install)))
  }
  .setup_row(pkg, "OK", paste0(have, extra))
}

.setup_packages <- function() {
  need <- .setup_required_versions()
  min_of <- function(p) if (p %in% names(need)) need[[p]] else ""
  poppler <- tryCatch(paste0(" (poppler ", pdftools::poppler_config()$version, ")"), error = function(e) "")
  im <- tryCatch(paste0(" (ImageMagick ", as.character(magick::magick_config()$version), ")"), error = function(e) "")
  rows <- list(
    .setup_package("pdftools", .higher_version(min_of("pdftools"), "3.9.1"),
                   why = "Older versions render JPEG-compressed scans as blank white pages.", extra = poppler),
    .setup_package("magick", min_of("magick"), extra = im),
    .setup_formats(),
    .setup_package("readxl", min_of("readxl")),
    .setup_package("writexl", min_of("writexl")),
    .setup_package("httr2", min_of("httr2")),
    .setup_package("cli", min_of("cli"))
  )
  shown <- c("pdftools", "magick", "readxl", "writexl", "httr2", "cli")
  base_pkgs <- c("utils", "stats", "tools", "graphics", "grDevices", "methods")
  others <- setdiff(names(need), c(shown, base_pkgs))
  if (length(others)) {
    have <- vapply(others, .pkg_version, character(1))
    bad <- others[!vapply(seq_along(others), function(i) .version_ok(have[i], need[[others[i]]]), logical(1))]
    rows[[length(rows) + 1]] <- if (length(bad)) {
      .setup_row("other packages", "PROBLEM",
                 paste0("missing or too old: ", paste(bad, collapse = ", "), "."),
                 paste0("Run install.packages(c(", paste0("\"", bad, "\"", collapse = ", "), ")), then restart R."))
    } else {
      .setup_row("other packages", "OK", paste(paste(others, have), collapse = ", "))
    }
  }
  c(rows, list(
    .setup_optional("haven", "for saving Stata .dta files with output()"),
    .setup_optional("shiny", "for the benchmark editor, benchmark(app = TRUE)")
  ))
}

.setup_optional <- function(pkg, what) {
  have <- .pkg_version(pkg)
  if (is.na(have)) {
    return(.setup_row(pkg, "NOTE", paste0("not installed; needed only ", what, "."),
                      paste0("If you need it: install.packages(\"", pkg, "\").")))
  }
  .setup_row(pkg, "OK", paste0(have, " (", what, ")"))
}

# Formats come from magick's build (magick_config()). JPEG is needed for
# every page; the others only for sources in those formats.
.setup_formats <- function() {
  cfg <- tryCatch(magick::magick_config(), error = function(e) NULL)
  if (is.null(cfg)) {
    return(.setup_row("image formats", "PROBLEM", "magick could not report which formats it reads.",
                      "Run install.packages(\"magick\"), then restart R."))
  }
  formats <- c(jpeg = "JPEG", tiff = "TIFF", heic = "HEIC", webp = "WEBP", libopenjp2 = "JPEG 2000")
  have <- vapply(names(formats), function(f) isTRUE(cfg[[f]]), logical(1))
  if (!have[["jpeg"]]) {
    return(.setup_row("image formats", "PROBLEM", "magick cannot read or write JPEG, which every page image needs.",
                      "Run install.packages(\"magick\"), then restart R."))
  }
  if (all(have)) return(.setup_row("image formats", "OK", paste(formats, collapse = ", ")))
  missing <- formats[!have]
  .setup_row("image formats", "NOTE",
             paste0("magick reads ", paste(formats[have], collapse = ", "), " but not ", paste(missing, collapse = ", "),
                    " (HEIC is the format of iPhone photos)."),
             paste("Only matters for sources in those formats: convert them to JPEG or PNG with another program,",
                   "or reinstall magick (on Linux, after installing ImageMagick with libheif and openjpeg)."))
}

# ------- key and Google -------------------------------------------------------

.setup_key <- function(online) {
  gem <- Sys.getenv("GEMINI_API_KEY")
  goo <- Sys.getenv("GOOGLE_API_KEY")
  # a .Renviron saved as UTF-16 (Notepad's "Unicode", PowerShell's echo >)
  # is ignored by R at startup, so any key in it never arrives
  bom <- .file_bom(.renviron_path())
  if (!is.na(bom) && grepl("^UTF-(16|32)", bom)) {
    rows <- list(.setup_row("Gemini key", "PROBLEM",
                            paste0("Your .Renviron (", .show_path(.renviron_path()), ") is saved as ", bom,
                                   ", which R cannot read when it starts, so the key in it is ignored."),
                            "Run set_key() and paste the key: it rewrites the file as UTF-8 and keeps your other lines."))
    if (online) rows[[2]] <- .setup_reachable(NULL)
    return(rows)
  }
  if (!nzchar(gem) && !nzchar(goo)) {
    rows <- list(.setup_row("Gemini key", "PROBLEM", "No Gemini API key found.",
                            paste("Run set_key() and paste your key (get one at https://aistudio.google.com/apikey).",
                                  "To practise without a key, use model = \"example\" or model = \"mock\".")))
    if (online) rows[[2]] <- .setup_reachable(NULL)
    return(rows)
  }
  var <- if (nzchar(gem)) "GEMINI_API_KEY" else "GOOGLE_API_KEY"
  key <- trimws(if (nzchar(gem)) gem else goo)
  shape <- tryCatch({
    .check_key_shape(key)
    NULL
  }, amanuensis_error_key = function(e) e$message)
  rows <- list()
  if (!is.null(shape)) {
    rows[[1]] <- .setup_row("Gemini key", "PROBLEM", paste0(shape, " (from ", var, ")"),
                            "Run set_key() and paste only the key, copied from https://aistudio.google.com/apikey.")
  } else {
    src <- tryCatch(.key_source(var, key), error = function(e) "source could not be told")
    rows[[1]] <- .setup_row("Gemini key", "OK", paste0(.mask_key(key), ", ", src))
  }
  if (nzchar(gem) && nzchar(goo) && !identical(trimws(gem), trimws(goo))) {
    rows[[length(rows) + 1]] <- .setup_row("second key", "NOTE",
      paste0("GOOGLE_API_KEY is also set, to a different key (", .mask_key(trimws(goo)), "); amanuensis uses GEMINI_API_KEY."),
      paste0("Nothing to do if that is intended. Otherwise remove the GOOGLE_API_KEY line from your .Renviron (file.edit(\"",
             .show_path(.renviron_path()), "\") opens it) and restart R."))
  }
  if (online) rows[[length(rows) + 1]] <- .setup_reachable(if (is.null(shape)) key else NULL)
  rows
}

# Where the key in use came from, as far as can be told: R reads a .Renviron
# in the folder it was started in instead of the personal one, so that is
# checked first.
.key_source <- function(var, key) {
  stored_in <- function(path) {
    lines <- .renviron_lines(path)
    hit <- grep(paste0("^\\s*", var, "\\s*="), lines, value = TRUE)
    if (!length(hit)) return(NA)
    value <- trimws(sub("^[^=]*=", "", hit[1]))
    value <- gsub("^[\"']|[\"']$", "", value)
    identical(value, key)
  }
  local <- file.path(getwd(), ".Renviron")
  personal <- .renviron_path()
  if (file.exists(local) && .show_path(local) != .show_path(personal) && isTRUE(stored_in(local))) {
    return(paste0("from ", var, " in ", .show_path(local), " (this folder's own .Renviron, which R reads instead of your personal one)"))
  }
  s <- stored_in(personal)
  if (isTRUE(s)) return(paste0("from ", var, " in ", .show_path(personal)))
  if (isFALSE(s)) {
    return(paste0("from ", var, " (set in this session; ", .show_path(personal), " holds a different key)"))
  }
  paste0("from ", var, " (not stored in ", .show_path(personal), ": set for this session or by your system)")
}

# With a key: Google is asked whether it accepts it (free). Without one:
# only whether Google answers at all (any HTTP reply counts).
.setup_reachable <- function(key) {
  host <- sub("^https?://([^/]+).*$", "\\1", .gemini_base_url())
  unreachable <- .setup_row("Google", "PROBLEM", paste0("Could not reach Google (", host, ")."),
                            paste0("Check the internet connection. On a university or office network, ask whether a proxy or firewall blocks ",
                                   host, "."))
  if (is.null(key)) {
    req <- httr2::request(.gemini_base_url())
    req <- httr2::req_url_path_append(req, "v1beta", "models")
    req <- httr2::req_timeout(req, 20)
    req <- httr2::req_error(req, is_error = function(resp) FALSE)
    resp <- tryCatch(httr2::req_perform(req), error = function(e) NULL)
    if (is.null(resp)) return(unreachable)
    return(.setup_row("Google", "OK", "reachable (no key to check)"))
  }
  accepted <- .check_key_online(key, quiet = TRUE)
  if (isTRUE(accepted)) return(.setup_row("Google", "OK", "reachable, and it accepts the key"))
  if (isFALSE(accepted)) {
    return(.setup_row("Google", "PROBLEM", "Google refused the key.",
                      paste("Copy the key again from https://aistudio.google.com/apikey and run set_key().",
                            "With a university Google account, AI Studio may be switched off by the administrator: try a personal account.")))
  }
  unreachable
}

# ------- working folder -------------------------------------------------------

.setup_folder <- function() {
  wd <- .show_path(getwd())
  tmp <- tempfile(".amanuensis-check-", tmpdir = getwd())
  ok <- tryCatch(suppressWarnings({
    con <- file(tmp, open = "wb")
    writeBin(as.raw(1), con)
    close(con)
    TRUE
  }), error = function(e) FALSE)
  if (file.exists(tmp)) unlink(tmp)
  if (ok) return(.setup_row("working folder", "OK", paste0(wd, " (you can save files there)")))
  .setup_row("working folder", "PROBLEM", paste0("Cannot save files in your working folder, ", wd, "."),
             "Work in a folder of your own: in RStudio, File > New Project; or setwd(\"C:/research/my-project\") with your own folder.")
}

# Sync clients (OneDrive above all, the default Documents folder on many
# Windows computers) lock files while uploading them, which makes a save fail
# now and then. The package retries, but the user should know.
.setup_synced <- function() {
  wd <- .show_path(getwd())
  low <- tolower(wd)
  service <- if (grepl("onedrive", low)) "OneDrive" else
    if (grepl("dropbox", low)) "Dropbox" else
    if (grepl("/library/mobile documents/|icloud", low)) "iCloud Drive" else
    if (grepl("google ?drive|/my drive(/|$)", low)) "Google Drive" else NA_character_
  if (is.na(service)) {
    for (var in c("OneDrive", "OneDriveCommercial", "OneDriveConsumer")) {
      root <- Sys.getenv(var)
      if (nzchar(root) && startsWith(low, tolower(.show_path(root)))) service <- "OneDrive"
    }
  }
  if (is.na(service)) return(.setup_row("synced folder", "OK", "not inside OneDrive, Dropbox, iCloud Drive or Google Drive"))
  .setup_row("synced folder", "NOTE",
             paste0("Your working folder is inside ", service, ", which can lock a file for a moment while it syncs it."),
             paste0("If saving ever fails with \"permission denied\", run the same line again, pause syncing during long runs, ",
                    "or work in a folder outside ", service, "."))
}

# ------- printing -------------------------------------------------------------

#' @export
print.amanuensis_setup <- function(x, ...) {
  width <- max(getOption("width", 80), 60)
  label_w <- max(nchar(x$check), 14) + 2
  indent <- strrep(" ", 2 + label_w + 9)
  cat("amanuensis setup check\n")
  for (i in seq_len(nrow(x))) {
    text <- x$detail[i]
    if (nzchar(x$fix[i]) && x$status[i] != "OK") text <- paste0(text, " Fix: ", x$fix[i])
    wrapped <- strwrap(text, width = max(width - nchar(indent), 30))
    first <- sprintf("  %s%s%s", formatC(x$check[i], width = -label_w), formatC(x$status[i], width = -9), wrapped[1])
    cat(first, "\n", sep = "")
    if (length(wrapped) > 1) cat(paste0(indent, wrapped[-1], "\n"), sep = "")
  }
  n_problem <- sum(x$status == "PROBLEM")
  n_note <- sum(x$status == "NOTE")
  if (!n_problem) {
    cat("Everything needed is in place", if (n_note) paste0(" (", n_note, " note", if (n_note > 1) "s", " above)"), ".\n", sep = "")
  } else {
    cat(n_problem, " problem", if (n_problem > 1) "s", " to fix (lines marked PROBLEM)",
        if (n_note) paste0(", and ", n_note, " note", if (n_note > 1) "s"), ".\n", sep = "")
  }
  invisible(x)
}
