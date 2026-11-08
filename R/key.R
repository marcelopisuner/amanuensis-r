# The Gemini API key. set_key() is the one place the package may write outside
# a folder the user named: a line in the user's .Renviron, which is what the
# function is for. It asks first in an interactive session (CRAN policy: no
# writing to the home folder without consent), writes atomically, keeps every
# other line byte for byte, and says exactly which file it touched. The key's
# shape is checked before anything else: a terminal prompt pasted as the key
# once cost five minutes of retries before anyone noticed.

#' Store your Gemini API key
#'
#' Saves your Google Gemini API key so that the other functions can use it.
#' You only need to do this once per computer (or again when you get a new
#' key).
#'
#' Get a key at <https://aistudio.google.com/apikey>. Treat it like a
#' password: never put it in a script, an email or a shared document.
#'
#' The key is always set for the current R session. To have it in future
#' sessions too, it is written as a line `GEMINI_API_KEY=...` in your personal
#' `.Renviron` file, which R reads every time it starts; the function tells
#' you where that file is and asks before writing to it. If a line for the key
#' is already there it is replaced, not duplicated, and every other line is
#' left as it was.
#'
#' Keys from a Google project without billing are free, but Google may use
#' what you send to improve its products. For archival material you may not
#' share (for example, records with personal data), use a key from a project
#' with billing switched on. Batch runs (`transcribe(batch = TRUE)`) also
#' need billing.
#'
#' @param key Your key, as text. Leave it out to be asked for it in a pop-up
#'   window that hides what you type (recommended, so the key does not end up
#'   in your R history).
#' @param store Save the key in your `.Renviron` for future sessions? `NA`
#'   (default) asks you; `TRUE` saves without asking; `FALSE` uses the key in
#'   this session only.
#' @param check Ask Google whether it accepts the key? This costs nothing. If
#'   the check cannot be made (no internet), the key is still kept.
#' @param remove `TRUE` to delete a stored key from your `.Renviron` and from
#'   this session (for shared or classroom computers).
#' @return The path of the `.Renviron` file (invisibly), or `NULL` when nothing
#'   was written.
#' @seealso [check_setup()] to check the key and everything else in one go.
#' @export
#' @examples
#' \dontrun{
#' set_key()                 # asks for the key in a hidden pop-up
#' set_key(store = FALSE)    # this session only
#' set_key(remove = TRUE)    # forget it on this computer
#' }
set_key <- function(key = NULL, store = NA, check = TRUE, remove = FALSE) {
  if (isTRUE(remove)) return(.remove_key())
  if (is.null(key)) {
    if (!interactive()) {
      .abort(c("No key given.",
               "i" = "Run {.code set_key()} once in the R console, where it can ask for the key."),
             class = "amanuensis_error_key")
    }
    key <- askpass::askpass("Paste your Gemini API key (it will not be shown):")
    if (is.null(key)) .abort("No key entered. Nothing was changed.", class = "amanuensis_error_key")
  }
  .check_string(key, arg = "key")
  key <- .check_key_shape(key)

  Sys.setenv(GEMINI_API_KEY = key)
  .inform(c("v" = "Key {.val {(.mask_key(key))}} is set for this R session."))
  other <- Sys.getenv("GOOGLE_API_KEY")
  if (nzchar(other) && !identical(other, key)) {
    .inform(c("i" = "A different {.envvar GOOGLE_API_KEY} is also set; amanuensis uses {.envvar GEMINI_API_KEY} first, so this key wins."))
  }

  path <- .renviron_path()
  if (is.na(store)) {
    store <- interactive() && isTRUE(utils::askYesNo(
      paste0("Save the key in ", .show_path(path), " so you do not have to set it again? (a plain-text file on this computer)"),
      default = TRUE))
    if (!interactive()) {
      .inform(c("i" = "The key is set for this session only. Use {.code set_key(\"...\", store = TRUE)} to save it for future sessions."))
    }
  }
  written <- NULL
  if (isTRUE(store)) {
    if (!grepl("^[A-Za-z0-9_.-]+$", key)) {
      .inform(c("!" = "This key contains characters that {.file .Renviron} cannot hold safely, so it was not saved.",
                "i" = "It works for this session. Copy the key again from {.url https://aistudio.google.com/apikey} if it should not contain those characters."))
    } else {
      .renviron_set(path, "GEMINI_API_KEY", key)
      written <- path
      .inform(c("v" = "Saved in {.path {(.show_path(path))}}; R sessions started from now on will find it.",
                "i" = "To remove it later: {.code set_key(remove = TRUE)}."))
      if (file.exists(".Renviron") && !identical(.show_path(".Renviron"), .show_path(path))) {
        .inform(c("!" = "This folder has its own {.file .Renviron}, which R reads instead of the one above when it starts here.",
                  "i" = "Add the key to {.path {(.show_path('.Renviron'))}} too, or delete that file."))
      }
    }
  }
  if (isTRUE(check)) .check_key_online(key)
  .inform(c("i" = "Costs shown by amanuensis are Google's paid prices. Keys without billing are not charged, but Google may use what you send to improve its products."))
  invisible(written)
}

.check_key_shape <- function(key, call = rlang::caller_env()) {
  key <- trimws(key)
  codes <- utf8ToInt(enc2utf8(key))
  if (!length(codes) || anyNA(codes)) .abort("The key is empty.", call = call, class = "amanuensis_error_key")
  if (any(codes < 33 | codes > 126)) {
    .abort(c("That does not look like an API key: it contains a space, a line break or an unusual character.",
             "i" = "The clipboard probably held something else. Copy only the key from {.url https://aistudio.google.com/apikey} and try again."),
           call = call, class = "amanuensis_error_key")
  }
  if (length(codes) < 20) {
    .abort(c("That does not look like an API key: it has only {length(codes)} character{?s}.",
             "i" = "Gemini keys start with {.val AIza} (older keys, about 39 characters long) or {.val AQ.} (newer keys)."),
           call = call, class = "amanuensis_error_key")
  }
  key
}

.mask_key <- function(key) {
  n <- nchar(key)
  paste0(substr(key, 1, 4), "...", substr(key, n - 2, n), " (", n, " characters)")
}

# R reads R_ENVIRON_USER if set, else ~/.Renviron (?Startup); on Windows "~"
# is R_USER or HOME, usually the Documents folder.
.renviron_path <- function() {
  env <- Sys.getenv("R_ENVIRON_USER")
  if (nzchar(env)) return(path.expand(env))
  path.expand(file.path("~", ".Renviron"))
}

.renviron_lines <- function(path) {
  if (!file.exists(path)) return(character())
  strsplit(.read_text(path), "\n", fixed = TRUE)[[1]]
}

.renviron_set <- function(path, name, value) {
  lines <- .renviron_lines(path)
  line <- paste0(name, "=", value)
  hit <- grepl(paste0("^\\s*", name, "\\s*="), lines)
  if (any(hit)) {
    first <- which(hit)[1]
    lines[first] <- line
    lines <- lines[setdiff(seq_along(lines), setdiff(which(hit), first))]
  } else {
    if (length(lines) && !nzchar(lines[length(lines)])) lines <- lines[-length(lines)]
    lines <- c(lines, line)
  }
  .renviron_write(c(lines, ""), path)
}

.remove_key <- function() {
  Sys.unsetenv("GEMINI_API_KEY")
  path <- .renviron_path()
  lines <- .renviron_lines(path)
  hit <- grepl("^\\s*GEMINI_API_KEY\\s*=", lines)
  if (!any(hit)) {
    .inform(c("v" = "No stored key found in {.path {(.show_path(path))}}; the key is removed from this session."))
    return(invisible(NULL))
  }
  # R may ignore a last line without a newline, so the file keeps one
  .renviron_write(c(lines[!hit], ""), path)
  .inform(c("v" = "Removed the key from {.path {(.show_path(path))}} and from this session."))
  invisible(path)
}

# Both writers of .Renviron (storing and removing the key) go through here.
# The file holds secrets, often other keys besides this one, so on unix it is
# private (0600) after every write, and already while it is written: the
# atomic writer replaces the file with a new temporary one, which the umask
# below makes private before any byte goes into it (the default umask would
# make it readable by every user of the computer). A .Renviron that is a
# symbolic link, as dotfile managers such as stow, chezmoi or yadm make it, is
# written through to the file it points to: replacing the link itself would
# break it and leave the key in the real file, where removing it then fails
# without a word. The messages already name the real file, because
# .show_path() resolves links. Returns the file actually written.
.renviron_write <- function(lines, path) {
  target <- .resolve_link(path)
  unix <- .Platform$OS.type == "unix"
  if (unix) {
    old <- Sys.umask("077")
    on.exit(Sys.umask(old), add = TRUE)
  }
  .write_text(lines, target)
  if (unix) Sys.chmod(target, mode = "0600", use_umask = FALSE)
  invisible(target)
}

# The file at the end of a chain of symbolic links (it may not exist yet).
# Sys.readlink() gives "" for a plain file, and on Windows.
.resolve_link <- function(path) {
  for (i in 1:40) {
    to <- Sys.readlink(path)
    if (is.na(to) || !nzchar(to)) break
    path <- if (grepl("^(/|\\\\|[A-Za-z]:)", to)) to else file.path(dirname(path), to)
  }
  path
}

# The key used by API calls: GEMINI_API_KEY first, then GOOGLE_API_KEY.
.gemini_key <- function(call = rlang::caller_env()) {
  key <- Sys.getenv("GEMINI_API_KEY")
  if (!nzchar(key)) key <- Sys.getenv("GOOGLE_API_KEY")
  if (!nzchar(key)) {
    .abort(c("No Gemini API key found.",
             "i" = "Run {.code set_key()} once and paste your key (get one at {.url https://aistudio.google.com/apikey}).",
             "i" = "To practise without a key, use {.code model = \"mock\"}."),
           class = "amanuensis_error_key", call = call)
  }
  key
}

# TRUE / FALSE / NA (could not check).
.check_key_online <- function(key, quiet = FALSE) {
  req <- httr2::req_url_query(.gemini_request("models", key = key, timeout = 30), pageSize = 1)
  res <- tryCatch(.gemini_perform(req, retries = 1, quiet = TRUE), error = function(e) e)
  if (inherits(res, "error")) {
    if (!quiet) {
      .inform(c("!" = "Could not check the key with Google: {conditionMessage(res)}",
                "i" = "The key is kept. If extraction later says the key was refused, run {.code set_key()} again."))
    }
    return(invisible(NA))
  }
  status <- httr2::resp_status(res)
  if (status < 300) {
    if (!quiet) .inform(c("v" = "Google accepted the key."))
    return(invisible(TRUE))
  }
  if (!quiet) {
    .inform(c("x" = "Google refused the key (HTTP {status}): {(.gemini_error_message(res))}",
              "i" = "Copy the key again from {.url https://aistudio.google.com/apikey} and run {.code set_key()}."))
  }
  invisible(FALSE)
}
