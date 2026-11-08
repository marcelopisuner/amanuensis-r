# The benchmark editor: a small shiny app that shows a page image beside a
# form built from the codebook, and saves the benchmark file after every
# page, so closing the browser or pressing Esc never loses more than the
# page in hand. It edits the same file benchmark() writes as a template
# (an Excel file, or a .txt/.md/.docx file for free text) and is not a
# separate format: what it saves can be opened in Excel, and what was typed
# in Excel opens in the editor.
#
# Everything except the shiny server itself is a plain function of a
# "state" (the book of benchmark.R plus the page in hand), so the form, the
# reading of its inputs, the checks and the saving are tested without a
# browser. shiny is only in Suggests: these functions are reached only after
# rlang::check_installed("shiny").
#
# Judgment calls:
# - A page is marked done only when it is saved with Next page, Save or Save
#   and finish, and only after its values pass the codebook's checks. Going
#   back with Previous keeps (and saves) what was typed without marking the
#   page done.
# - Category and yes/no variables are drop-down menus with an empty first
#   choice, so "not on the page" is always one click away and nothing is
#   preselected for the user.
# - Rows left entirely empty are dropped when saving, so an "Add row" too
#   many does no harm.

.gold_app_css <- paste(
  "body { padding-top: 8px; }",
  ".am-image { height: 86vh; overflow: auto; border: 1px solid #bbb; background: #f2f2f2; }",
  ".am-image img { display: block; max-width: none; }",
  ".am-form { height: 72vh; overflow: auto; padding-right: 6px; }",
  ".am-rows td, .am-rows th { padding: 2px 3px; vertical-align: top; }",
  ".am-rows th { font-weight: 600; white-space: nowrap; }",
  ".am-rows .form-group { margin-bottom: 0; }",
  ".am-rows input, .am-rows select { min-width: 90px; }",
  ".am-buttons { margin-top: 8px; }",
  ".am-buttons .btn { margin: 2px; }",
  ".am-msg-error { color: #a40000; }",
  ".am-msg-ok { color: #1d6b1d; }",
  sep = "\n"
)

# Run the editor on a benchmark file; returns TRUE when the user clicked
# "Save and finish", FALSE when the window was closed or Esc pressed.
.gold_app_run <- function(file, cb = NULL, pages = NULL) {
  rlang::check_installed("shiny", "for the benchmark editor")
  state <- .gold_state(file, cb, pages)
  app <- .gold_app(state)
  .inform(c("i" = "Opening the benchmark editor in your browser. It saves {.file {basename(file)}} after every page.",
            "i" = "Click {.strong Save and finish} when you are done. To stop earlier, close the browser tab or press Esc here: what was saved stays saved."))
  res <- tryCatch(shiny::runApp(app, launch.browser = TRUE, quiet = TRUE),
                  interrupt = function(e) FALSE)
  invisible(isTRUE(res))
}

# ------- the state ------------------------------------------------------------

# Everything the editor needs, read from the benchmark file: the book, the
# pages in order, each page's values as text, which pages are done, and
# where each page image is.
.gold_state <- function(file, cb = NULL, pages = NULL, call = rlang::caller_env()) {
  book <- .gold_load(file, fallback = cb, call = call)
  files <- book$pages$file
  if (!length(files)) {
    fc <- .gold_match_columns(names(book$lines), "file")[[1]]
    files <- if (is.na(fc)) character() else unique(stats::na.omit(trimws(book$lines[[fc]])))
  }
  if (!length(files)) {
    .abort("{.file {basename(file)}} lists no pages to transcribe.", call = call, class = "amanuensis_error_input")
  }
  units <- .gold_units_chr(book$lines, book$cb, files)
  done <- .gold_parse_done(book$pages$done[match(.page_key(files), .page_key(book$pages$file))])
  has_text <- vapply(units, function(u) any(nzchar(trimws(u$page))) || nrow(u$rows) > 0, logical(1))
  done[is.na(done)] <- if (book$ext %in% .gold_text_exts) has_text[is.na(done)] else FALSE
  if (is.null(pages) && !is.null(book$about$images) && !is.na(book$about$images) && dir.exists(book$about$images)) {
    pages <- tryCatch(.as_images(book$about$images), error = function(e) NULL)
  }
  images <- stats::setNames(rep(NA_character_, length(files)), files)
  if (!is.null(pages)) {
    m <- match(.page_key(files), .page_key(pages$file))
    m2 <- match(.page_key(.stem(files)), .page_key(.stem(pages$file)))
    m[is.na(m)] <- m2[is.na(m)]
    images[!is.na(m)] <- pages$image[m[!is.na(m)]]
  }
  first <- which(!done)[1]
  rows <- suppressWarnings(as.integer(book$about$rows %||% NA))
  list(file = file, book = book, files = files, units = units, done = done, images = images,
       current = if (is.na(first)) 1L else first, rows = if (is.na(rows)) NULL else rows)
}

.gold_vars <- function(state) {
  v <- state$book$cb$variables
  list(page = v[v$level == "page", , drop = FALSE], row = v[v$level == "row", , drop = FALSE])
}

# Rows shown for the page in hand: those typed so far, and at least one
# empty row to start from when the codebook has row variables.
.gold_shown_rows <- function(state) {
  v <- .gold_vars(state)
  if (!nrow(v$row)) return(0L)
  u <- state$units[[state$files[state$current]]]
  max(1L, nrow(u$rows))
}

# The page in hand as typed, read from the form's inputs (a shiny input
# list, or any named list). Input ids: p_<j> for the j-th page variable,
# r_<i>_<j> for row i of the j-th row variable.
.gold_form_values <- function(input, state) {
  v <- .gold_vars(state)
  get <- function(id) {
    x <- input[[id]]
    if (is.null(x) || !length(x) || is.na(x[1])) "" else as.character(x[1])
  }
  page <- vapply(seq_len(nrow(v$page)), function(j) get(paste0("p_", j)), "")
  names(page) <- v$page$variable
  n <- .gold_shown_rows(state)
  if (nrow(v$row)) {
    rows <- lapply(seq_len(nrow(v$row)), function(j) vapply(seq_len(n), function(i) get(paste0("r_", i, "_", j)), ""))
    names(rows) <- v$row$variable
    rows <- as.data.frame(rows, stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  } else {
    rows <- data.frame()
  }
  list(page = page, rows = rows)
}

# Problems that stop the page being saved as done, in plain words: values
# that do not fit their codebook type (empty is always fine).
.gold_check_values <- function(values, cb) {
  vars <- cb$variables
  out <- character()
  for (k in seq_len(nrow(vars))) {
    v <- vars$variable[k]
    type <- vars$type[k]
    if (!type %in% c("number", "integer", "boolean")) next
    if (vars$level[k] == "page") {
      x <- if (v %in% names(values$page)) values$page[[v]] else ""
      where <- v
    } else {
      x <- if (v %in% names(values$rows)) values$rows[[v]] else character()
      where <- paste0("row ", seq_along(x), ", ", v)
    }
    conv <- .gold_convert(trimws(x), type, vars$values[[k]])
    bad <- which(!is.na(conv$problem))
    if (length(bad)) out <- c(out, paste0(where[bad], ": \"", x[bad], "\" ", conv$problem[bad]))
  }
  out
}

.gold_store_page <- function(state, values) {
  f <- state$files[state$current]
  state$units[[f]] <- list(page = values$page, rows = values$rows)
  state
}

.gold_add_row <- function(state) {
  v <- .gold_vars(state)
  if (!nrow(v$row)) return(state)
  f <- state$files[state$current]
  rows <- state$units[[f]]$rows
  n <- .gold_shown_rows(state)
  if (nrow(rows) < n) {
    blank <- as.data.frame(lapply(stats::setNames(v$row$variable, v$row$variable), function(x) rep("", n - nrow(rows))),
                           stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
    rows <- if (nrow(rows)) rbind(rows, blank) else blank
  }
  one <- as.data.frame(lapply(stats::setNames(v$row$variable, v$row$variable), function(x) ""),
                       stringsAsFactors = FALSE, check.names = FALSE, optional = TRUE)
  state$units[[f]]$rows <- rbind(rows, one)
  state
}

.gold_delete_row <- function(state) {
  f <- state$files[state$current]
  rows <- state$units[[f]]$rows
  if (nrow(rows)) state$units[[f]]$rows <- rows[-nrow(rows), , drop = FALSE]
  state
}

# Mark the page in hand done: shown as done, and "yes" in the file's pages.
.gold_mark_done <- function(state) {
  f <- state$files[state$current]
  state$done[state$current] <- TRUE
  p <- state$book$pages
  m <- match(.page_key(f), .page_key(p$file))
  if (is.na(m)) {
    p <- rbind(p, data.frame(file = f, done = "yes", notes = NA_character_, stringsAsFactors = FALSE))
  } else {
    p$done[m] <- "yes"
  }
  state$book$pages <- p
  state
}

# The book as it stands in the editor: lines rebuilt from every page's
# values; the pages keep what the file said, except those saved as done.
.gold_state_book <- function(state) {
  book <- state$book
  book$lines <- .gold_lines_chr(state$units, book$cb, state$files)
  book
}

# Save the benchmark file as it stands in the editor (pages saved as done
# are marked yes; see .gold_mark_done()). Returns the file, invisibly.
.gold_app_save <- function(state, file = state$file) {
  book <- .gold_state_book(state)
  book$file <- file
  book$ext <- .ext(file)
  .gold_book_write(book)
  invisible(file)
}

# ------- the page image ---------------------------------------------------------

# Web addresses for the page images: each folder holding a page is served
# under its own prefix (shiny::addResourcePath), so images from several
# folders work. Returns the folders to serve and the address of each page.
.gold_image_urls <- function(images) {
  ok <- !is.na(images)
  dirs <- unique(dirname(images[ok]))
  prefixes <- if (length(dirs)) paste0("amanuensis-pages-", seq_along(dirs)) else character()
  urls <- stats::setNames(rep(NA_character_, length(images)), names(images))
  if (any(ok)) {
    p <- prefixes[match(dirname(images[ok]), dirs)]
    urls[ok] <- paste0(p, "/", vapply(basename(images[ok]), utils::URLencode, "", reserved = TRUE, USE.NAMES = FALSE))
  }
  list(dirs = stats::setNames(dirs, prefixes), urls = urls)
}

.gold_image_ui <- function(state, zoom = 100) {
  f <- state$files[state$current]
  url <- state$urls[[f]] %||% NA_character_
  if (is.na(url)) {
    return(shiny::div(style = "padding: 20px;",
                      shiny::p(paste0("The image of ", f, " was not found. Give the page images to benchmark() ",
                                      "(images = ...) to see it here."))))
  }
  shiny::tags$img(src = url, alt = f, style = sprintf("width: %d%%;", as.integer(zoom)))
}

.gold_page_title <- function(state) {
  f <- state$files[state$current]
  paste0("Page ", state$current, " of ", length(state$files), ": ", f,
         if (isTRUE(state$done[state$current])) " (done)" else "")
}

# ------- the form ---------------------------------------------------------------

.gold_input <- function(id, var, value, label = TRUE) {
  lab <- if (label) var$variable else NULL
  value <- if (is.null(value) || is.na(value)) "" else as.character(value)
  type <- var$type
  if (type == "category") {
    vals <- var$values[[1]]
    choices <- c("", vals)
    names(choices) <- c("(empty)", vals)
    if (nzchar(value) && !value %in% vals) {
      choices <- c(choices, stats::setNames(value, paste0(value, " (not in the codebook)")))
    }
    return(shiny::selectInput(id, lab, choices = choices, selected = value, selectize = FALSE, width = "100%"))
  }
  if (type == "boolean") {
    yn <- .gold_parse_yes_no(value)
    sel <- if (is.na(yn)) "" else if (yn) "yes" else "no"
    return(shiny::selectInput(id, lab, choices = c("(empty)" = "", "yes" = "yes", "no" = "no"),
                              selected = sel, selectize = FALSE, width = "100%"))
  }
  if (var$level == "page" && var$variable == "text" && type == "text") {
    return(shiny::textAreaInput(id, lab, value = value, rows = 22, width = "100%", resize = "vertical"))
  }
  shiny::textInput(id, lab, value = value, width = "100%")
}

.gold_short <- function(x, n = 160) {
  x <- gsub("\\s+", " ", x)
  if (nchar(x) > n) paste0(substr(x, 1, n - 3), "...") else x
}

# The form for the page in hand: page variables as single inputs, row
# variables as a grid with one line of inputs per row.
.gold_form_ui <- function(state) {
  v <- .gold_vars(state)
  f <- state$files[state$current]
  u <- state$units[[f]]
  page_inputs <- lapply(seq_len(nrow(v$page)), function(j) {
    var <- v$page[j, ]
    val <- if (var$variable %in% names(u$page)) u$page[[var$variable]] else ""
    shiny::tagList(.gold_input(paste0("p_", j), var, val),
                   if (nzchar(var$description)) shiny::helpText(.gold_short(var$description)))
  })
  rows_ui <- NULL
  if (nrow(v$row)) {
    n <- .gold_shown_rows(state)
    header <- shiny::tags$tr(shiny::tags$th("row"),
                             lapply(seq_len(nrow(v$row)), function(j) {
                               shiny::tags$th(title = v$row$description[j], v$row$variable[j])
                             }))
    body <- lapply(seq_len(n), function(i) {
      shiny::tags$tr(shiny::tags$td(i), lapply(seq_len(nrow(v$row)), function(j) {
        var <- v$row[j, ]
        val <- if (i <= nrow(u$rows) && var$variable %in% names(u$rows)) u$rows[[var$variable]][i] else ""
        shiny::tags$td(.gold_input(paste0("r_", i, "_", j), var, val, label = FALSE))
      }))
    })
    rows_ui <- shiny::tagList(
      shiny::tags$h4("Rows"),
      if (!is.null(state$rows)) shiny::p(paste0("Type only the first ", state$rows, " rows of the page.")),
      shiny::p(class = "help-block", "One line per row of the page's table, in order. Hover over a column name for its description."),
      shiny::div(style = "overflow-x: auto;",
                 shiny::tags$table(class = "am-rows", shiny::tags$thead(header), shiny::tags$tbody(body))),
      shiny::div(class = "am-buttons",
                 shiny::actionButton("add_row", "Add row"),
                 shiny::actionButton("del_row", "Delete last row"))
    )
  }
  shiny::tagList(
    if (nrow(v$page)) shiny::tags$h4(if (nrow(v$row)) "Page" else "Values"),
    page_inputs,
    rows_ui
  )
}

.gold_message_ui <- function(msg, kind = "ok") {
  if (!length(msg)) return(NULL)
  cls <- if (identical(kind, "error")) "am-msg-error" else "am-msg-ok"
  if (length(msg) == 1) return(shiny::p(class = cls, msg))
  shiny::div(class = cls, shiny::p("Not saved: correct these values first."),
             shiny::tags$ul(lapply(msg, shiny::tags$li)))
}

.gold_app_ui <- function(state) {
  shiny::fluidPage(
    title = paste("Benchmark -", basename(state$file)),
    shiny::tags$head(shiny::tags$style(shiny::HTML(.gold_app_css))),
    shiny::fluidRow(
      shiny::column(7,
        shiny::fluidRow(
          shiny::column(5, shiny::sliderInput("zoom", "Zoom (%)", min = 50, max = 400, value = 100, step = 10,
                                              width = "100%")),
          shiny::column(7, shiny::h4(shiny::textOutput("title", inline = TRUE)))
        ),
        shiny::div(class = "am-image", shiny::uiOutput("image"))
      ),
      shiny::column(5,
        shiny::div(class = "am-form", shiny::uiOutput("form")),
        shiny::uiOutput("message"),
        shiny::div(class = "am-buttons",
                   shiny::actionButton("prev_page", "Previous"),
                   shiny::actionButton("next_page", "Next page"),
                   shiny::actionButton("save", "Save"),
                   shiny::actionButton("finish", "Save and finish", class = "btn-primary"))
      )
    )
  )
}

# ------- the app ----------------------------------------------------------------

.gold_app <- function(state) {
  served <- .gold_image_urls(state$images)
  for (p in names(served$dirs)) shiny::addResourcePath(p, served$dirs[[p]])
  state$urls <- served$urls
  ui <- .gold_app_ui(state)
  server <- function(input, output, session) {
    rv <- shiny::reactiveValues(state = state, page = state$current, version = 0L, msg = NULL, kind = "ok")
    now <- function() shiny::isolate(rv$state)
    capture <- function() {
      s <- now()
      .gold_store_page(s, .gold_form_values(shiny::isolate(shiny::reactiveValuesToList(input)), s))
    }
    show <- function(s) {
      rv$state <- s
      rv$page <- s$current
      rv$version <- shiny::isolate(rv$version) + 1L
    }
    write <- function(s) {
      tryCatch({
        .gold_app_save(s)
        TRUE
      }, error = function(e) {
        rv$msg <- conditionMessage(e)
        rv$kind <- "error"
        FALSE
      })
    }
    # validate, mark done and save; NULL when the page cannot be saved yet
    save_page <- function() {
      s <- capture()
      probs <- .gold_check_values(s$units[[s$files[s$current]]], s$book$cb)
      if (length(probs)) {
        rv$state <- s
        rv$msg <- probs
        rv$kind <- "error"
        return(NULL)
      }
      s <- .gold_mark_done(s)
      if (!write(s)) {
        rv$state <- s
        return(NULL)
      }
      rv$msg <- paste0("Saved ", s$files[s$current], " in ", basename(s$file), ".")
      rv$kind <- "ok"
      s
    }
    output$title <- shiny::renderText({
      rv$version
      .gold_page_title(shiny::isolate(rv$state))
    })
    output$image <- shiny::renderUI({
      rv$page
      .gold_image_ui(shiny::isolate(rv$state), input$zoom %||% 100)
    })
    output$form <- shiny::renderUI({
      rv$version
      .gold_form_ui(shiny::isolate(rv$state))
    })
    output$message <- shiny::renderUI(.gold_message_ui(rv$msg, rv$kind))
    shiny::observeEvent(input$add_row, show(.gold_add_row(capture())))
    shiny::observeEvent(input$del_row, show(.gold_delete_row(capture())))
    shiny::observeEvent(input$next_page, {
      s <- save_page()
      if (is.null(s)) return()
      if (s$current < length(s$files)) {
        s$current <- s$current + 1L
      } else {
        rv$msg <- "That was the last page. Click Save and finish when you are done."
      }
      show(s)
    })
    shiny::observeEvent(input$prev_page, {
      s <- capture()
      if (!write(s)) return()
      s$current <- max(1L, s$current - 1L)
      rv$msg <- NULL
      show(s)
    })
    shiny::observeEvent(input$save, {
      s <- save_page()
      if (!is.null(s)) show(s)
    })
    shiny::observeEvent(input$finish, {
      s <- save_page()
      if (!is.null(s)) shiny::stopApp(TRUE)
    })
    session$onSessionEnded(function() try(shiny::stopApp(FALSE), silent = TRUE))
  }
  shiny::shinyApp(ui, server)
}
