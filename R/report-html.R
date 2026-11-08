# The accuracy report written by score(report = "file.html"): one
# self-contained HTML file (inline CSS and JS, no fonts, scripts or images
# from elsewhere), so it can be e-mailed, archived beside the data, or opened
# years later without a network.
#
# Policies:
# - Built by substituting @@MARKERS@@ into one TEMPLATE string; every marker
#   must appear exactly once in the template and none may survive, so a
#   misspelt marker fails loudly instead of leaving a hole in the page. Data
#   never contains "@" (escaped as &#64;), so it cannot fake a marker.
# - Deterministic: no clock is read; the only dates shown are those of the
#   extraction and of the benchmark's draw. The same score gives the same
#   bytes, so a rebuilt report that did not change is visibly unchanged.
# - Every table is a plain <table> with a header row, no colspan or rowspan
#   and no <th> in the body, so a script (pandas.read_html, rvest) can read
#   it back.
# - Light or dark follows the reader's system (prefers-color-scheme).

.write_score_report <- function(s, path) {
  html <- .score_html(s)
  existed <- file.exists(path)
  .write_text(html, path)
  .inform(c("v" = "Wrote the report {.file {(.show_path(path))}}{if (existed) ' (replaced)' else ''}."))
  invisible(path)
}

.score_html <- function(s) {
  a <- attr(s, "about")
  cells <- attr(s, "cells")
  agree <- identical(a$reference, "run")
  title <- if (agree) "Agreement with the first run" else "Accuracy against your benchmark"
  values <- list(
    TITLE = .html_escape(title),
    HEADING = .html_escape(title),
    SUBTITLE = .html_escape(.score_subtitle(a)),
    STAMP = .report_stamp(a),
    TILES = .report_tiles(s, a, agree),
    NOTES = .report_notes(s, a, agree),
    VARIABLES = .report_variables(s, a),
    COUNTS = .report_counts(cells),
    PAGES = .report_pages(s, a, cells),
    METHOD = .report_method(a),
    FOOTER = .html_escape(paste0("Made with amanuensis ", as.character(utils::packageVersion("amanuensis")),
                                 ". The same score always gives the same file."))
  )
  .fill_template(.score_template, values)
}

# Substitute every @@NAME@@ of the template, checking that the template has
# each marker exactly once and that no "@@" is left.
.fill_template <- function(template, values) {
  pieces <- strsplit(template, "@@", fixed = TRUE)[[1]]
  stopifnot(length(pieces) %% 2 == 1)
  markers <- pieces[seq(2, length(pieces), by = 2)]
  stopifnot(!anyDuplicated(markers), setequal(markers, names(values)))
  pieces[seq(2, length(pieces), by = 2)] <- vapply(markers, function(m) enc2utf8(values[[m]]), "")
  out <- paste(pieces, collapse = "")
  stopifnot(!grepl("@@", out, fixed = TRUE))
  out
}

.html_escape <- function(x) {
  x <- as.character(x)
  x[is.na(x)] <- ""
  x <- gsub("&", "&amp;", x, fixed = TRUE)
  x <- gsub("<", "&lt;", x, fixed = TRUE)
  x <- gsub(">", "&gt;", x, fixed = TRUE)
  x <- gsub("\"", "&quot;", x, fixed = TRUE)
  x <- gsub("'", "&#39;", x, fixed = TRUE)
  gsub("@", "&#64;", x, fixed = TRUE)
}

# Two decimals, never "100.00%" below 100%.
.pct2 <- function(x) {
  vapply(x, function(v) {
    if (is.na(v)) return("&ndash;")
    s <- sprintf("%.2f%%", 100 * v)
    if (v < 1 && s == "100.00%") return("&gt;99.99%")
    if (v > 0 && s == "0.00%") return("&lt;0.01%")
    s
  }, character(1), USE.NAMES = FALSE)
}

.report_stamp <- function(a) {
  bits <- character()
  if (identical(a$reference, "run")) {
    bits <- c(bits, paste0("First run: ", a$reference_model %||% "", .folder_label(a$source)))
  } else {
    src <- if (!is.null(a$source) && !is.na(a$source)) basename(a$source) else "a table given in R"
    d <- a$draw
    draw <- if (!is.null(d$seed)) paste0(" (pages drawn with seed ", d$seed,
                                         if (!is.null(d$date)) paste0(" on ", substr(d$date, 1, 10)), ")") else ""
    bits <- c(bits, paste0("Benchmark: ", src, draw))
  }
  dates <- a$output_dates
  when <- if (length(dates) && !all(is.na(dates))) {
    paste0(", extracted ", if (dates[1] == dates[length(dates)]) dates[1] else paste(dates[1], "to", dates[length(dates)]))
  } else ""
  if (!is.null(a$model) && !is.na(a$model)) {
    bits <- c(bits, paste0(if (identical(a$reference, "run")) "Second run: " else "Output: ", a$model, when,
                           .folder_label(a$output_folder)))
  }
  pairing <- switch(a$align, auto = "by position, or by content where the row counts differ",
                    position = "by position", content = "by content")
  # a benchmark of the first rows only is paired by content wherever it
  # reaches the limit (see .score_page())
  if (identical(a$align, "auto") && !is.null(a$rows_limit)) {
    pairing <- paste0("by content where the benchmark reaches its ", a$rows_limit,
                      " rows; elsewhere by position, or by content where the row counts differ")
  }
  if (a$row_variables && !identical(a$mode, "text")) bits <- c(bits, paste0("Rows paired ", pairing))
  if (!is.null(a$rows_limit)) bits <- c(bits, paste0("first ", a$rows_limit, " rows of each page"))
  paste(.html_escape(bits), collapse = " &middot; ")
}

.folder_label <- function(path) {
  if (is.null(path) || is.na(path) || !nzchar(path)) "" else paste0(" (folder ", basename(path), ")")
}

.report_tiles <- function(s, a, agree) {
  sm <- s$summary
  get <- function(m) sm[sm$measure == m, , drop = FALSE]
  right <- if (agree) "the same" else "right"
  tile <- function(label, r, detail, lenient = NULL) {
    v <- if (nrow(r)) .pct2(r$value) else "&ndash;"
    len <- if (!is.null(lenient) && nrow(lenient)) paste0("<div class=\"d\">lenient ", .pct2(lenient$value), "</div>") else ""
    paste0("<div class=\"tile\"><div class=\"k\">", .html_escape(label), "</div><div class=\"v\">", v,
           "</div><div class=\"d\">", detail, "</div>", len, "</div>")
  }
  lenient <- function(m) if (isTRUE(a$normalize)) get(paste0(m, " (lenient)")) else NULL
  counts <- function(r, what) {
    if (!nrow(r) || r$compared == 0) return(paste("no", what, "to compare"))
    paste(.fmt_int(r$matched), "of", .fmt_int(r$compared), what)
  }
  edits <- function(r, what) {
    if (!nrow(r) || r$compared == 0) return(paste("no", what, "to compare"))
    paste(.fmt_int(r$compared - r$matched), "edits over", .fmt_int(r$compared), what)
  }
  if (identical(a$mode, "text")) {
    return(paste0(
      tile(paste("Characters", right), get("characters"), edits(get("characters"), "characters"), lenient("characters")),
      tile(paste("Words", right), get("words"), edits(get("words"), "words"), lenient("words")),
      tile(if (agree) "Pages identical" else "Pages entirely right", get("pages"), counts(get("pages"), "pages"))
    ))
  }
  pv <- get("page values")
  rows_detail <- if (a$row_variables) counts(get("rows"), "rows") else "no row variables"
  if (nrow(pv) && a$row_variables) rows_detail <- paste0(rows_detail, " &middot; page values: ", .fmt_int(pv$matched), " of ",
                                                         .fmt_int(pv$compared), " pages")
  paste0(
    tile(paste("Rows with every value", right), get("rows"), rows_detail, lenient("rows")),
    tile(paste("Values", right), get("values"), counts(get("values"), "values"), lenient("values")),
    tile(paste("Characters", right, "(text)"), get("characters"), edits(get("characters"), "characters"), lenient("characters"))
  )
}

.report_notes <- function(s, a, agree) {
  notes <- s$notes
  dm <- s$summary[s$summary$measure == "doubt marks", , drop = FALSE]
  if (nrow(dm) && dm$matched > 0) {
    notes <- c(notes, paste0(.doubt_sentence(dm, agree), "."))
  }
  if (!length(notes)) return("")
  paste0("<ul class=\"notes\">", paste0("<li>", .html_escape(notes), "</li>", collapse = ""), "</ul>")
}

.report_variables <- function(s, a) {
  bv <- s$by_variable
  top <- max(c(bv$differences, 0))
  lenient <- isTRUE(a$normalize)
  head <- c("Variable", "Type", "Level", "Compared", "Wrong", "Mismatches", "Values right", "Characters right", "Edits",
            if (lenient) c("Wrong (lenient)", "Values right (lenient)", "Characters right (lenient)"))
  body <- vapply(seq_len(nrow(bv)), function(i) {
    w <- if (top > 0) sprintf("%.1f", 100 * bv$differences[i] / top) else "0.0"
    cells <- c(
      paste0("<td>", .html_escape(bv$variable[i]), "</td>"),
      paste0("<td>", .html_escape(bv$type[i]), "</td>"),
      paste0("<td>", .html_escape(bv$level[i]), "</td>"),
      paste0("<td class=\"num\">", .fmt_int(bv$compared[i]), "</td>"),
      paste0("<td class=\"num\">", .fmt_int(bv$differences[i]), "</td>"),
      paste0("<td><div class=\"bar\"><span style=\"width:", w, "%\"></span></div></td>"),
      paste0("<td class=\"num\">", .pct2(bv$variable_match[i]), "</td>"),
      paste0("<td class=\"num\">", .pct2(bv$character_match[i]), "</td>"),
      paste0("<td class=\"num\">", if (is.na(bv$edit_distance[i])) "&ndash;" else .fmt_int(bv$edit_distance[i]), "</td>"),
      if (lenient) c(paste0("<td class=\"num\">", .fmt_int(bv$lenient_differences[i]), "</td>"),
                     paste0("<td class=\"num\">", .pct2(bv$lenient_variable_match[i]), "</td>"),
                     paste0("<td class=\"num\">", .pct2(bv$lenient_character_match[i]), "</td>"))
    )
    paste0("<tr>", paste(cells, collapse = ""), "</tr>")
  }, character(1))
  .html_table(head, body, "vars")
}

.html_table <- function(head, body, class = NULL) {
  paste0("<table", if (!is.null(class)) paste0(" class=\"", class, "\""), "><thead><tr>",
         paste0("<th>", .html_escape(head), "</th>", collapse = ""), "</tr></thead><tbody>\n",
         paste(body, collapse = "\n"), "\n</tbody></table>")
}

.report_counts <- function(cells) {
  wrong <- !cells$match
  rc <- cells$level == "row"
  rows <- length(unique(paste(cells$file[rc & wrong], cells$pair[rc & wrong], sep = "\r")))
  paste0(.fmt_int(rows), " row", if (rows != 1) "s", " and ", .fmt_int(sum(wrong)), " value", if (sum(wrong) != 1) "s",
         " differ")
}

.report_method <- function(a) {
  p <- c(
    if (identical(a$mode, "text")) c(
      "Characters right: one minus the number of single-character edits (insertions, deletions, substitutions) needed to turn the output into the benchmark, divided by the length of the longer of the two texts, summed over pages.",
      "Words right: the same count made on whole words, so a misread word costs one edit however long it is."
    ) else c(
      "Rows with every value right: rows of the benchmark and of the output paired on each page; a row counts as right only if every variable matches. A row missing from the output, or added by it, counts as a wrong row.",
      "Values right: values that match, out of all values compared; page-level variables count once per page. An empty cell matches an empty cell; text is compared exactly as written, after a standard Unicode normalisation; numbers and yes/no by value.",
      "Characters right (text variables only): one minus the number of single-character edits needed to turn the output into the benchmark, divided by the number of characters (the longer of the two values each time). A small misreading costs little here and a whole value in the other two figures."
    ),
    "A page whose extraction failed counts as entirely wrong; a page not extracted yet is left out and listed.",
    if (isTRUE(a$normalize)) "Lenient figures ignore capital letters, accents, punctuation (including doubt marks) and spacing; numbers are never changed."
  )
  paste0("<p class=\"meta\">", .html_escape(p), "</p>", collapse = "\n")
}

# ------- pages ---------------------------------------------------------------

.report_pages <- function(s, a, cells) {
  bp <- s$by_page
  vars <- a$variables
  cells$html <- .cells_html(cells)
  out <- character(nrow(bp))
  by_file <- split(seq_len(nrow(cells)), factor(cells$file, levels = unique(c(bp$file, cells$file))))
  for (i in seq_len(nrow(bp))) {
    p <- bp[i, ]
    if (p$status == "not run") {
      out[i] <- paste0("<section class=\"page\"><h3>", .html_escape(p$file), "</h3><p class=\"meta\">",
                       "not in the results yet: left out of every figure</p></section>")
      next
    }
    sub <- cells[by_file[[p$file]] %||% integer(), , drop = FALSE]
    meta <- c(p$status,
              if (!is.na(p$alignment) && p$alignment %in% c("position", "content")) paste("rows paired by", p$alignment),
              if (!is.na(p$gold_rows)) paste0(a$labels$ref_col, " ", .plural(p$gold_rows, "row", "rows")),
              if (!is.na(p$output_rows)) paste0(a$labels$out_col, " ", .plural(p$output_rows, "row", "rows")),
              if (isTRUE(p$missing_rows > 0)) paste(p$missing_rows, "missing"),
              if (isTRUE(p$extra_rows > 0)) paste(p$extra_rows, "extra"),
              paste(.plural(p$differences, "value", "values"), "wrong"))
    parts <- paste0("<h3>", .html_escape(p$file), "</h3><p class=\"meta\">", paste(.html_escape(meta), collapse = " &middot; "), "</p>")
    if (identical(a$mode, "text")) {
      parts <- c(parts, .report_text_table(sub, a))
    } else {
      pc <- sub[sub$level == "page" & !sub$separate, , drop = FALSE]
      if (nrow(pc)) parts <- c(parts, .report_page_values(pc, vars, a))
      sc <- sub[sub$separate, , drop = FALSE]
      if (nrow(sc)) parts <- c(parts, "<p class=\"meta\">Other text on the page:</p>", .report_text_table(sc, a))
      rc <- sub[sub$level == "row", , drop = FALSE]
      if (nrow(rc)) parts <- c(parts, .report_rows(rc, vars, a))
    }
    cls <- if (isTRUE(p$differences == 0)) "page same" else "page"
    out[i] <- paste0("<section class=\"", cls, "\">", paste(parts, collapse = "\n"), "</section>")
  }
  paste(out, collapse = "\n")
}

.report_page_values <- function(pc, vars, a) {
  pv <- vars$variable[vars$level == "page" & !vars$variable %in% a$separate]
  tds <- pc$html[match(pv, pc$variable)]
  wrong <- sum(!pc$match)
  tr <- paste0("<tr class=\"", if (wrong) "diff" else "same", "\"><td>page values</td>", paste(tds, collapse = ""),
               .match_td(wrong, nrow(pc)), "</tr>")
  paste0("<div class=\"scroll\">", .html_table(c("", pv, "Match"), tr, "cmp"), "</div>")
}

# The rows of one page as a wide table: one line per pair of rows, one
# column per variable. Every pair has one cell per row variable, so after
# ordering by pair and variable the cells fill a matrix row by row.
.report_rows <- function(rc, vars, a) {
  rv <- vars$variable[vars$level == "row"]
  rc <- rc[order(rc$pair, match(rc$variable, rv)), , drop = FALSE]
  nv <- length(rv)
  np <- nrow(rc) %/% nv
  html <- matrix(rc$html, nrow = np, ncol = nv, byrow = TRUE)
  wrong <- rowSums(matrix(!rc$match, nrow = np, ncol = nv, byrow = TRUE))
  first <- seq(1, nrow(rc), by = nv)
  num <- function(x) ifelse(is.na(x), "<span class=\"miss\">&mdash;</span>", as.character(x))
  trs <- paste0("<tr class=\"", ifelse(wrong > 0, "diff", "same"), "\"><td class=\"num\">", num(rc$row_gold[first]),
                "</td><td class=\"num\">", num(rc$row_output[first]), "</td>",
                apply(html, 1, paste, collapse = ""),
                vapply(wrong, .match_td, "", n = nv), "</tr>")
  head <- c(paste0("Row (", a$labels$ref_col, ")"), paste0("Row (", a$labels$out_col, ")"), rv, "Match")
  paste0("<div class=\"scroll\">", .html_table(head, trs, "cmp"), "</div>")
}

.match_td <- function(wrong, n) {
  if (wrong == 0) return("<td class=\"ok\">&#10003;</td>")
  paste0("<td class=\"bad\">&#10007; ", wrong, " of ", n, "</td>")
}

# Every cell as a <td>: the value once when it matches; otherwise washed
# red, the benchmark's value over the output's, with the differing stretch
# marked.
.cells_html <- function(cells) {
  show <- function(v, miss) {
    out <- .html_escape(v)
    out[is.na(v)] <- "<span class=\"null\">empty</span>"
    out[miss] <- "<span class=\"miss\">missing</span>"
    out
  }
  g <- show(cells$gold, cells$g_missing)
  o <- show(cells$output, cells$o_missing)
  both <- which(!cells$match & !cells$g_missing & !cells$o_missing & !is.na(cells$gold) & !is.na(cells$output))
  for (i in both) {
    m <- .mark_pair(cells$gold[i], cells$output[i])
    g[i] <- m[1]
    o[i] <- m[2]
  }
  ifelse(cells$match, paste0("<td>", g, "</td>"),
         paste0("<td class=\"x\"><div class=\"g\">", g, "</div>\n<div class=\"o\">", o, "</div></td>"))
}

# Mark what lies between the common beginning and the common end of two
# values (enough for the short cells of a table; whole texts get a word diff).
.mark_pair <- function(a, b) {
  ca <- strsplit(a, "", fixed = TRUE)[[1]]
  cb <- strsplit(b, "", fixed = TRUE)[[1]]
  na <- length(ca)
  nb <- length(cb)
  lim <- min(na, nb)
  p <- 0L
  while (p < lim && ca[p + 1L] == cb[p + 1L]) p <- p + 1L
  s <- 0L
  while (s < lim - p && ca[na - s] == cb[nb - s]) s <- s + 1L
  fmt <- function(ch, n, cls) {
    pre <- paste(ch[seq_len(p)], collapse = "")
    mid <- if (n - s > p) paste(ch[(p + 1L):(n - s)], collapse = "") else ""
    suf <- if (s > 0) paste(ch[(n - s + 1L):n], collapse = "") else ""
    paste0(.html_escape(pre), if (nzchar(mid)) paste0("<mark class=\"", cls, "\">", .html_escape(mid), "</mark>"),
           .html_escape(suf))
  }
  c(fmt(ca, na, "g"), fmt(cb, nb, "o"))
}

.report_text_table <- function(tc, a) {
  trs <- vapply(seq_len(nrow(tc)), function(i) {
    cell <- tc[i, ]
    if (cell$match) {
      txt <- if (is.na(cell$gold)) "<span class=\"null\">empty</span>" else .html_escape(cell$gold)
      return(paste0("<tr class=\"same\"><td class=\"txt\">", txt, "</td><td class=\"txt\">", txt, "</td></tr>"))
    }
    if (cell$o_missing) {
      m <- c(.html_escape(cell$gold), "<span class=\"miss\">missing</span>")
    } else {
      m <- .word_diff_html(.blank(cell$gold), .blank(cell$output))
    }
    paste0("<tr class=\"diff\"><td class=\"txt\">", m[1], "</td><td class=\"txt\">", m[2], "</td></tr>")
  }, "")
  head <- c(paste0(toupper(substr(a$labels$ref_col, 1, 1)), substring(a$labels$ref_col, 2)),
            paste0(toupper(substr(a$labels$out_col, 1, 1)), substring(a$labels$out_col, 2)))
  paste0("<div class=\"scroll\">", .html_table(head, trs, "cmp text"), "</div>")
}

# Whole texts: words paired by a longest common subsequence (the alignment
# of .dp_align with "equal" as the only allowed pair); unpaired words are
# marked. Very long pages (over 4 million word pairs) fall back to marking
# the stretch between the common beginning and end.
.word_diff_html <- function(a, b) {
  ta <- if (nzchar(a)) regmatches(a, gregexpr("\\s+|\\S+", a, perl = TRUE))[[1]] else character()
  tb <- if (nzchar(b)) regmatches(b, gregexpr("\\s+|\\S+", b, perl = TRUE))[[1]] else character()
  wa <- which(!grepl("^\\s", ta, perl = TRUE))
  wb <- which(!grepl("^\\s", tb, perl = TRUE))
  if (as.numeric(length(wa)) * length(wb) > 4e6) return(.mark_pair(a, b))
  keep_a <- keep_b <- integer()
  if (length(wa) && length(wb)) {
    eq <- outer(ta[wa], tb[wb], "==")
    pairs <- .dp_align(eq * 1, eq)
    both <- !is.na(pairs$gi) & !is.na(pairs$oi)
    keep_a <- wa[pairs$gi[both]]
    keep_b <- wb[pairs$oi[both]]
  }
  fmt <- function(tok, words, keep, cls) {
    out <- .html_escape(tok)
    mark <- setdiff(words, keep)
    out[mark] <- paste0("<mark class=\"", cls, "\">", out[mark], "</mark>")
    paste(out, collapse = "")
  }
  c(fmt(ta, wa, keep_a, "g"), fmt(tb, wb, keep_b, "o"))
}

# ------- the template ---------------------------------------------------------

.score_template <- '<!DOCTYPE html>
<html lang="en">
<head>
<meta charset="utf-8">
<meta name="viewport" content="width=device-width, initial-scale=1">
<title>@@TITLE@@</title>
<style>
:root{color-scheme:light dark;--bg:#ffffff;--fg:#1f2328;--muted:#59636e;--line:#d1d9e0;--card:#f6f8fa;
--bad:#ffebe9;--badline:#ff8182;--good:#1a7f37;--bar:#cf222e;--mg:#cfe8ff;--mo:#ffcecb;--miss:#818b98}
@media (prefers-color-scheme: dark){:root{--bg:#0d1117;--fg:#e6edf3;--muted:#9198a1;--line:#3d444d;
--card:#151b23;--bad:#3c1618;--badline:#8e1519;--good:#3fb950;--bar:#f85149;--mg:#0c2d6b;--mo:#6e1a1f;--miss:#768390}}
*{box-sizing:border-box}
html{-webkit-text-size-adjust:100%}
body{margin:0;padding:24px 16px 48px;background:var(--bg);color:var(--fg);
font:15px/1.5 -apple-system,BlinkMacSystemFont,"Segoe UI",Roboto,"Helvetica Neue",Arial,sans-serif}
main{max-width:1200px;margin:0 auto}
h1{font-size:24px;line-height:1.25;margin:0 0 4px}
h2{font-size:18px;margin:32px 0 8px}
h3{font-size:16px;margin:0 0 4px;word-break:break-all}
.sub{color:var(--muted);margin:0 0 4px}
.stamp{color:var(--muted);font-size:13px;margin:0 0 16px}
.tiles{display:grid;grid-template-columns:repeat(auto-fit,minmax(220px,1fr));gap:12px;margin:16px 0}
.tile{background:var(--card);border:1px solid var(--line);border-radius:8px;padding:12px 16px}
.tile .k{color:var(--muted);font-size:13px}
.tile .v{font-size:30px;font-weight:600;font-variant-numeric:tabular-nums}
.tile .d{font-size:13px;color:var(--muted)}
ul.notes{padding-left:20px;margin:8px 0}
.scroll{overflow-x:auto;margin:8px 0 16px;max-width:100%}
table{border-collapse:collapse;font-size:13px;font-variant-numeric:tabular-nums}
th,td{border:1px solid var(--line);padding:4px 8px;text-align:left;vertical-align:top}
thead th{background:var(--card);white-space:nowrap}
td{white-space:pre-wrap}
td.num{text-align:right;white-space:nowrap}
.bar{width:120px;height:10px;background:var(--card);border:1px solid var(--line);border-radius:3px;overflow:hidden}
.bar span{display:block;height:100%;background:var(--bar)}
td.x{background:var(--bad);border-color:var(--badline)}
td.x .o{border-top:1px dashed var(--badline);margin-top:2px;padding-top:2px}
mark{color:inherit;border-radius:2px}
mark.g{background:var(--mg)}
mark.o{background:var(--mo)}
.null,.miss{color:var(--miss);font-style:italic}
td.ok{color:var(--good);white-space:nowrap}
td.bad{color:var(--bar);white-space:nowrap}
td.txt{min-width:260px;max-width:560px}
.page{border-top:1px solid var(--line);padding-top:16px;margin-top:16px}
.meta{color:var(--muted);font-size:13px;margin:0 0 8px}
.toggle{position:sticky;top:0;z-index:2;background:var(--bg);padding:8px 0;border-bottom:1px solid var(--line)}
.toggle label{cursor:pointer;font-weight:600}
body.only tr.same,body.only section.same{display:none}
footer{color:var(--muted);font-size:12px;margin-top:40px}
</style>
</head>
<body>
<main>
<h1>@@HEADING@@</h1>
<p class="sub">@@SUBTITLE@@</p>
<p class="stamp">@@STAMP@@</p>
<div class="tiles">@@TILES@@</div>
@@NOTES@@
<h2>By variable</h2>
<div class="scroll">@@VARIABLES@@</div>
<h2>Page by page</h2>
<div class="toggle"><label><input type="checkbox" id="only"> Show only differences</label>
<span class="meta">&middot; @@COUNTS@@</span></div>
@@PAGES@@
<h2>How the figures are made</h2>
@@METHOD@@
<footer>@@FOOTER@@</footer>
</main>
<script>
(function(){var c=document.getElementById("only");if(!c)return;
function u(){document.body.classList.toggle("only",c.checked);}
c.addEventListener("change",u);u();})();
</script>
</body>
</html>
'
