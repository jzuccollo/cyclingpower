# The app is a shell over the package's own functions. Nothing is computed
# here that cyclingpower does not already export, and the download renders
# the results already on screen rather than running the pipeline again.
#
# Shiny is in Suggests, so every call is qualified rather than attached.

REPLICATE_CHOICES <- c(200, 500, 1000, 2000)
BLOCK_CHOICES <- c(30, 60, 120)

#' Interpretation sits above the thing it explains rather than in a help tab,
#' because the whole point of the package is that these displays are easy to
#' read the wrong way.
#' @noRd
guidance <- function(...) shiny::div(class = "guidance", shiny::p(...))

#' One sentence of orientation stays on the page; the rest folds away. Prose
#' that is always open and never changes stops being read, and a caveat nobody
#' reads is not a caveat.
#' @noRd
details <- function(title, ...) {
  bslib::accordion(
    bslib::accordion_panel(title, ...),
    open = FALSE,
    class = "mb-3"
  )
}

#' Tables sit at the bottom, folded. The chart is what you look at; the table
#' is what you check afterwards.
#' @noRd
full_table <- function(...) details("The numbers in full", ...)

#' A heading inside the sidebar, rather than a nested accordion: 280px is too
#' narrow for one, and the controls are here to be visible, which a closed
#' accordion undoes.
#'
#' `affects` names the tab the block below governs, so a knob in the sidebar
#' says which screen it changes.
#' @noRd
sidebar_heading <- function(text, affects = NULL) {
  shiny::tagList(
    shiny::div(class = "sidebar-heading", text),
    if (!is.null(affects)) shiny::div(class = "sidebar-note", affects)
  )
}

#' Built on demand rather than at install time, so the theme is whatever
#' bslib does now and not whatever it did when the package was built.
#' @noRd
app_theme <- function() {
  bslib::bs_theme(
    version = 5,
    base_font = bslib::font_collection(
      "-apple-system",
      "BlinkMacSystemFont",
      "Segoe UI",
      "Helvetica Neue",
      "sans-serif"
    ),
    code_font = bslib::font_collection(
      "ui-monospace",
      "SFMono-Regular",
      "Menlo",
      "monospace"
    ),
    bg = "#fdfdfc",
    fg = "#1c1917",
    primary = "#3c5a6e",
    secondary = "#78716c",
    success = "#4d7c5f",
    warning = "#b45309",
    danger = "#9f1239",
    "font-size-base" = "0.95rem",
    "body-bg" = "#f5f4f2",
    "card-bg" = "#ffffff",
    "card-border-color" = "#e7e5e4",
    "card-border-radius" = "0.6rem",
    "card-cap-bg" = "#ffffff",
    "border-color" = "#e7e5e4",
    "table-border-color" = "#eceae8",
    "navbar-padding-y" = "0.6rem"
  ) |>
    # No double quotes anywhere in this block. It is a plain R string, so one
    # inside a CSS comment closes it, and the app then dies at source time with
    # a missing-comma error several hundred lines away from the cause.
    # parse(inst/shiny/app.R) catches it in a second.
    bslib::bs_add_rules(
      "
      /* page_navbar routes through page_fillable whenever it carries a sidebar,
         so fillable = FALSE alone still leaves the body a fixed-height flex
         container with the scroll hidden inside .main. On macOS, overlay
         scrollbars mean nothing on screen says there is more below, and card
         bodies open mid-sentence with their footnotes sliced through the middle
         of the glyphs. Give the window its scrollbar back. */
      body.bslib-page-fill {
        height: auto;
        min-height: 100vh;
        overflow: visible;
      }
      body.bslib-page-fill > .html-fill-item {
        flex: 0 0 auto;
        min-height: 0;
      }
      .bslib-sidebar-layout {
        height: auto;
        min-height: 0;
      }
      .bslib-sidebar-layout > .main {
        overflow-x: auto;
        overflow-y: visible;
      }
      /* The knobs stay reachable from anywhere on a long page, which is the
         whole argument for having consolidated them into one sidebar. */
      .bslib-sidebar-layout > .sidebar {
        align-self: start;
        position: sticky;
        top: 0;
        max-height: 100vh;
      }
      /* Cards keep their own scroll when expanded to full screen, but never
         when inline: an inline card that scrolls is the porthole again. */
      .bslib-card:not([data-full-screen=true]),
      .bslib-card:not([data-full-screen=true]) > .card-body {
        overflow: visible;
      }

      /* Prose is capped in characters, not pixels: the failure at a wide window
         is a 200-character line, which no percentage width fixes. */
      .guidance, .alert, .form-text, .help-block {
        max-width: 68ch;
        line-height: 1.6;
      }
      .guidance {
        color: #57534e;
        font-size: 0.9rem;
        margin-bottom: 1rem;
      }
      .guidance code { color: #3c5a6e; background: #f5f4f2; padding: 0 0.2em; }

      /* Card headers were rendering smaller than the form labels beside them,
         which left the sections with no presence at all. */
      .card-header {
        font-size: 1.05rem;
        font-weight: 600;
        letter-spacing: -0.01em;
        border-bottom: 1px solid #eceae8;
        padding-block: 0.75rem;
      }
      .card { box-shadow: 0 1px 2px rgba(28, 25, 23, 0.04); }

      .navbar { border-bottom: 1px solid #e7e5e4; }
      .navbar-brand { font-weight: 600; letter-spacing: -0.02em; }
      /* Nothing here forces the tab bar open. bslib 0.11 emits no
         navbar-expand class at all, so the links stay a wrapping row down to
         844px with no hamburger \u2014 the collapse this was meant to defeat is not
         in this version. navbar_options(collapsible = FALSE) is the trap: with
         no expand class to fall back on, Bootstrap stacks the five links
         vertically and costs 470px of page before any content. */

      .sidebar-heading {
        font-size: 0.78rem;
        font-weight: 700;
        letter-spacing: 0.06em;
        text-transform: uppercase;
        color: #78716c;
        margin-bottom: 0.35rem;
        padding-bottom: 0.3rem;
        border-bottom: 1px solid #e7e5e4;
      }
      .sidebar .form-label { margin-bottom: 0.15rem; }
      .sidebar-note {
        font-size: 0.82rem;
        line-height: 1.5;
        color: #78716c;
        margin-bottom: 0.75rem;
      }
      /* One width for every button in the sidebar. Three buttons at three
         content widths read as three levels of importance. */
      .sidebar .btn { width: 100%; }

      /* A row of actions. The buttons in it were previously sized by their own
         labels, so Scan came out half the width of Load rides and the row read
         as a hierarchy that was not intended. */
      .actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
        align-items: center;
      }
      .actions .btn { min-width: 11rem; }
      /* shinyDirButton and input_task_button both emit a bare button, so the
         form-group margin that lines up textInput has to be added by hand. */
      .actions-inline .btn { margin-bottom: 1rem; }

      /* Steps are numbered in the header, and the number carries the ordering
         rather than the prose having to. */
      .step-number {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        width: 1.5rem;
        height: 1.5rem;
        border-radius: 50%;
        background: #3c5a6e;
        color: #ffffff;
        font-size: 0.85rem;
        font-weight: 600;
        margin-right: 0.5rem;
      }

      /* Results tables: the numbers are the point, so give them a monospace
         figure and stop the header shouting over them. */
      .table { font-variant-numeric: tabular-nums; margin-bottom: 0; }
      /* Sentence case, not small caps: a column headed Difference (W) is read,
         one headed DIFFERENCE_W is decoded. */
      .table thead th {
        font-size: 0.86rem;
        font-weight: 600;
        color: #44403c;
        border-bottom: 1px solid #d6d3d1;
        vertical-align: bottom;
      }
      .table td { padding-block: 0.5rem; }
      .table tbody tr:hover { background: #faf9f8; }

      /* Six short columns were being stretched across 1,355px of monitor, so
         reading a pair against its number meant crossing most of the screen.
         Scoped to renderTable's output; DT manages its own width. */
      table.shiny-table { width: auto; }
      table.shiny-table td { white-space: nowrap; }
      table.shiny-table th, table.shiny-table td { padding-inline: 0.7rem; }
      table.shiny-table th:first-child, table.shiny-table td:first-child {
        padding-left: 0;
      }

      /* The protocol is the exception those rules are written against: its last
         column is a sentence, not a number, and nowrap pushes it off the card. */
      #protocol_table table.shiny-table { width: 100%; }
      #protocol_table table.shiny-table td { white-space: normal; }
      #protocol_table table.shiny-table tbody tr:last-child {
        font-style: italic;
        color: #57534e;
      }

      /* An estimate and its interval share a cell, and the strings are padded to
         a fixed width so the estimates form a column you can read straight down.
         Right-aligning the composite instead lines up the closing bracket, which
         is the one character nobody is scanning for. */
      td.ci, th.ci { white-space: pre; text-align: left; }

      /* A chained row rests on an assumption the data cannot test, and unmarked
         it reads exactly as authoritative as the rows above it. */
      tr.chained td { background: #fdf8ef; }
      .badge-chained {
        background: #f3e6cd;
        color: #7c4a02;
        font-size: 0.72rem;
        font-weight: 600;
        padding: 0.1rem 0.4rem;
        border-radius: 0.25rem;
      }

      /* The link icon marking a chained pair is a mark, not an illustration:
         left at its default it rendered larger than the number beside it. */
      .bslib-value-box .value-box-showcase svg {
        max-height: 1.6rem;
        width: auto;
        opacity: 0.55;
      }

      /* DT floats its row count, so anything rendered after the table lands on
         top of it. */
      .dataTables_wrapper::after { content: ''; display: block; clear: both; }
      table.dataTable { font-variant-numeric: tabular-nums; }
      table.dataTable td, table.dataTable th { white-space: nowrap; }
      .dataTables_wrapper { overflow-x: auto; }

      /* Alerts read as notes rather than as warnings shouted at you. */
      .alert { border: 1px solid; border-left-width: 3px; }

      /* Recording. The chips and the canvas are painted by record.js rather than
         rendered by R, so their styling lives here and not in an inline style
         attribute the JavaScript would have to carry. */
      #meter_status {
        display: flex;
        flex-wrap: wrap;
        gap: 0.5rem;
        margin-bottom: 0.75rem;
        min-height: 2rem;
      }
      .meter-chip {
        display: flex;
        align-items: baseline;
        gap: 0.5rem;
        border: 1px solid #e7e5e4;
        border-radius: 0.4rem;
        background: #ffffff;
        padding: 0.35rem 0.6rem;
        font-size: 0.9rem;
      }
      .meter-dot {
        width: 0.6rem;
        height: 0.6rem;
        border-radius: 50%;
        align-self: center;
      }
      .meter-name { font-weight: 600; }
      .meter-watts {
        font-variant-numeric: tabular-nums;
        min-width: 4.5rem;
        text-align: right;
      }
      .meter-state {
        font-size: 0.78rem;
        text-transform: uppercase;
        letter-spacing: 0.04em;
        color: #78716c;
      }
      /* A meter that has gone has to be legible from across a room, mid-effort. */
      .meter-reconnecting, .meter-lost, .meter-failed {
        border-color: #b45309;
        background: #fdf5e6;
      }
      .meter-reconnecting .meter-state,
      .meter-lost .meter-state,
      .meter-failed .meter-state { color: #b45309; font-weight: 600; }

      #record_trace {
        width: 100%;
        height: 240px;
        display: block;
        background: #ffffff;
        border: 1px solid #e7e5e4;
        border-radius: 0.4rem;
      }
      "
    )
}
