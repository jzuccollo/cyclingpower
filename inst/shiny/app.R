# The app is a shell over the package's own functions. Nothing is computed here
# that cyclingpower does not already export, and the download renders the
# results already on screen rather than running the pipeline a second time.

library(shiny)
library(cyclingpower)

REPLICATE_CHOICES <- c(200, 500, 1000, 2000)
BLOCK_CHOICES <- c(30, 60, 120)

#' Interpretation sits above the thing it explains rather than in a help tab,
#' because the whole point of the package is that these displays are easy to
#' read the wrong way.
guidance <- function(...) div(class = "guidance", p(...))

#' One sentence of orientation stays on the page; the rest folds away. Prose
#' that is always open and never changes stops being read, and a caveat nobody
#' reads is not a caveat.
details <- function(title, ...) {
  bslib::accordion(
    bslib::accordion_panel(title, ...),
    open = FALSE,
    class = "mb-3"
  )
}

#' Tables sit at the bottom, folded. The chart is what you look at; the table
#' is what you check afterwards.
full_table <- function(...) details("The numbers in full", ...)

#' A heading inside the sidebar. Nested accordions were the alternative and
#' 280px is too narrow for them — and the reason these controls were moved in
#' from the tabs was to make them visible, which a closed accordion undoes.
sidebar_heading <- function(text) {
  div(class = "sidebar-heading", text)
}

#' The interface is deliberately quiet. Every saturated colour in this app is
#' spent on data — the Okabe-Ito palette in source_palette() — so the chrome is
#' a warm grey scale with a single slate accent that cannot be mistaken for a
#' series. A blue navbar and blue buttons would compete with the blue points.
app_theme <- bslib::bs_theme(
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
       844px with no hamburger — the collapse this was meant to defeat is not
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
    "
  )

ui <- bslib::page_navbar(
  title = "cyclingpower",
  id = "nav",
  theme = app_theme,

  # bslib fills the viewport by default, which turns every card into a fixed
  # porthole with a hidden inner scroll — and, because plotOutput(height =
  # "auto") then collapses to nothing while renderPlot sizes the image, lets
  # the by-level table paint straight over its own plot. Let the page scroll.
  fillable = FALSE,

  # No explicit progress bar: none of the package functions report progress, so
  # a bar would jump 0 to 100 with a lie in between. Shiny dims recalculating
  # outputs on its own, which says the same thing honestly.

  # One set of knobs for the whole app, including the four that used to sit on
  # the tabs they affected. Three of those reach the downloaded report, which
  # outlives the session; a control that decides what a saved document says but
  # lives on a tab its reader never opened is the failure this app exists to
  # prevent. max_offset was worse still, since dragging it silently invalidated
  # every result on every other tab.
  sidebar = bslib::sidebar(
    width = 280,

    sidebar_heading("Comparison"),
    sliderInput("min_power", "Minimum power (W)", 0, 400, 0, step = 5),
    helpText(
      "Seconds where either meter reads below this are dropped. Coasting is ",
      "always dropped: two meters both reading zero agree trivially."
    ),
    selectInput("block", "Bootstrap block (s)", BLOCK_CHOICES, selected = 60),
    selectInput(
      "replicates",
      "Bootstrap replicates",
      REPLICATE_CHOICES,
      selected = 500
    ),
    numericInput("seed", "Seed", value = 1, min = 1, step = 1),
    actionButton("reseed", "Reseed", class = "btn-sm btn-outline-secondary"),
    helpText(
      "Intervals are resampled, so they move a little each time. If a ",
      "conclusion changes when you reseed, there are too few replicates ",
      "behind it."
    ),

    hr(),
    sidebar_heading("Alignment"),
    sliderInput("max_offset", "Largest offset to consider (s)", 0, 120, 30, 5),
    sliderInput("smooth", "Trace smoothing (s)", 1, 120, 30, 1),

    hr(),
    sidebar_heading("Power bands"),
    sliderInput("width", "Band width (W)", 10, 100, 25, 5),
    sliderInput("min_seconds", "Minimum seconds per band", 0, 300, 30, 10)
  ),

  bslib::nav_panel(
    "Rides",
    bslib::card(
      bslib::card_header("Find the files"),
      guidance(
        "The unit of analysis is a run: one session, ridden once, recorded by ",
        "two or more meters. Files named ",
        tags$code("<run><sep><source>"),
        " fill the table in automatically — ",
        tags$code("run1_kickr.fit"),
        " is the kickr's recording of run one. Anything else you label by ",
        "hand: the run and source cells are editable."
      ),
      # Where the files are on one line, what to do about them on the next.
      # Mixing inputs and buttons in a single wrapping row put the buttons on
      # a baseline of their own and orphaned the last one.
      div(
        class = "d-flex gap-2 align-items-end flex-wrap",
        div(
          class = "flex-grow-1",
          style = "min-width: 240px;",
          textInput("dir", "Directory", width = "100%")
        ),
        div(
          class = "mb-3",
          shinyFiles::shinyDirButton(
            "dir_choose",
            "Browse…",
            "Choose a directory of ride files"
          )
        ),
        textInput("sep", "Separator", value = "_", width = "90px")
      ),
      div(
        class = "d-flex gap-2 align-items-center flex-wrap mb-3",
        actionButton("scan", "Scan", class = "btn-outline-secondary mb-0"),
        bslib::input_task_button(
          "load",
          "Load rides",
          label_busy = "Reading files…"
        ),
        span(class = "flex-grow-1"),
        bslib::input_task_button(
          "demo",
          "Load demo rides",
          label_busy = "Building demo…",
          type = "outline-secondary"
        )
      ),
      helpText(
        "Scanning again replaces the table, discarding any edits. Scan and ",
        "Load are separate so you can correct the labels in between, which is ",
        "what the editable table is for."
      ),
      uiOutput("scan_status"),
      uiOutput("load_status")
    ),
    bslib::card(
      bslib::card_header("Runs and sources"),
      guidance(
        "Two files must never carry the same run and source. They would be ",
        "averaged into a single meter with nothing in the output to say it ",
        "had happened, so that combination is refused rather than warned ",
        "about."
      ),
      uiOutput("demo_note"),
      uiOutput("spec_issues"),
      p(
        class = "small text-body-secondary mb-2",
        "Double-click a run or source cell to edit it. Select rows and use ",
        "Remove to leave a file out of the comparison entirely."
      ),
      DT::DTOutput("spec"),
      div(
        class = "mt-2",
        actionButton(
          "remove_rows",
          "Remove selected",
          class = "btn-sm btn-outline-secondary"
        )
      )
    )
  ),

  bslib::nav_panel(
    "Alignment",
    uiOutput("align_headline"),
    uiOutput("align_alert"),
    uiOutput("edge_alert"),
    bslib::card(
      bslib::card_header("Power traces"),
      full_screen = TRUE,
      guidance(
        "The visual check that these are recordings of the same ride: traces ",
        "should track each other's shape, whatever vertical gap sits between ",
        "them."
      ),
      plotOutput("timeseries", height = "auto")
    ),
    full_table(tableOutput("align_table"), uiOutput("align_provenance")),
    details(
      "How the offsets were found",
      p(
        "Two meters recording the same ride do not necessarily agree about ",
        "what time it is, and differencing on nominal timestamps then ",
        "compares power from one moment against power from another. That ",
        "inflates the spread and shifts the bias, and the result looks ",
        "entirely plausible on the way out. Each source is matched against a ",
        "reference by cross-correlating the power traces."
      ),
      p(
        tags$code("offset"),
        " is the correction applied, in seconds. ",
        tags$code("correlation"),
        " is how well the traces matched there, and ",
        tags$code("sharpness"),
        " how far that peak stood above the rest. ",
        tags$code("status"),
        " is ",
        tags$code("aligned"),
        " when the offset ",
        "was established, ",
        tags$code("assumed"),
        " for the reference ",
        "itself, and ",
        tags$code("unresolved"),
        " when the data could not ",
        "settle it — in which case the recorded timestamps stand."
      )
    )
  ),

  bslib::nav_panel(
    "Agreement",
    uiOutput("headline"),
    uiOutput("chain_notes"),
    # Both full width, stacked. Measured, the table wants about 910px for its
    # six columns and the faceted plot wants at least as much again, so
    # splitting a 900px pane between them gave a table scrolling sideways
    # behind its own card edge and a plot too small to read a tilt off. The
    # table's own width: auto is what stops it stretching across the monitor,
    # which was the actual complaint.
    bslib::card(
      bslib::card_header("Every pair"),
      uiOutput("pair_table"),
      uiOutput("provenance")
    ),
    bslib::card(
      bslib::card_header("Bland-Altman"),
      full_screen = TRUE,
      guidance(
        "Each cell counts the seconds spent there: the two meters' mean ",
        "across, their difference up. Flat above or below zero is a ",
        "constant offset; a tilt means the gap changes with effort. Only ",
        "pairs that rode together appear here, because a chained ",
        "comparison has no shared seconds to plot."
      ),
      uiOutput("bridge_ui"),
      plotOutput("agreement_plot", height = "auto")
    ),
    details(
      "How to read these numbers",
      p(
        "Gain is the difference as a percentage, which is usually the more ",
        "natural way to read a meter, since meters tend to be a few per cent ",
        "out rather than a fixed number of watts out. The limits of ",
        "agreement bound the range within which most individual seconds ",
        "fall — they describe the scatter, not the uncertainty in the ",
        "average, which is what the interval beside each estimate gives."
      ),
      p(
        tags$b("Where each number came from. "),
        "Most pairs rode together, and their row names the run. A pair that ",
        "never rode together is linked through a bridge meter present in ",
        "both runs: each run gives a gain relative to the bridge, and the ",
        "difference between those gains is the comparison. Such a row is ",
        "marked ",
        tags$em("chained"),
        " and rests on the bridge having read ",
        "the same on both days, which nothing in the ride files can test. It ",
        "has no watts and no limits of agreement, because there is no second ",
        "the two meters both recorded."
      ),
      p(
        tags$b("Why the intervals are wide. "),
        "They come from a moving-block bootstrap rather than from treating ",
        "each second as independent evidence. What you are riding now is ",
        "nearly what you were riding a second ago, so an hour carries far ",
        "less independent information than its 3,600 rows suggest, and an ",
        "interval built on independence would be roughly an order of ",
        "magnitude too narrow."
      ),
      p(
        "A cloud fanning out to the right on the Bland-Altman plot is ",
        "ordinary — a fixed percentage error is more watts at more watts."
      )
    )
  ),

  bslib::nav_panel(
    "By level",
    uiOutput("level_headline"),
    uiOutput("level_alert"),
    uiOutput("thin_alert"),
    guidance(
      "A single average hides the pattern that usually matters: meters that ",
      "agree at endurance pace and drift at threshold. A meter reading high ",
      "in one half of the range and low in the other can post an overall ",
      "bias near zero and still be wrong everywhere."
    ),
    # Two views of one computation rather than a toggle: a constant percentage
    # error is a rising line in watts and a flat line in per cent, and being
    # able to put the two side by side is the point of showing both.
    bslib::navset_card_tab(
      full_screen = TRUE,
      title = "Across the power range",
      bslib::nav_panel(
        "In watts",
        plotOutput("by_level_plot", height = "auto")
      ),
      bslib::nav_panel(
        "In per cent",
        plotOutput("by_level_pc_plot", height = "auto")
      ),
      bslib::nav_panel(
        "The numbers",
        DT::DTOutput("by_level_table"),
        uiOutput("by_level_note")
      )
    ),
    details(
      "How to read this",
      p(
        "Seconds are binned by the ",
        tags$i("mean"),
        " of the two meters, ",
        "not by one nominated meter: binning on one and plotting its own ",
        "difference against that bin turns measurement noise into an ",
        "apparent slope."
      ),
      p(
        "A meter that is a constant percentage out reads further from its ",
        "partner the harder you ride, so its line rises in watts while ",
        "nothing about the meter has changed. The per-cent panel is flat for ",
        "that meter and tilts only for one whose error genuinely depends on ",
        "how hard you were riding."
      )
    )
  ),

  bslib::nav_panel(
    "Report",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header("Save what is on screen"),
        guidance(
          "Renders the results shown in the other tabs to a standalone HTML ",
          "file, with the same explanation alongside them. It presents the ",
          "same objects rather than recomputing, so the document cannot ",
          "disagree with the screen, and it saves in about the time the ",
          "render takes rather than repeating the bootstrap."
        ),
        uiOutput("report_ui")
      ),
      bslib::card(
        bslib::card_header("What goes into it"),
        uiOutput("report_summary")
      )
    )
  )
)

server <- function(input, output, session) {
  volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(
    input,
    "dir_choose",
    roots = volumes,
    session = session
  )

  opening_dir <- getShinyOption("cyclingpower_dir")
  if (!is.null(opening_dir)) {
    updateTextInput(session, "dir", value = opening_dir)
  }

  # ---- Rides -------------------------------------------------------------

  # The directory to scan, rather than input$dir directly: updateTextInput is
  # asynchronous, so a button that sets the box and scans in one go would
  # otherwise scan the previous value.
  scan_dir <- reactiveVal(NULL)

  # Rendered by the table; changes only on scan or demo. Edits go to spec_rv,
  # so typing in a cell does not re-render the table under the cursor.
  table_spec <- reactiveVal(NULL)
  spec_rv <- reactiveVal(NULL)
  load_trigger <- reactiveVal(0L)
  demo_note <- reactiveVal(NULL)

  set_spec <- function(spec) {
    table_spec(spec)
    spec_rv(spec)
  }

  # ride_spec_from_names() is all or nothing, so one unparseable name leaves
  # every row for the user to fill in rather than half a table.
  build_spec <- function(paths, sep) {
    tryCatch(
      ride_spec_from_names(paths, sep = sep),
      error = function(e) tibble::tibble(path = paths, run = "", source = "")
    )
  }

  ride_files <- function(dir) {
    list.files(
      dir,
      pattern = "\\.(fit|gpx|csv)$",
      ignore.case = TRUE,
      full.names = TRUE
    )
  }

  # Counted rather than storing the path alone: a reactiveVal set to the value
  # it already holds does not invalidate, so scanning the same directory twice
  # — which is exactly what you do after correcting the separator — would be a
  # silent no-op.
  scan_trigger <- reactiveVal(0L)
  rescan <- function(dir) {
    scan_dir(dir)
    demo_note(NULL)
    scan_trigger(isolate(scan_trigger()) + 1L)
  }

  observeEvent(input$scan, rescan(input$dir))

  observeEvent(input$dir_choose, {
    chosen <- shinyFiles::parseDirPath(volumes, input$dir_choose)
    if (length(chosen) > 0) {
      updateTextInput(session, "dir", value = chosen)
      rescan(as.character(chosen))
    }
  })

  # An alert above the table it describes, not a toast. A notification that
  # explains what the three meters do has to be read in fifteen seconds and
  # then cannot be got back.
  observeEvent(input$demo, {
    dir <- demo_rides()
    updateTextInput(session, "dir", value = dir)
    set_spec(build_spec(ride_files(dir), "_"))
    demo_note(paste(
      "Demo rides. assioma is the reference and rides in both sessions;",
      "kickr reads about 2.5% below it with a 7-second clock offset; quarq",
      "reads low below 200 W and high above it. kickr and quarq never ride",
      "together, so their comparison has to be chained through assioma."
    ))
    load_trigger(isolate(load_trigger()) + 1L)
  })

  output$demo_note <- renderUI({
    req(demo_note())
    div(class = "alert alert-info", demo_note())
  })

  # Returns the problem rather than validate()ing it: the only consumer is an
  # observer, and an observer swallows a validation error, so the message would
  # never reach the screen.
  files <- eventReactive(scan_trigger(), {
    dir <- scan_dir()
    req(dir)

    if (!dir.exists(dir)) {
      return(list(error = paste0("No directory at ", dir, ".")))
    }
    found <- ride_files(dir)
    if (length(found) == 0) {
      return(list(error = "No .fit, .gpx or .csv files in that directory."))
    }
    list(paths = found)
  })

  observeEvent(list(files(), input$sep), {
    req(files()$paths)
    set_spec(build_spec(files()$paths, input$sep))
  })

  output$scan_status <- renderUI({
    req(files()$error)
    div(class = "alert alert-danger", files()$error)
  })

  observeEvent(input$spec_cell_edit, {
    edited <- DT::editData(spec_rv(), input$spec_cell_edit, rownames = FALSE)
    edited$run <- trimws(edited$run)
    edited$source <- trimws(edited$source)
    spec_rv(edited)
  })

  # The missing capability: without it a stray .csv in the directory could only
  # be dealt with by moving the file.
  observeEvent(input$remove_rows, {
    req(input$spec_rows_selected)
    set_spec(spec_rv()[-input$spec_rows_selected, ])
  })

  output$spec <- DT::renderDT(
    {
      req(table_spec())
    },
    rownames = FALSE,
    # Selection and editing coexist: a click selects, a double-click edits. DT
    # has no single-click editing to configure, so the fix for that is the
    # hint above the table rather than an option here.
    editable = list(target = "cell", disable = list(columns = 0)),
    selection = "multiple",
    escape = -1,
    options = list(
      paging = FALSE,
      dom = "t",
      # Sorting would reorder what is on screen without reordering spec_rv(),
      # and Remove would then delete a row the user had not selected.
      ordering = FALSE,
      columnDefs = list(
        # A 90-character temp path took the whole width and pushed run and
        # source — the two editable columns, and the only ones that matter —
        # off the right-hand edge.
        list(
          targets = 0,
          render = DT::JS(
            "function(data, type, row) {",
            "  if (type !== 'display') return data;",
            "  var s = String(data);",
            "  var short = s.split('/').pop();",
            "  return '<span title=' + JSON.stringify(s) + '>' + short + '</span>';",
            "}"
          )
        )
      )
    )
  )

  spec_checks <- reactive({
    spec <- spec_rv()
    req(spec)

    blocking <- character()
    if (nrow(spec) == 0) {
      blocking <- c(blocking, "No files left to compare.")
    }
    if (any(!nzchar(spec$run) | !nzchar(spec$source))) {
      blocking <- c(blocking, "Every row needs a run and a source.")
    }
    if (any(duplicated(spec[, c("run", "source")]))) {
      blocking <- c(
        blocking,
        paste(
          "Two files claim the same run and source. They would be averaged",
          "into a single meter with nothing to say it had happened, so give",
          "them different labels or drop one."
        )
      )
    }
    if (any(!file.exists(spec$path))) {
      blocking <- c(blocking, "Some of these files no longer exist.")
    }

    warnings <- character()
    if (
      any(grepl("\\.fit$", spec$path, ignore.case = TRUE)) &&
        !requireNamespace("FITfileR", quietly = TRUE)
    ) {
      warnings <- c(
        warnings,
        paste(
          "Reading FIT files needs FITfileR, which is not installed.",
          'remotes::install_github("grimbough/FITfileR")'
        )
      )
    }
    if (nrow(spec) > 0 && all(nzchar(spec$run)) && all(nzchar(spec$source))) {
      per_run <- table(unique(spec[, c("run", "source")])$run)
      thin <- names(per_run)[per_run < 2]
      if (length(thin) > 0) {
        warnings <- c(
          warnings,
          paste0(
            "Run(s) ",
            paste(thin, collapse = ", "),
            " have only one source. They will be aligned and plotted but ",
            "contribute to no comparison."
          )
        )
      }
    }

    list(blocking = blocking, warnings = warnings)
  })

  output$spec_issues <- renderUI({
    checks <- spec_checks()
    tagList(
      lapply(checks$blocking, function(m) div(class = "alert alert-danger", m)),
      lapply(checks$warnings, function(m) div(class = "alert alert-warning", m))
    )
  })

  observeEvent(input$load, load_trigger(isolate(load_trigger()) + 1L))

  rides <- eventReactive(load_trigger(), {
    req(load_trigger() > 0)
    checks <- spec_checks()
    validate(need(
      length(checks$blocking) == 0,
      paste(checks$blocking, collapse = " ")
    ))
    read_rides(spec_rv())
  })

  data_version <- reactiveVal(0L)
  observeEvent(rides(), data_version(isolate(data_version()) + 1L))

  # The link the task button's spinner needs: it reverts when the reactive
  # cycle started by the click completes, and that cycle only contains the file
  # read if something on the visible tab actually depends on rides().
  output$load_status <- renderUI({
    req(load_trigger() > 0)
    loaded <- rides()
    div(
      class = "alert alert-success py-2 mb-0",
      sprintf(
        "Loaded %s files: %s sources across %s runs, %s seconds of riding. ",
        nrow(spec_rv()),
        dplyr::n_distinct(loaded$source),
        dplyr::n_distinct(loaded$run),
        format(nrow(loaded), big.mark = ",")
      ),
      # A link, not an automatic jump. Advancing would take the user off the
      # only tab carrying the spec table and the blocking alerts at exactly
      # the moment they most need to check that the labels parsed.
      actionLink("to_alignment", "Check the clock offsets")
    )
  })

  observeEvent(input$to_alignment, bslib::nav_select("nav", "Alignment"))

  # ---- Settings ----------------------------------------------------------

  # Clearing the box leaves the input NA, which with_seed rejects, so every
  # panel in the app would go down until something was typed back in.
  current_seed <- reactive(if (is.na(input$seed)) 1L else input$seed)

  # Bundled and debounced together, so a drag settles into one recompute rather
  # than a cascade, and events queued during a slow bootstrap collapse into one.
  # max_offset belongs in here now that it sits in the sidebar where anyone can
  # drag it: it is part of align_key(), so every intermediate value would
  # otherwise invalidate every cached result in the app.
  settings <- debounce(
    reactive(list(
      min_power = input$min_power,
      block = as.numeric(input$block),
      replicates = as.numeric(input$replicates),
      seed = current_seed(),
      max_offset = input$max_offset,
      smooth = input$smooth
    )),
    700
  )

  level_opts <- debounce(
    reactive(list(width = input$width, min_seconds = input$min_seconds)),
    700
  )

  # The settings sit in a sidebar and the numbers render per tab, so nothing on
  # screen otherwise says what produced them. One component, rendered under
  # each set of numbers it governs, which is what consolidating the knobs into
  # the sidebar costs and has to repay.
  provenance <- function(...) {
    s <- settings()
    div(
      class = "small text-body-secondary border-top pt-2 mt-2",
      sprintf(
        paste(
          "Intervals are 95%% from %s moving-block bootstrap replicates over",
          "%s-second blocks, seed %s. Seconds below %s W dropped, and",
          "coasting always."
        ),
        s$replicates,
        s$block,
        s$seed,
        s$min_power
      ),
      ...
    )
  }

  # ---- Align -------------------------------------------------------------

  alignment <- reactive({
    messages <- character()
    data <- withCallingHandlers(
      align_rides(rides(), max_offset = settings()$max_offset),
      warning = function(w) {
        messages <<- c(messages, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    )
    list(data = data, warnings = messages)
  })

  aligned <- reactive(alignment()$data)
  align_report <- reactive(alignment_report(aligned()))

  # What identifies the aligned data for caching. Everything expensive is keyed
  # on this rather than on the tibble itself, which would be hashed on every
  # read.
  align_key <- reactive(list(data_version(), settings()$max_offset))

  # status first: it is what decides whether the rest of the row means
  # anything, and it was the last column and off screen below 900px. offset is
  # integer by construction, so three decimals were three lies.
  output$align_table <- renderTable(
    {
      align_report() |>
        dplyr::transmute(
          Run = run,
          Source = source,
          Status = status,
          `Offset (s)` = sprintf("%+d", as.integer(round(offset))),
          Correlation = sprintf("%.3f", correlation),
          Sharpness = sprintf("%.2f", sharpness)
        )
    },
    align = "lllrrr"
  )

  output$align_provenance <- renderUI({
    provenance(sprintf(
      " Clock offsets were sought up to %s s either way.",
      settings()$max_offset
    ))
  })

  # One box per source: what the clock correction was, and whether it was
  # established or fallen back on.
  output$align_headline <- renderUI({
    report <- align_report()
    boxes <- lapply(seq_len(nrow(report)), function(i) {
      r <- report[i, ]
      bslib::value_box(
        title = paste(r$source, "—", r$run),
        value = if (r$status == "unresolved") {
          "not found"
        } else {
          sprintf("%+d s", as.integer(round(r$offset)))
        },
        p(switch(
          r$status,
          aligned = "Clock offset established by cross-correlation.",
          assumed = "The reference for this run; everything else is matched to it.",
          "No offset could be established, so the recorded timestamps stand."
        )),
        theme = bslib::value_box_theme(
          bg = if (r$status == "unresolved") "#fdf5e6" else "#ffffff",
          fg = "#1c1917"
        )
      )
    })
    do.call(
      bslib::layout_column_wrap,
      c(list(width = "260px", class = "mb-3"), boxes)
    )
  })

  # Driven by the status column rather than the captured warning: one warning
  # is emitted per run, and an alert built from the same column it sits above
  # cannot drift from it.
  output$align_alert <- renderUI({
    report <- align_report()
    unresolved <- report$source[report$status == "unresolved"]
    if (length(unresolved) == 0) {
      return(NULL)
    }
    div(
      class = "alert alert-warning",
      strong("Alignment incomplete. "),
      paste0(
        "No clock offset could be established for ",
        paste(unresolved, collapse = ", "),
        ", so the recorded timestamps stand. Expected for a steady effort ",
        "with no features to lock onto; a low correlation instead suggests ",
        "these are not recordings of the same ride."
      )
    )
  })

  # A best offset sitting exactly on the edge of the search window is a
  # boundary being reported as an answer: the true offset may lie outside it
  # and nothing in the correlation can say so.
  output$edge_alert <- renderUI({
    report <- align_report()
    limit <- settings()$max_offset
    at_edge <- report$status == "aligned" & abs(report$offset) >= limit
    if (!any(at_edge)) {
      return(NULL)
    }
    div(
      class = "alert alert-warning",
      strong("An offset is at the edge of the search. "),
      sprintf(
        paste(
          "%s matched best at %s s, the largest offset considered. The true",
          "offset may be larger still, so widen the search before trusting",
          "this one."
        ),
        paste(report$source[at_edge], collapse = ", "),
        limit
      )
    )
  })

  n_runs <- reactive(dplyr::n_distinct(aligned()$run))

  output$timeseries <- renderPlot(
    plot_timeseries(aligned(), smooth = settings()$smooth),
    height = function() max(320, 240 * n_runs())
  )

  # ---- Agreement ---------------------------------------------------------

  agreement <- bindCache(
    reactive({
      s <- settings()
      withr::with_seed(
        s$seed,
        power_agreement(aligned(), s$min_power, s$block, s$replicates)
      )
    }),
    align_key(),
    settings()
  )

  # An estimate and its interval belong in one cell. Three columns each turns
  # the table into thirteen columns of numbers with the intervals too far from
  # their point estimates to read together, which is the one comparison the
  # table exists to support.
  #
  # The estimate is padded to a fixed width and the cell set in pre, so the
  # estimates line up down the column. Right-aligning the whole string instead
  # aligns the closing bracket and leaves +2.58, -0.61 and -3.11 starting at
  # three different places. Bounds are signed like the estimate: signing one
  # and not the other reads as a difference in kind.
  with_ci <- function(point, lo, hi, digits) {
    sprintf(
      paste0(
        "%+",
        digits + 5,
        ".",
        digits,
        "f  (%+.",
        digits,
        "f to %+.",
        digits,
        "f)"
      ),
      point,
      lo,
      hi
    )
  }

  # Raw numbers, one row per pair, direct and chained together. The formatting
  # happens in pair_table(); the value boxes read this, so the headline and the
  # table cannot disagree about what the pairs are.
  pair_rows <- reactive({
    direct <- agreement() |>
      dplyr::transmute(
        source_a,
        source_b,
        from = run,
        basis = "direct",
        gain_pc,
        gain_lo,
        gain_hi,
        detail = sprintf("From %s, %s s.", run, format(seconds, big.mark = ","))
      )

    linked <- chained_rows()
    if (is.null(linked)) {
      return(direct)
    }

    dplyr::bind_rows(
      direct,
      dplyr::transmute(
        linked,
        source_a,
        source_b,
        from = paste("chained via", via),
        basis = "chained",
        gain_pc,
        gain_lo,
        gain_hi,
        detail = paste("Chained via", via, "— no shared seconds.")
      )
    )
  })

  # The answer, stated in a sentence, before any table. Phrased strictly in the
  # estimate's own orientation: +2.58 is source_a above source_b, and the
  # reciprocal (source_b below source_a) is a different number.
  output$headline <- renderUI({
    rows <- pair_rows()
    boxes <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      bslib::value_box(
        title = paste(r$source_a, "vs", r$source_b),
        value = sprintf("%+.2f%%", r$gain_pc),
        showcase = if (r$basis == "chained") icon("link"),
        showcase_layout = "left center",
        # Always in source_a's orientation. Turning it round — "kickr reads
        # 2.58% below assioma" — is the reciprocal, which is a different
        # number (2.51%), and it would disagree with the table below.
        p(sprintf(
          "%s reads %.2f%% %s than %s.",
          r$source_a,
          abs(r$gain_pc),
          if (r$gain_pc >= 0) "higher" else "lower",
          r$source_b
        )),
        p(
          class = "small mb-0",
          sprintf("95%% interval %+.2f to %+.2f. ", r$gain_lo, r$gain_hi),
          r$detail
        ),
        p(class = "small mb-0", previous_note(r)),
        theme = bslib::value_box_theme(bg = "#ffffff", fg = "#1c1917")
      )
    })
    do.call(
      bslib::layout_column_wrap,
      c(list(width = "280px", fixed_width = FALSE, class = "mb-3"), boxes)
    )
  })

  # Every pair the data can speak to, direct and chained in one table. The two
  # differ in where the number came from, which is a column, not a tab.
  pair_table <- reactive({
    direct <- agreement() |>
      dplyr::transmute(
        Pair = paste(source_a, "vs", source_b),
        From = run,
        `Gain %` = with_ci(gain_pc, gain_lo, gain_hi, 2),
        `Difference (W)` = with_ci(bias_w, bias_lo, bias_hi, 1),
        # Signed like every other number in the table. Signing the estimate
        # and not the bounds reads as a difference in kind rather than a
        # difference in formatting.
        `Limits of agreement (W)` = sprintf(
          "%+.1f to %+.1f",
          loa_lower,
          loa_upper
        ),
        Seconds = format(seconds, big.mark = ","),
        basis = "direct"
      )

    linked <- chained_rows()
    if (is.null(linked)) {
      return(direct)
    }

    dplyr::bind_rows(
      direct,
      dplyr::transmute(
        linked,
        Pair = paste(source_a, "vs", source_b),
        From = paste("chained via", via),
        `Gain %` = with_ci(gain_pc, gain_lo, gain_hi, 2),
        `Difference (W)` = "—",
        `Limits of agreement (W)` = "—",
        Seconds = "—",
        basis = "chained"
      )
    )
  })

  # Built by hand rather than through renderTable so a chained row can carry a
  # class. Unmarked, -3.11 (-4.22 to -2.02) looks exactly as authoritative as
  # the direct rows above it, and the three em-dashes read as missing data
  # rather than as a comparison that has no watts to report.
  output$pair_table <- renderUI({
    rows <- pair_table()
    numeric_cols <- c("Gain %", "Difference (W)")

    header <- tags$tr(lapply(
      setdiff(names(rows), "basis"),
      function(nm) {
        tags$th(class = if (nm %in% numeric_cols) "ci", nm)
      }
    ))

    body <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      tags$tr(
        class = if (r$basis == "chained") "chained",
        lapply(setdiff(names(rows), "basis"), function(nm) {
          tags$td(
            class = if (nm %in% numeric_cols) "ci",
            if (nm == "From" && r$basis == "chained") {
              tagList(span(class = "badge-chained", "chained"), " ", r[[nm]])
            } else {
              r[[nm]]
            }
          )
        })
      )
    })

    tagList(
      div(
        style = "overflow-x: auto;",
        tags$table(
          class = "table shiny-table",
          tags$thead(header),
          tags$tbody(body)
        )
      ),
      if (any(rows$basis == "chained")) {
        p(
          class = "small text-body-secondary mt-2 mb-0",
          "— no second that both meters recorded, so there is no difference ",
          "in watts and no limits of agreement to report."
        )
      }
    )
  })

  output$provenance <- renderUI(provenance())

  # Panels, not runs: a pair generally rides in one run, so what multiplies the
  # facets is the number of pairs actually ridden together.
  n_panels <- reactive({
    nrow(unique(agreement()[, c("run", "source_a", "source_b")]))
  })

  output$agreement_plot <- renderPlot(
    plot_agreement(aligned(), min_power = settings()$min_power),
    height = function() max(420, 380 * ceiling(n_panels() / 2))
  )

  # ---- By level ----------------------------------------------------------

  by_level <- bindCache(
    reactive({
      s <- settings()
      l <- level_opts()
      withr::with_seed(
        s$seed,
        agreement_by_level(
          aligned(),
          width = l$width,
          min_seconds = l$min_seconds,
          min_power = s$min_power,
          block = s$block,
          replicates = s$replicates
        )
      )
    }),
    align_key(),
    settings(),
    level_opts()
  )

  output$by_level_plot <- renderPlot(
    plot_by_level(by_level()),
    height = function() max(360, 280 * n_runs())
  )

  output$by_level_pc_plot <- renderPlot(
    plot_by_level(by_level(), scale = "percent"),
    height = function() max(360, 280 * n_runs())
  )

  # The raw tibble has thirteen columns named after the code that made them,
  # which asks the reader to know what bias_lo and gain_pc are. Same treatment
  # as the agreement table: a band is a range, an estimate carries its
  # interval, and nothing is shown that the plot above already says better.
  by_level_rows <- reactive({
    by_level() |>
      dplyr::transmute(
        Band = band_range,
        Pair = paste(source_a, "vs", source_b),
        Run = run,
        Seconds = format(seconds, big.mark = ","),
        `Difference (W)` = with_ci(bias_w, bias_lo, bias_hi, 1),
        `Gain %` = with_ci(gain_pc, gain_lo, gain_hi, 2)
      )
  })

  output$by_level_table <- DT::renderDT(
    by_level_rows(),
    rownames = FALSE,
    selection = "none",
    options = list(
      pageLength = 10,
      dom = "tip",
      ordering = FALSE,
      columnDefs = list(list(className = "ci", targets = c(4, 5)))
    )
  )

  # The finding this tab exists for: how far the gain travels across the range.
  # A pair whose spread is small is the reassuring case, and says so.
  output$level_headline <- renderUI({
    spans <- level_dependence(by_level())

    boxes <- lapply(seq_len(nrow(spans)), function(i) {
      s <- spans[i, ]
      boxed <- s$separated
      bslib::value_box(
        title = paste(s$source_a, "vs", s$source_b),
        value = sprintf("%+.1f to %+.1f%%", s$low_gain, s$high_gain),
        p(sprintf(
          "Gain runs from %+.1f%% at %s to %+.1f%% at %s.",
          s$low_gain,
          s$low_band,
          s$high_gain,
          s$high_band
        )),
        p(
          class = "small mb-0",
          if (boxed) {
            "Those two intervals do not overlap: the average is not a good summary of this pair."
          } else {
            "Those two intervals overlap, so the range does not establish a changing error."
          }
        ),
        theme = bslib::value_box_theme(
          bg = if (boxed) "#fdf5e6" else "#ffffff",
          fg = "#1c1917"
        )
      )
    })
    do.call(
      bslib::layout_column_wrap,
      c(list(width = "320px", class = "mb-3"), boxes)
    )
  })

  # Triggered on the per-cent gain, and only when the extreme bands' intervals
  # are separated. On watts it would fire for every clean percentage meter,
  # whose difference rises across the range purely because a percentage of
  # 300 W exceeds a percentage of 100 W — which is the exact confusion this
  # alert exists to dispel.
  output$level_alert <- renderUI({
    uneven <- level_dependence(by_level())
    uneven <- uneven[uneven$separated, ]
    if (nrow(uneven) == 0) {
      return(NULL)
    }

    div(
      class = "alert alert-warning",
      strong("The error is not constant across the range. "),
      lapply(seq_len(nrow(uneven)), function(i) {
        r <- uneven[i, ]
        p(
          class = "mb-0",
          sprintf(
            paste(
              "%s against %s in %s runs from %+.1f%% at %s to %+.1f%% at %s,",
              "and those two intervals do not overlap. The single average on",
              "the Agreement tab is not a good summary of this pair."
            ),
            r$source_a,
            r$source_b,
            r$run,
            r$low_gain,
            r$low_band,
            r$high_gain,
            r$high_band
          )
        )
      })
    )
  })

  # block_bootstrap_draws() quietly shortens the block to floor(n / 4) when a
  # band holds fewer than two blocks' worth of seconds, so the provenance
  # line's 60-second blocks are not true of the thin ones. Better than an
  # arbitrary seconds threshold, because it names what actually happened.
  output$thin_alert <- renderUI({
    band <- by_level()
    block <- settings()$block
    thin <- band$seconds < 2 * block
    dropped <- attr(by_level(), "dropped_seconds")

    if (!any(thin)) {
      return(NULL)
    }

    at <- which.min(band$seconds)
    div(
      class = "alert alert-warning",
      strong("Some bands are thinner than the bootstrap block. "),
      sprintf(
        paste(
          "%s of %s bands hold fewer than %s seconds, so their blocks were",
          "shortened to a quarter of the band rather than the %s seconds",
          "asked for. The thinnest is %s with %s seconds, whose blocks were",
          "%s seconds. A further %s seconds fell in bands too thin to show at all."
        ),
        sum(thin),
        nrow(band),
        2 * block,
        block,
        band$band_range[at],
        band$seconds[at],
        max(1, floor(band$seconds[at] / 4)),
        format(dropped, big.mark = ",")
      )
    )
  })

  # Stated because the intervals in this table come from bands, not from the
  # whole ride, and a band is a much smaller sample than the pair.
  output$by_level_note <- renderUI({
    l <- level_opts()
    provenance(sprintf(
      " Bands are %s W wide, showing only those with at least %s seconds.",
      l$width,
      l$min_seconds
    ))
  })

  # ---- Chain -------------------------------------------------------------

  bridges <- reactive(bridge_candidates(aligned()))

  # With one candidate there is nothing to choose, so no selector is drawn and
  # input$bridge is never set. Where there is a choice, the selector is drawn
  # by renderUI and can lag the data by a flush, hence the membership guard —
  # without it chain_sources() throws on the reload after a new set of files.
  bridge <- reactive({
    candidates <- bridges()
    if (length(candidates) == 0) {
      return(NULL)
    }
    if (length(candidates) == 1) {
      return(candidates[[1]])
    }
    req(input$bridge %in% candidates)
    input$bridge
  })

  chain_result <- bindCache(
    reactive({
      req(bridge())
      s <- settings()
      out <- try(
        withr::with_seed(
          s$seed,
          chain_sources(
            aligned(),
            bridge(),
            s$min_power,
            s$block,
            s$replicates
          )
        ),
        silent = TRUE
      )
      if (inherits(out, "try-error")) {
        list(error = conditionMessage(attr(out, "condition")))
      } else {
        list(chained = out, chain = chain_report(out))
      }
    }),
    align_key(),
    settings(),
    bridge()
  )

  # Only the pairs that had to be linked. The rest of chain_sources()' output
  # duplicates power_agreement() for pairs that rode together, pooled across
  # runs, and the per-run figures are the more informative of the two.
  chained_rows <- reactive({
    if (length(bridges()) == 0) {
      return(NULL)
    }
    linked <- chain_result()$chained
    if (is.null(linked)) {
      return(NULL)
    }
    out <- linked[linked$basis == "chained", ]
    if (nrow(out) == 0) NULL else out
  })

  # The bridge only matters when something has to be chained through it, so it
  # appears alongside the rows it governs rather than as standing furniture.
  output$bridge_ui <- renderUI({
    req(length(bridges()) > 1)
    selectInput("bridge", "Bridge meter", choices = bridges())
  })

  output$chain_notes <- renderUI({
    if (length(bridges()) == 0) {
      return(NULL)
    }

    result <- chain_result()
    if (!is.null(result$error)) {
      return(div(class = "alert alert-warning", result$error))
    }
    req(chained_rows())

    overlap <- round(100 * result$chain$overlap)
    div(
      class = "alert alert-warning",
      strong("Some rows are chained. "),
      result$chain$assumption,
      sprintf(
        paste(
          " The runs overlap over %s%% of the power range they cover between",
          "them; gains compared over different parts of the range are not",
          "really comparable."
        ),
        overlap
      )
    )
  })

  # ---- Reseed ------------------------------------------------------------

  # What has to match for two seeds to be comparable at all. The seed itself is
  # deliberately not in it.
  comparable <- reactive({
    list(align_key(), settings()[c("min_power", "block", "replicates")])
  })
  previous <- reactiveVal(NULL)

  observeEvent(input$reseed, {
    previous(list(
      seed = current_seed(),
      rows = isolate(pair_rows()),
      key = isolate(comparable())
    ))
    updateNumericInput(session, "seed", value = current_seed() + 1L)
  })

  # Checked at display time against a stored fingerprint, not cleared by an
  # observer. observeEvent fires on invalidation rather than on a changed
  # value, so settings() re-firing after the seed bump would wipe the snapshot
  # every single time — and a stale comparison would put a number computed
  # under different settings beside the current one.
  previous_note <- function(row) {
    snapshot <- previous()
    if (is.null(snapshot) || !identical(snapshot$key, comparable())) {
      return(NULL)
    }
    match <- snapshot$rows[
      snapshot$rows$source_a == row$source_a &
        snapshot$rows$source_b == row$source_b,
    ]
    if (nrow(match) != 1) {
      return(NULL)
    }
    sprintf(
      "Seed %s gave %+.2f (%+.2f to %+.2f).",
      snapshot$seed,
      match$gain_pc,
      match$gain_lo,
      match$gain_hi
    )
  }

  # ---- Report ------------------------------------------------------------

  # The same shape compare_all() builds, assembled from what the tabs already
  # hold. Named rather than inlined into the handler so it can be inspected.
  report_results <- reactive({
    s <- settings()
    l <- level_opts()
    chain <- if (length(bridges()) > 0) {
      chain_result()
    } else {
      list(
        error = paste(
          "No meter appears in more than one run, so there is nothing to",
          "chain through."
        )
      )
    }

    list(
      aligned = aligned(),
      spec = spec_rv(),
      alignment = align_report(),
      agreement = agreement(),
      by_level = by_level(),
      chained = chain$chained,
      chain = chain$chain,
      chain_error = chain$error,
      settings = list(
        min_power = s$min_power,
        width = l$width,
        min_seconds = l$min_seconds,
        block = s$block,
        replicates = s$replicates,
        seed = s$seed,
        max_offset = s$max_offset,
        smooth = s$smooth
      )
    )
  })

  report_path <- reactiveVal(NULL)
  report_error <- reactiveVal(NULL)
  report_stamp <- reactiveVal(NULL)

  # What the rendered file is a picture of. Any change here and the file on
  # disk is no longer what the screen says. spec_rv() is in it because edits
  # that do not trigger a reload still change the file list the report prints.
  report_key <- reactive({
    list(align_key(), settings(), level_opts(), bridge(), spec_rv())
  })

  observeEvent(input$render, {
    path <- file.path(tempdir(), "power-comparison.html")
    # Against house style in general, but a genuine boundary: an error in an
    # observer tears down the session, and the user can act on a message like
    # pandoc not found.
    out <- try(render_comparison(report_results(), path), silent = TRUE)
    if (inherits(out, "try-error")) {
      report_error(conditionMessage(attr(out, "condition")))
      report_path(NULL)
    } else {
      report_error(NULL)
      report_path(path)
      report_stamp(isolate(report_key()))
    }
  })

  # Rendering to a tempfile and copying is what makes the staleness check
  # possible at all, and it makes the second download instant. Rendering inside
  # content() would give a file that silently disagrees with the screen
  # whenever a setting changed after the render.
  output$download <- downloadHandler(
    filename = function() "power-comparison.html",
    content = function(file) file.copy(report_path(), file, overwrite = TRUE)
  )

  output$report_ui <- renderUI({
    if (load_trigger() == 0) {
      return(div(class = "alert alert-info", "Load some rides first."))
    }

    fresh <- !is.null(report_path()) &&
      identical(report_stamp(), report_key())

    tagList(
      # No withProgress: rmarkdown::render() reports nothing about where it is,
      # so a bar could only invent its position. The spinner is honest.
      bslib::input_task_button(
        "render",
        "Render report",
        label_busy = "Rendering…"
      ),
      if (!is.null(report_error())) {
        div(class = "alert alert-danger mt-3", report_error())
      },
      if (fresh) {
        div(
          class = "mt-3",
          downloadButton("download", "Download report", class = "btn-primary")
        )
      } else if (!is.null(report_path())) {
        div(
          class = "alert alert-warning mt-3",
          "The settings have changed since this was rendered, so the file on ",
          "disk no longer matches the screen. Render again before downloading."
        )
      }
    )
  })

  # The Report tab was one paragraph, one button and most of a screen of
  # white, which reads as broken rather than as minimal. What the document
  # will contain is the natural thing to put there.
  output$report_summary <- renderUI({
    req(load_trigger() > 0)
    s <- settings()
    l <- level_opts()
    spec <- spec_rv()

    tagList(
      tags$dl(
        class = "row small mb-3",
        tags$dt(class = "col-7", "Minimum power"),
        tags$dd(class = "col-5", paste(s$min_power, "W")),
        tags$dt(class = "col-7", "Band width"),
        tags$dd(class = "col-5", paste(l$width, "W")),
        tags$dt(class = "col-7", "Minimum seconds per band"),
        tags$dd(class = "col-5", l$min_seconds),
        tags$dt(class = "col-7", "Largest offset considered"),
        tags$dd(class = "col-5", paste(s$max_offset, "s")),
        tags$dt(class = "col-7", "Trace smoothing"),
        tags$dd(class = "col-5", paste(s$smooth, "s")),
        tags$dt(class = "col-7", "Bootstrap"),
        tags$dd(
          class = "col-5",
          sprintf("%s x %ss", s$replicates, s$block)
        ),
        tags$dt(class = "col-7", "Seed"),
        tags$dd(class = "col-5", s$seed)
      ),
      p(class = "small text-body-secondary mb-1", "Files"),
      tags$ul(
        class = "small",
        lapply(seq_len(nrow(spec)), function(i) {
          tags$li(sprintf(
            "%s — %s, %s",
            basename(spec$path[[i]]),
            spec$run[[i]],
            spec$source[[i]]
          ))
        })
      )
    )
  })
}

shinyApp(ui, server)
