#' The app's server
#'
#' Kept whole rather than split per tab: the reactives are shared across
#' tabs -- aligned() feeds Alignment, Agreement, By level and Report -- so
#' splitting them would mean threading a state object through six functions
#' to gain nothing but shorter files.
#' @noRd
app_server <- function(input, output, session) {
  volumes <- c(Home = path.expand("~"), shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(
    input,
    "dir_choose",
    roots = volumes,
    session = session
  )

  opening_dir <- shiny::getShinyOption("cyclingpower_dir")
  if (!is.null(opening_dir)) {
    shiny::updateTextInput(session, "dir", value = opening_dir)
  }

  # ---- Rides -------------------------------------------------------------

  # The directory to scan, rather than input$dir directly: updateTextInput is
  # asynchronous, so a button that sets the box and scans in one go would
  # otherwise scan the previous value.
  scan_dir <- shiny::reactiveVal(NULL)

  # Rendered by the table; changes only on scan or demo. Edits go to spec_rv,
  # so typing in a cell does not re-render the table under the cursor.
  table_spec <- shiny::reactiveVal(NULL)
  spec_rv <- shiny::reactiveVal(NULL)
  load_trigger <- shiny::reactiveVal(0L)
  demo_note <- shiny::reactiveVal(NULL)

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

  # ride_files() is the package's own scanner, shared with read_rides(), so the
  # tab and the API cannot disagree about what counts as a ride file.

  # Counted rather than storing the path alone: a reactiveVal set to the value
  # it already holds does not invalidate, so scanning the same directory twice
  # — which is exactly what you do after correcting the separator — would be a
  # silent no-op.
  scan_trigger <- shiny::reactiveVal(0L)
  rescan <- function(dir) {
    scan_dir(dir)
    demo_note(NULL)
    scan_trigger(shiny::isolate(scan_trigger()) + 1L)
  }

  shiny::observeEvent(input$scan, rescan(input$dir))

  shiny::observeEvent(input$dir_choose, {
    chosen <- shinyFiles::parseDirPath(volumes, input$dir_choose)
    if (length(chosen) > 0) {
      shiny::updateTextInput(session, "dir", value = chosen)
      rescan(as.character(chosen))
    }
  })

  # An alert above the table it describes, not a toast. A notification that
  # explains what the three meters do has to be read in fifteen seconds and
  # then cannot be got back.
  shiny::observeEvent(input$demo, {
    dir <- demo_rides()
    shiny::updateTextInput(session, "dir", value = dir)
    set_spec(build_spec(ride_files(dir), "_"))
    demo_note(paste(
      "Demo rides. assioma is the reference and rides in both sessions;",
      "kickr reads about 2.5% below it with a 7-second clock offset; quarq",
      "reads low below 200 W and high above it. kickr and quarq never ride",
      "together, so their comparison has to be chained through assioma."
    ))
    load_trigger(shiny::isolate(load_trigger()) + 1L)
  })

  output$demo_note <- shiny::renderUI({
    shiny::req(demo_note())
    shiny::div(class = "alert alert-info", demo_note())
  })

  # Returns the problem rather than validate()ing it: the only consumer is an
  # observer, and an observer swallows a validation error, so the message would
  # never reach the screen.
  files <- shiny::eventReactive(scan_trigger(), {
    dir <- scan_dir()
    shiny::req(dir)

    if (!dir.exists(dir)) {
      return(list(error = paste0("No directory at ", dir, ".")))
    }
    found <- ride_files(dir)
    if (length(found) == 0) {
      return(list(error = "No .fit, .gpx or .csv files in that directory."))
    }
    list(paths = found)
  })

  shiny::observeEvent(list(files(), input$sep), {
    shiny::req(files()$paths)
    set_spec(build_spec(files()$paths, input$sep))
  })

  output$scan_status <- shiny::renderUI({
    shiny::req(files()$error)
    shiny::div(class = "alert alert-danger", files()$error)
  })

  shiny::observeEvent(input$spec_cell_edit, {
    edited <- DT::editData(spec_rv(), input$spec_cell_edit, rownames = FALSE)
    edited$run <- trimws(edited$run)
    edited$source <- trimws(edited$source)
    spec_rv(edited)
  })

  # The missing capability: without it a stray .csv in the directory could only
  # be dealt with by moving the file.
  shiny::observeEvent(input$remove_rows, {
    shiny::req(input$spec_rows_selected)
    set_spec(spec_rv()[-input$spec_rows_selected, ])
  })

  output$spec <- DT::renderDT(
    {
      shiny::req(table_spec())
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

  spec_checks <- shiny::reactive({
    spec <- spec_rv()
    shiny::req(spec)

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

  # suspendWhenHidden = FALSE, or the flag the conditionalPanel reads is never
  # sent while the panel it controls is hidden, and step 2 never opens.
  output$has_spec <- shiny::reactive(!is.null(table_spec()))
  shiny::outputOptions(output, "has_spec", suspendWhenHidden = FALSE)

  # An empty card with a Load button in it looks like a step that has been
  # skipped rather than one that is waiting on the step above. Rendered from
  # the same output as the issues: a card body is a flex column with a gap, so
  # an empty uiOutput still costs a row of white.
  output$spec_issues <- shiny::renderUI({
    if (is.null(table_spec())) {
      return(shiny::div(
        class = "alert alert-secondary",
        "Nothing listed yet. Choose a folder at step 1, or load the demo rides."
      ))
    }
    checks <- spec_checks()
    shiny::tagList(
      lapply(checks$blocking, function(m) {
        shiny::div(class = "alert alert-danger", m)
      }),
      lapply(checks$warnings, function(m) {
        shiny::div(class = "alert alert-warning", m)
      })
    )
  })

  shiny::observeEvent(
    input$load,
    load_trigger(shiny::isolate(load_trigger()) + 1L)
  )

  rides <- shiny::eventReactive(load_trigger(), {
    shiny::req(load_trigger() > 0)
    checks <- spec_checks()
    shiny::validate(shiny::need(
      length(checks$blocking) == 0,
      paste(checks$blocking, collapse = " ")
    ))
    read_rides(spec_rv())
  })

  data_version <- shiny::reactiveVal(0L)
  shiny::observeEvent(
    rides(),
    data_version(shiny::isolate(data_version()) + 1L)
  )

  # The link the task button's spinner needs: it reverts when the reactive
  # cycle started by the click completes, and that cycle only contains the file
  # read if something on the visible tab actually depends on rides().
  output$load_status <- shiny::renderUI({
    shiny::req(load_trigger() > 0)
    loaded <- rides()
    shiny::div(
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
      shiny::actionLink("to_alignment", "Check the clock offsets")
    )
  })

  shiny::observeEvent(input$to_alignment, bslib::nav_select("nav", "Alignment"))

  # ---- Record ------------------------------------------------------------

  # The handoff between the two halves of the app is the filesystem and nothing
  # else: the recorder writes into a directory, the Rides tab scans it. Analysis
  # is repeatable and recording is not, so a bug in the analysis costs a re-run
  # while a bug in the recorder costs a ride, and the boundary keeps one from
  # taking the other down.

  shinyFiles::shinyDirChoose(
    input,
    "record_dir_choose",
    roots = volumes,
    session = session
  )

  shiny::updateTextInput(
    session,
    "record_dir",
    value = if (is.null(opening_dir)) {
      file.path(path.expand("~"), "cyclingpower-rides")
    } else {
      opening_dir
    }
  )

  shiny::observeEvent(input$record_dir_choose, {
    chosen <- shinyFiles::parseDirPath(volumes, input$record_dir_choose)
    if (length(chosen) > 0) {
      shiny::updateTextInput(
        session,
        "record_dir",
        value = as.character(chosen)
      )
    }
  })

  # The protocol is data, so the table on the tab and the file you ride cannot
  # drift apart: both come from comparison_protocol().
  protocol <- shiny::reactive({
    comparison_protocol(step_seconds = as.numeric(input$step_minutes) * 60)
  })

  output$protocol_table <- shiny::renderTable(
    {
      collapse_protocol(protocol(), ftp = input$ftp)
    },
    striped = TRUE,
    width = "100%"
  )

  output$download_workout <- shiny::downloadHandler(
    filename = function() {
      paste0("cyclingpower-comparison.", input$workout_format)
    },
    content = function(file) {
      # The handler is handed a path with no extension of its own, so the
      # format has to be chosen before write_workout() sees it.
      built <- tempfile(fileext = paste0(".", input$workout_format))
      write_workout(built, ftp = input$ftp, protocol = protocol())
      file.copy(built, file, overwrite = TRUE)
    }
  )

  # What the page reported it could do, rendered only once it has said so.
  # Until then there is nothing to show.
  output$ble_status <- shiny::renderUI({
    if (is.null(input$ble_supported) || isTRUE(input$ble_supported)) {
      return(NULL)
    }
    shiny::div(
      class = "alert alert-danger",
      shiny::strong("This browser cannot record. "),
      "Recording needs Web Bluetooth, which Chrome and Edge have and Safari ",
      "and Firefox do not \u2014 WebKit has never shipped it and Mozilla declined ",
      "the specification. Open the app in Chrome to record, or record on head ",
      "units as before and scan the files on the Rides tab."
    )
  })

  meters <- shiny::reactiveVal(list())
  meter_states <- shiny::reactiveVal(list())

  shiny::observeEvent(input$meter_paired, {
    paired <- input$meter_paired
    meters(c(
      meters(),
      list(list(
        slot = paired$slot,
        name = paired$name,
        colour = paired$colour
      ))
    ))
  })

  shiny::observeEvent(input$meter_state, {
    states <- meter_states()
    states[[as.character(input$meter_state$slot)]] <- input$meter_state$state
    meter_states(states)
  })

  # Advertised names are ASSIOMA-12345 and KICKR CORE 8422, which are neither
  # filenames nor anything you want in a chart legend. A starting point to
  # overwrite, not a decision.
  slug <- function(name) {
    out <- tolower(gsub("[^A-Za-z0-9]+", "", name))
    if (nzchar(out)) out else "meter"
  }

  # Isolated, and re-rendered only when the set of meters changes: reading the
  # boxes reactively would rebuild the whole list on every keystroke and take
  # the cursor with it.
  output$meter_labels <- shiny::renderUI({
    paired <- meters()
    if (length(paired) == 0) {
      return(shiny::p(
        class = "small text-body-secondary mb-0",
        "No meters paired yet."
      ))
    }

    rows <- lapply(paired, function(meter) {
      id <- paste0("label_", meter$slot)
      current <- shiny::isolate(input[[id]])
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap mb-2",
        shiny::div(
          style = "min-width: 180px;",
          shiny::textInput(
            id,
            paste("Source label for", meter$name),
            value = if (is.null(current) || !nzchar(current)) {
              slug(meter$name)
            } else {
              current
            }
          )
        ),
        shiny::div(
          class = "mb-3",
          shiny::actionButton(
            paste0("drop_", meter$slot),
            "Unpair",
            class = "btn-sm btn-outline-secondary"
          )
        )
      )
    })

    shiny::tagList(
      shiny::p(
        class = "small text-body-secondary mb-2",
        "The source label names the meter in every chart and becomes part of ",
        "the filename."
      ),
      rows
    )
  })

  # One observer per slot, created when the slot appears. observeEvent on a
  # dynamically named input cannot be written once for all of them.
  shiny::observeEvent(meters(), {
    for (meter in meters()) {
      local({
        slot <- meter$slot
        id <- paste0("drop_", slot)
        if (is.null(session$userData[[id]])) {
          session$userData[[id]] <- shiny::observeEvent(input[[id]], {
            session$sendCustomMessage("meter_drop", list(slot = slot))
            meters(Filter(function(m) m$slot != slot, meters()))
          })
        }
      })
    }
  })

  recording <- shiny::reactiveVal(NULL)
  record_error <- shiny::reactiveVal(NULL)
  last_recording <- shiny::reactiveVal(NULL)

  meter_labels <- shiny::reactive({
    stats::setNames(
      lapply(meters(), function(m) input[[paste0("label_", m$slot)]]),
      vapply(meters(), function(m) as.character(m$slot), character(1))
    )
  })

  shiny::observeEvent(input$start_record, {
    labels <- shiny::isolate(meter_labels())

    if (length(labels) == 0) {
      record_error("Pair at least one meter before recording.")
      return()
    }
    # Without this an unlabelled meter drops out of the list sent to the page,
    # which then has no name to file its samples under and quietly records
    # everything except that meter.
    if (any(vapply(labels, function(x) !isTRUE(nzchar(x)), logical(1)))) {
      record_error("Every paired meter needs a source label.")
      return()
    }
    sources <- unlist(labels, use.names = FALSE)

    # Caught rather than let through: an error in an observer tears the session
    # down, and every one of these is something the user can act on — a name
    # with an underscore in it, a directory that is not writable, a run already
    # recorded.
    opened <- try(
      start_recording(input$record_dir, input$record_run, sources),
      silent = TRUE
    )
    if (inherits(opened, "try-error")) {
      record_error(conditionMessage(attr(opened, "condition")))
      return()
    }

    record_error(NULL)
    last_recording(NULL)
    recording(opened)
    session$sendCustomMessage("record_start", list(labels = labels))
  })

  # The page sends the browser's clock at arrival, as ISO 8601 with the
  # trailing Z, and it is written through unaltered. Re-stamping here would put
  # the websocket's latency into the timestamps and lose the one property the
  # whole arrangement exists for.
  samples_frame <- function(rows) {
    data.frame(
      source = vapply(rows, function(r) r$source, character(1)),
      time = vapply(rows, function(r) r$time, character(1)),
      power = vapply(rows, function(r) as.numeric(r$power), numeric(1)),
      stringsAsFactors = FALSE
    )
  }

  # Priority, above the stop observer's. Stopping queues a final batch from the
  # page before the button press reaches the server, and both can arrive in one
  # flush; without the ordering the last half-second of a ride would be written
  # after the files had been closed.
  shiny::observeEvent(
    input$samples,
    {
      open <- recording()
      shiny::req(open)
      recording(record_samples(open, samples_frame(input$samples$rows)))
    },
    priority = 10
  )

  shiny::observeEvent(
    input$stop_record,
    {
      open <- recording()
      shiny::req(open)
      session$sendCustomMessage("record_stop", list())
      last_recording(stop_recording(open))
      recording(NULL)
    },
    priority = -10
  )

  # A tab killed mid-ride leaves the connections open in a process that outlives
  # it. Everything written is already on disk — record_samples() flushes each
  # batch — so this closes the handles rather than saving anything.
  session$onSessionEnded(function() {
    open <- shiny::isolate(recording())
    if (!is.null(open)) {
      stop_recording(open)
    }
  })

  output$record_status <- shiny::renderUI({
    if (!is.null(record_error())) {
      return(shiny::div(class = "alert alert-danger", record_error()))
    }

    open <- recording()
    if (!is.null(open)) {
      states <- meter_states()
      adrift <- names(states)[
        !vapply(
          states,
          identical,
          logical(1),
          "connected"
        )
      ]
      paired <- vapply(meters(), function(m) as.character(m$slot), character(1))
      adrift <- intersect(adrift, paired)

      return(shiny::tagList(
        shiny::div(
          class = "alert alert-success py-2",
          shiny::strong("Recording. "),
          sprintf(
            "%s written to %s.",
            paste(
              sprintf("%s: %s rows", open$sources, open$rows),
              collapse = ", "
            ),
            open$dir
          )
        ),
        if (length(adrift) > 0) {
          shiny::div(
            class = "alert alert-warning py-2 mb-0",
            shiny::strong("A meter has gone. "),
            "Its file will have a gap over the seconds it missed, which is ",
            "what it should have: those seconds are absent from the ",
            "comparison rather than counted as two meters agreeing on zero."
          )
        }
      ))
    }

    done <- last_recording()
    if (is.null(done)) {
      return(NULL)
    }

    shiny::div(
      class = "alert alert-success py-2 mb-0",
      sprintf(
        "Recorded %s. ",
        paste(
          sprintf("%s (%s rows)", basename(done$path), done$rows),
          collapse = ", "
        )
      ),
      shiny::actionLink("record_to_rides", "Load these rides")
    )
  })

  shiny::observeEvent(input$record_to_rides, {
    dir <- dirname(last_recording()$path[[1]])
    shiny::updateTextInput(session, "dir", value = dir)
    rescan(dir)
    bslib::nav_select("nav", "Rides")
  })

  # ---- Settings ----------------------------------------------------------

  # Clearing the box leaves the input NA, which with_seed rejects, so every
  # panel in the app would go down until something was typed back in.
  current_seed <- shiny::reactive(if (is.na(input$seed)) 1L else input$seed)

  # Bundled and debounced together, so a drag settles into one recompute rather
  # than a cascade, and events queued during a slow bootstrap collapse into one.
  # max_offset belongs in here now that it sits in the sidebar where anyone can
  # drag it: it is part of align_key(), so every intermediate value would
  # otherwise invalidate every cached result in the app.
  settings <- shiny::debounce(
    shiny::reactive(list(
      min_power = input$min_power,
      block = as.numeric(input$block),
      replicates = as.numeric(input$replicates),
      seed = current_seed(),
      max_offset = input$max_offset,
      smooth = input$smooth
    )),
    700
  )

  level_opts <- shiny::debounce(
    shiny::reactive(list(width = input$width, min_seconds = input$min_seconds)),
    700
  )

  # The settings sit in a sidebar and the numbers render per tab, so nothing on
  # screen otherwise says what produced them. One component, rendered under each
  # set of numbers it governs.
  provenance <- function(...) {
    s <- settings()
    shiny::div(
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

  alignment <- shiny::reactive({
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

  aligned <- shiny::reactive(alignment()$data)
  align_report <- shiny::reactive(alignment_report(aligned()))

  # What identifies the aligned data for caching. Everything expensive is keyed
  # on this rather than on the tibble itself, which would be hashed on every
  # read.
  align_key <- shiny::reactive(list(data_version(), settings()$max_offset))

  # status first: it decides whether the rest of the row means anything, and
  # as the last column it fell off screen below 900px. offset is integer by
  # construction, so it is shown without decimals.
  output$align_table <- shiny::renderTable(
    {
      align_report() |>
        dplyr::transmute(
          Run = .data$run,
          Source = .data$source,
          Status = .data$status,
          `Offset (s)` = sprintf("%+d", as.integer(round(.data$offset))),
          Correlation = sprintf("%.3f", .data$correlation),
          Sharpness = sprintf("%.2f", .data$sharpness)
        )
    },
    align = "lllrrr"
  )

  output$align_provenance <- shiny::renderUI({
    provenance(sprintf(
      " Clock offsets were sought up to %s s either way.",
      settings()$max_offset
    ))
  })

  # One box per source: what the clock correction was, and whether it was
  # established or fallen back on.
  output$align_headline <- shiny::renderUI({
    report <- align_report()
    boxes <- lapply(seq_len(nrow(report)), function(i) {
      r <- report[i, ]
      bslib::value_box(
        title = paste(r$source, "\u2014", r$run),
        value = if (r$status == "unresolved") {
          "not found"
        } else {
          sprintf("%+d s", as.integer(round(r$offset)))
        },
        shiny::p(switch(
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
  output$align_alert <- shiny::renderUI({
    report <- align_report()
    unresolved <- report$source[report$status == "unresolved"]
    if (length(unresolved) == 0) {
      return(NULL)
    }
    shiny::div(
      class = "alert alert-warning",
      shiny::strong("Alignment incomplete. "),
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
  output$edge_alert <- shiny::renderUI({
    report <- align_report()
    limit <- settings()$max_offset
    at_edge <- report$status == "aligned" & abs(report$offset) >= limit
    if (!any(at_edge)) {
      return(NULL)
    }
    shiny::div(
      class = "alert alert-warning",
      shiny::strong("An offset is at the edge of the search. "),
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

  n_runs <- shiny::reactive(dplyr::n_distinct(aligned()$run))

  output$timeseries <- shiny::renderPlot(
    plot_timeseries(aligned(), smooth = settings()$smooth),
    height = function() max(320, 240 * n_runs())
  )

  # ---- Agreement ---------------------------------------------------------

  agreement <- shiny::bindCache(
    shiny::reactive({
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
  # the table into thirteen columns of numbers, with the intervals too far from
  # their point estimates to read together.
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
  pair_rows <- shiny::reactive({
    direct <- agreement() |>
      dplyr::transmute(
        source_a = .data$source_a,
        source_b = .data$source_b,
        from = .data$run,
        basis = "direct",
        gain_pc = .data$gain_pc,
        gain_lo = .data$gain_lo,
        gain_hi = .data$gain_hi,
        detail = sprintf(
          "From %s, %s s.",
          .data$run,
          format(.data$seconds, big.mark = ",")
        )
      )

    linked <- chained_rows()
    if (is.null(linked)) {
      return(direct)
    }

    dplyr::bind_rows(
      direct,
      dplyr::transmute(
        linked,
        source_a = .data$source_a,
        source_b = .data$source_b,
        from = paste("chained via", .data$via),
        basis = "chained",
        gain_pc = .data$gain_pc,
        gain_lo = .data$gain_lo,
        gain_hi = .data$gain_hi,
        detail = paste("Chained via", .data$via, "\u2014 no shared seconds.")
      )
    )
  })

  # The answer, stated in a sentence, before any table. Phrased strictly in the
  # estimate's own orientation: +2.58 is source_a above source_b, and the
  # reciprocal (source_b below source_a) is a different number.
  output$headline <- shiny::renderUI({
    rows <- pair_rows()
    boxes <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      bslib::value_box(
        title = paste(r$source_a, "vs", r$source_b),
        value = sprintf("%+.2f%%", r$gain_pc),
        showcase = if (r$basis == "chained") shiny::icon("link"),
        showcase_layout = "left center",
        # Always in source_a's orientation. Turning it round — "kickr reads
        # 2.58% below assioma" — is the reciprocal, which is a different
        # number (2.51%), and it would disagree with the table below.
        shiny::p(sprintf(
          "%s reads %.2f%% %s than %s.",
          r$source_a,
          abs(r$gain_pc),
          if (r$gain_pc >= 0) "higher" else "lower",
          r$source_b
        )),
        shiny::p(
          class = "small mb-0",
          sprintf("95%% interval %+.2f to %+.2f. ", r$gain_lo, r$gain_hi),
          r$detail
        ),
        shiny::p(class = "small mb-0", previous_note(r)),
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
  pair_table <- shiny::reactive({
    direct <- agreement() |>
      dplyr::transmute(
        Pair = paste(.data$source_a, "vs", .data$source_b),
        From = .data$run,
        `Gain %` = with_ci(.data$gain_pc, .data$gain_lo, .data$gain_hi, 2),
        `Difference (W)` = with_ci(
          .data$bias_w,
          .data$bias_lo,
          .data$bias_hi,
          1
        ),
        # Signed like every other number in the table. Signing the estimate
        # and not the bounds reads as a difference in kind rather than a
        # difference in formatting.
        `Limits of agreement (W)` = sprintf(
          "%+.1f to %+.1f",
          .data$loa_lo,
          .data$loa_hi
        ),
        Seconds = format(.data$seconds, big.mark = ","),
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
        Pair = paste(.data$source_a, "vs", .data$source_b),
        From = paste("chained via", .data$via),
        `Gain %` = with_ci(.data$gain_pc, .data$gain_lo, .data$gain_hi, 2),
        `Difference (W)` = "\u2014",
        `Limits of agreement (W)` = "\u2014",
        Seconds = "\u2014",
        basis = "chained"
      )
    )
  })

  # Built by hand rather than through renderTable so a chained row can carry a
  # class. Unmarked, -3.11 (-4.22 to -2.02) looks exactly as authoritative as
  # the direct rows above it, and the three em-dashes read as missing data
  # instead of a comparison that has no watts to report.
  output$pair_table <- shiny::renderUI({
    rows <- pair_table()
    numeric_cols <- c("Gain %", "Difference (W)")

    header <- shiny::tags$tr(lapply(
      setdiff(names(rows), "basis"),
      function(nm) {
        shiny::tags$th(class = if (nm %in% numeric_cols) "ci", nm)
      }
    ))

    body <- lapply(seq_len(nrow(rows)), function(i) {
      r <- rows[i, ]
      shiny::tags$tr(
        class = if (r$basis == "chained") "chained",
        lapply(setdiff(names(rows), "basis"), function(nm) {
          shiny::tags$td(
            class = if (nm %in% numeric_cols) "ci",
            if (nm == "From" && r$basis == "chained") {
              shiny::tagList(
                shiny::span(class = "badge-chained", "chained"),
                " ",
                r[[nm]]
              )
            } else {
              r[[nm]]
            }
          )
        })
      )
    })

    shiny::tagList(
      shiny::div(
        style = "overflow-x: auto;",
        shiny::tags$table(
          class = "table shiny-table",
          shiny::tags$thead(header),
          shiny::tags$tbody(body)
        )
      ),
      if (any(rows$basis == "chained")) {
        shiny::p(
          class = "small text-body-secondary mt-2 mb-0",
          "\u2014 no second that both meters recorded, so there is no difference ",
          "in watts and no limits of agreement to report."
        )
      }
    )
  })

  output$provenance <- shiny::renderUI(provenance())

  # Panels, not runs: a pair generally rides in one run, so what multiplies the
  # facets is the number of pairs actually ridden together.
  n_panels <- shiny::reactive({
    nrow(unique(agreement()[, c("run", "source_a", "source_b")]))
  })

  output$agreement_plot <- shiny::renderPlot(
    plot_agreement(aligned(), min_power = settings()$min_power),
    height = function() max(420, 380 * ceiling(n_panels() / 2))
  )

  # ---- By level ----------------------------------------------------------

  by_level <- shiny::bindCache(
    shiny::reactive({
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

  output$by_level_plot <- shiny::renderPlot(
    plot_by_level(by_level()),
    height = function() max(360, 280 * n_runs())
  )

  output$by_level_pc_plot <- shiny::renderPlot(
    plot_by_level(by_level(), scale = "percent"),
    height = function() max(360, 280 * n_runs())
  )

  # The raw tibble has thirteen columns named after the code that made them,
  # which asks the reader to know what bias_lo and gain_pc are. Same treatment
  # as the agreement table: a band is a range, an estimate carries its
  # interval, and nothing is shown that the plot above already says better.
  by_level_rows <- shiny::reactive({
    by_level() |>
      dplyr::transmute(
        Band = .data$band_range,
        Pair = paste(.data$source_a, "vs", .data$source_b),
        Run = .data$run,
        Seconds = format(.data$seconds, big.mark = ","),
        `Difference (W)` = with_ci(
          .data$bias_w,
          .data$bias_lo,
          .data$bias_hi,
          1
        ),
        `Gain %` = with_ci(.data$gain_pc, .data$gain_lo, .data$gain_hi, 2)
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
  output$level_headline <- shiny::renderUI({
    spans <- level_dependence(by_level())

    boxes <- lapply(seq_len(nrow(spans)), function(i) {
      s <- spans[i, ]
      boxed <- s$separated
      bslib::value_box(
        title = paste(s$source_a, "vs", s$source_b),
        value = sprintf("%+.1f to %+.1f%%", s$low_gain, s$high_gain),
        shiny::p(sprintf(
          "Gain runs from %+.1f%% at %s to %+.1f%% at %s.",
          s$low_gain,
          s$low_band,
          s$high_gain,
          s$high_band
        )),
        shiny::p(
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
  # 300 W exceeds a percentage of 100 W. See level_dependence().
  output$level_alert <- shiny::renderUI({
    uneven <- level_dependence(by_level())
    uneven <- uneven[uneven$separated, ]
    if (nrow(uneven) == 0) {
      return(NULL)
    }

    shiny::div(
      class = "alert alert-warning",
      shiny::strong("The error is not constant across the range. "),
      lapply(seq_len(nrow(uneven)), function(i) {
        r <- uneven[i, ]
        shiny::p(
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
  output$thin_alert <- shiny::renderUI({
    band <- by_level()
    block <- settings()$block
    thin <- band$seconds < 2 * block
    dropped <- attr(by_level(), "dropped_seconds")

    if (!any(thin)) {
      return(NULL)
    }

    at <- which.min(band$seconds)
    shiny::div(
      class = "alert alert-warning",
      shiny::strong("Some bands are thinner than the bootstrap block. "),
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
  output$by_level_note <- shiny::renderUI({
    l <- level_opts()
    provenance(sprintf(
      " Bands are %s W wide, showing only those with at least %s seconds.",
      l$width,
      l$min_seconds
    ))
  })

  # ---- Chain -------------------------------------------------------------

  bridges <- shiny::reactive(bridge_candidates(aligned()))

  # With one candidate there is nothing to choose, so no selector is drawn and
  # input$bridge is never set. Where there is a choice, the selector is drawn
  # by renderUI and can lag the data by a flush, hence the membership guard —
  # without it chain_sources() throws on the reload after a new set of files.
  bridge <- shiny::reactive({
    candidates <- bridges()
    if (length(candidates) == 0) {
      return(NULL)
    }
    if (length(candidates) == 1) {
      return(candidates[[1]])
    }
    shiny::req(input$bridge %in% candidates)
    input$bridge
  })

  chain_result <- shiny::bindCache(
    shiny::reactive({
      shiny::req(bridge())
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
  chained_rows <- shiny::reactive({
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
  output$bridge_ui <- shiny::renderUI({
    shiny::req(length(bridges()) > 1)
    shiny::selectInput("bridge", "Bridge meter", choices = bridges())
  })

  output$chain_notes <- shiny::renderUI({
    if (length(bridges()) == 0) {
      return(NULL)
    }

    result <- chain_result()
    if (!is.null(result$error)) {
      return(shiny::div(class = "alert alert-warning", result$error))
    }
    shiny::req(chained_rows())

    overlap <- round(100 * result$chain$overlap)
    shiny::div(
      class = "alert alert-warning",
      shiny::strong("Some rows are chained. "),
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

  # What has to match for two seeds to be comparable at all. Not the seed
  # itself.
  comparable <- shiny::reactive({
    list(align_key(), settings()[c("min_power", "block", "replicates")])
  })
  previous <- shiny::reactiveVal(NULL)

  shiny::observeEvent(input$reseed, {
    previous(list(
      seed = current_seed(),
      rows = shiny::isolate(pair_rows()),
      key = shiny::isolate(comparable())
    ))
    shiny::updateNumericInput(session, "seed", value = current_seed() + 1L)
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
  report_results <- shiny::reactive({
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

  report_path <- shiny::reactiveVal(NULL)
  report_error <- shiny::reactiveVal(NULL)
  report_stamp <- shiny::reactiveVal(NULL)

  # What the rendered file is a picture of. Any change here and the file on
  # disk is no longer what the screen says. spec_rv() is in it because edits
  # that do not trigger a reload still change the file list the report prints.
  report_key <- shiny::reactive({
    list(align_key(), settings(), level_opts(), bridge(), spec_rv())
  })

  shiny::observeEvent(input$render, {
    path <- file.path(tempdir(), "power-comparison.html")
    # Against house style in general, but a boundary: an error in an observer
    # tears down the session, and the user can act on a message like pandoc not
    # found.
    out <- try(render_comparison(report_results(), path), silent = TRUE)
    if (inherits(out, "try-error")) {
      report_error(conditionMessage(attr(out, "condition")))
      report_path(NULL)
    } else {
      report_error(NULL)
      report_path(path)
      report_stamp(shiny::isolate(report_key()))
    }
  })

  # Rendering to a tempfile and copying is what makes the staleness check
  # possible at all, and it makes the second download instant. Rendering inside
  # content() would give a file that silently disagrees with the screen
  # whenever a setting changed after the render.
  output$download <- shiny::downloadHandler(
    filename = function() "power-comparison.html",
    content = function(file) file.copy(report_path(), file, overwrite = TRUE)
  )

  output$report_ui <- shiny::renderUI({
    if (load_trigger() == 0) {
      return(shiny::div(class = "alert alert-info", "Load some rides first."))
    }

    fresh <- !is.null(report_path()) &&
      identical(report_stamp(), report_key())

    shiny::tagList(
      # No withProgress: rmarkdown::render() reports nothing about where it is,
      # so a bar could only invent its position. A spinner cannot.
      bslib::input_task_button(
        "render",
        "Render report",
        label_busy = "Rendering\u2026"
      ),
      if (!is.null(report_error())) {
        shiny::div(class = "alert alert-danger mt-3", report_error())
      },
      if (fresh) {
        shiny::div(
          class = "mt-3",
          shiny::downloadButton(
            "download",
            "Download report",
            class = "btn-primary"
          )
        )
      } else if (!is.null(report_path())) {
        shiny::div(
          class = "alert alert-warning mt-3",
          "The settings have changed since this was rendered, so the file on ",
          "disk no longer matches the screen. Render again before downloading."
        )
      }
    )
  })

  # A summary of what the document will contain, so the Report tab is not a
  # button and an otherwise blank screen.
  output$report_summary <- shiny::renderUI({
    shiny::req(load_trigger() > 0)
    s <- settings()
    l <- level_opts()
    spec <- spec_rv()

    shiny::tagList(
      shiny::tags$dl(
        class = "row small mb-3",
        shiny::tags$dt(class = "col-7", "Minimum power"),
        shiny::tags$dd(class = "col-5", paste(s$min_power, "W")),
        shiny::tags$dt(class = "col-7", "Band width"),
        shiny::tags$dd(class = "col-5", paste(l$width, "W")),
        shiny::tags$dt(class = "col-7", "Minimum seconds per band"),
        shiny::tags$dd(class = "col-5", l$min_seconds),
        shiny::tags$dt(class = "col-7", "Largest offset considered"),
        shiny::tags$dd(class = "col-5", paste(s$max_offset, "s")),
        shiny::tags$dt(class = "col-7", "Trace smoothing"),
        shiny::tags$dd(class = "col-5", paste(s$smooth, "s")),
        shiny::tags$dt(class = "col-7", "Bootstrap"),
        shiny::tags$dd(
          class = "col-5",
          sprintf("%s x %ss", s$replicates, s$block)
        ),
        shiny::tags$dt(class = "col-7", "Seed"),
        shiny::tags$dd(class = "col-5", s$seed)
      ),
      shiny::p(class = "small text-body-secondary mb-1", "Files"),
      shiny::tags$ul(
        class = "small",
        lapply(seq_len(nrow(spec)), function(i) {
          shiny::tags$li(sprintf(
            "%s \u2014 %s, %s",
            basename(spec$path[[i]]),
            spec$run[[i]],
            spec$source[[i]]
          ))
        })
      )
    )
  })
}
