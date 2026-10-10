#' The app's interface
#'
#' One function per tab, assembled here. The tabs run in the order the work
#' happens, and every input id is unqualified: record.js addresses #pair_meter
#' and #stop_record directly, so a shiny module namespace would silently break
#' pairing and the ordered stop.
#' @noRd
app_ui <- function() {
  bslib::page_navbar(
    title = "cyclingpower",
    id = "nav",
    theme = app_theme(),

    # The tabs run in the order the work happens, but the app opens on Rides:
    # most sessions are analysing files that already exist, and landing someone
    # in Safari on a tab whose only content is that they need Chrome is a poor
    # first screen for a package that is mostly an analyser.
    selected = "Rides",

    # power-packet.js first: it defines the parser record.js calls, and holds
    # nothing else, so the tests can run it without a browser.
    header = shiny::tags$head(
      shiny::tags$script(src = file.path(www_prefix, "power-packet.js")),
      shiny::tags$script(src = file.path(www_prefix, "record.js"))
    ),

    # bslib fills the viewport by default, which turns every card into a fixed
    # porthole with a hidden inner scroll — and, because plotOutput(height =
    # "auto") then collapses to nothing while renderPlot sizes the image, lets
    # the by-level table paint straight over its own plot. Let the page scroll.
    fillable = FALSE,

    # No explicit progress bar: none of the package functions report progress, so
    # a bar would jump 0 to 100 with a lie in between. Shiny dims recalculating
    # outputs on its own.

    # Every setting lives here, not on the tab it affects. Three of them reach
    # the downloaded report, which outlives the session, and max_offset
    # invalidates the results on every tab at once.
    sidebar = bslib::sidebar(
      id = "settings",
      width = 280,
      title = "Analysis settings",

      shiny::div(
        class = "sidebar-note",
        "These settings apply to the Alignment, Agreement and By level tabs ",
        "and the saved report."
      ),

      sidebar_heading("Comparison", "Agreement, By level and the report."),
      shiny::sliderInput("min_power", "Minimum power (W)", 0, 400, 0, step = 5),
      shiny::helpText(
        "Drops seconds where either meter reads below this. Coasting is always ",
        "dropped, since two zeros agree trivially."
      ),
      shiny::selectInput(
        "block",
        "Bootstrap block (s)",
        BLOCK_CHOICES,
        selected = 60
      ),
      shiny::selectInput(
        "replicates",
        "Bootstrap replicates",
        REPLICATE_CHOICES,
        selected = 500
      ),
      shiny::numericInput("seed", "Seed", value = 1, min = 1, step = 1),
      shiny::actionButton(
        "reseed",
        "Reseed",
        class = "btn-sm btn-outline-secondary"
      ),
      shiny::helpText(
        "Intervals are resampled and move slightly between runs. If a ",
        "conclusion flips on reseeding, use more replicates."
      ),

      shiny::hr(),
      sidebar_heading(
        "Alignment",
        "The Alignment tab. The offsets found there carry into every later number."
      ),
      shiny::sliderInput(
        "max_offset",
        "Largest offset to consider (s)",
        0,
        120,
        30,
        5
      ),
      shiny::sliderInput("smooth", "Trace smoothing (s)", 1, 120, 30, 1),

      shiny::hr(),
      sidebar_heading("Power bands", "The By level tab."),
      shiny::sliderInput("width", "Band width (W)", 10, 100, 25, 5),
      shiny::sliderInput(
        "min_seconds",
        "Minimum seconds per band",
        0,
        300,
        30,
        10
      )
    ),

    ui_record(),
    ui_rides(),
    ui_alignment(),
    ui_agreement(),
    ui_levels(),
    ui_report()
  )
}

#' @noRd
ui_record <- function() {
  bslib::nav_panel(
    "Record",
    guidance(
      "Pair each meter, then record. One clock stamps every sample, and the ",
      "files appear on the Rides tab."
    ),
    shiny::uiOutput("ble_status"),

    # Nothing below is drawn without Web Bluetooth: on Safari the pair button
    # would open no chooser and say nothing about why.
    shiny::conditionalPanel(
      condition = "input.ble_supported === true",

      # Sticky, so the clock, the meters and the controls stay on screen while
      # the rider scrolls the protocol. The tiles and the clock are painted by
      # record.js from state it already holds: round-tripping live watts
      # through R would put the recorder behind the analysis tabs' work.
      shiny::div(
        class = "record-bar",
        shiny::div(
          class = "record-bar-top",
          shiny::div(id = "record_clock", class = "record-clock", "0:00"),
          shiny::div(
            class = "actions",
            shiny::actionButton(
              "pair_meter",
              "Pair a meter",
              class = "btn-outline-secondary btn-lg"
            ),
            shiny::actionButton(
              "start_record",
              "Start recording",
              class = "btn-success btn-lg"
            ),
            shiny::actionButton(
              "stop_record",
              "Stop",
              class = "btn-danger btn-lg"
            )
          )
        ),
        shiny::div(id = "meter_status"),
        shiny::uiOutput("record_status")
      ),

      shiny::div(
        class = "alert alert-warning",
        shiny::strong("Zero-offset every meter "),
        "after a few minutes of easy pedalling and just before you start. ",
        "It cannot be done afterwards."
      ),

      bslib::card(
        bslib::card_header("Live trace"),
        guidance("The last three minutes. Check that the meters move together."),
        shiny::tags$canvas(id = "record_trace")
      ),

      bslib::card(
        bslib::card_header("Setup"),
        shiny::div(
          class = "alert alert-warning",
          shiny::strong("Each meter takes one Bluetooth connection. "),
          "Connect your training app to the trainer over ANT+, or this page ",
          "cannot pair it. Head units often take the trainer over Bluetooth ",
          "first, so check."
        ),
        guidance(
          "Pairing opens a browser chooser each time, so three meters take ",
          "three presses."
        ),
        shiny::uiOutput("meter_labels"),
        shiny::div(
          class = "d-flex gap-2 align-items-end flex-wrap actions-inline",
          shiny::div(
            class = "flex-grow-1",
            style = "min-width: 240px;",
            shiny::textInput("record_dir", "Folder", width = "100%")
          ),
          shinyFiles::shinyDirButton(
            "record_dir_choose",
            "Browse\u2026",
            "Choose a directory to record into",
            buttonType = "outline-secondary"
          ),
          shiny::textInput("record_run", "Run", value = "run1", width = "140px")
        ),
        shiny::helpText(
          "Files are saved as ",
          shiny::tags$code("<run>_<source>.csv"),
          ". Use letters, digits and hyphens. Existing files are kept."
        )
      ),

      bslib::card(
        bslib::card_header("What to ride"),
        guidance(
          "Each block serves the analysis. Ride the same session every time: a ",
          "chained comparison assumes the bridge meter reads the same in both ",
          "runs."
        ),
        shiny::tableOutput("protocol_table"),
        shiny::div(
          class = "d-flex gap-2 align-items-end flex-wrap actions-inline mb-3",
          shiny::numericInput(
            "ftp",
            "FTP (W)",
            value = 250,
            min = 60,
            max = 600,
            step = 5,
            width = "120px"
          ),
          shiny::selectInput(
            "step_minutes",
            "Minutes per step",
            choices = c(2, 3, 4),
            selected = 2,
            width = "150px"
          ),
          shiny::selectInput(
            "workout_format",
            "Workout file",
            choices = c(
              "Zwift (.zwo)" = "zwo",
              "Golden Cheetah, TrainerDay (.erg)" = "erg"
            ),
            width = "260px"
          ),
          shiny::div(
            class = "mb-3",
            shiny::downloadButton(
              "download_workout",
              "Download",
              class = "btn-primary"
            )
          )
        ),
        shiny::helpText(
          "A ",
          shiny::tags$code(".zwo"),
          " file sets the surges and coasts to free ride. An ",
          shiny::tags$code(".erg"),
          " file cannot, so leave erg by hand for those blocks. Send the ",
          "targets over ANT+, because this page holds the trainer's Bluetooth."
        ),
        details(
          "Why these blocks",
          shiny::p(
            shiny::tags$b("The surges make a zero offset evidence. "),
            "One clock stamps every meter, so the offset is zero by ",
            "construction. ",
            shiny::tags$code("check_alignment()"),
            " reports a flat trace as ",
            shiny::tags$code("unresolved"),
            ", the same as a wrong clock. Only a resolved zero shows the ",
            "stamps are right."
          ),
          shiny::p(
            shiny::tags$b("The ladder runs up and back down. "),
            "A warming meter reads differently at the same target on the way ",
            "down. A level-dependent one repeats itself. Ridden one way, ",
            shiny::tags$code("level_dependence()"),
            " cannot tell them apart."
          ),
          shiny::p(
            shiny::tags$b("Two minutes a step is the floor. "),
            "Up and down share bands, so a band holds about four 60-second ",
            "bootstrap blocks. Shorter steps leave fewer than three. Four ",
            "minutes narrows bands by about 17% for twice the ladder."
          ),
          shiny::p(
            shiny::tags$b("Erg makes the trainer the controlled variable. "),
            "The trainer is nearly flat within a step, so most of the spread ",
            "comes from the other meter. The level is the mean of the two, so ",
            "nothing is biased, but the trainer decides which band a second ",
            "lands in."
          )
        )
      )
    )
  )
}

#' @noRd
ui_rides <- function() {
  bslib::nav_panel(
    "Rides",
    # Numbered steps: listing a folder and reading the files off disk are
    # separate acts because you correct the labels in between, and the
    # numbering is what says which comes first.
    bslib::card(
      bslib::card_header(
        shiny::span(class = "step-number", "1"),
        "Choose a folder"
      ),
      guidance(
        "A run is one session recorded by two or more meters. Name files ",
        shiny::tags$code("<run>_<source>"),
        ", such as ",
        shiny::tags$code("run1_kickr.fit"),
        ", and they label themselves. Label anything else at step 2."
      ),
      shiny::div(
        class = "d-flex gap-2 align-items-end flex-wrap actions-inline",
        shiny::div(
          class = "flex-grow-1",
          style = "min-width: 240px;",
          shiny::textInput("dir", "Folder", width = "100%")
        ),
        shinyFiles::shinyDirButton(
          "dir_choose",
          "Browse\u2026",
          "Choose a directory of ride files",
          buttonType = "outline-secondary"
        ),
        shiny::actionButton(
          "scan",
          "List the files",
          class = "btn-outline-secondary"
        )
      ),
      shiny::helpText(
        "Browsing lists the folder at once. A typed path needs the button. ",
        "Nothing is opened until step 2."
      ),
      shiny::uiOutput("scan_status"),
      shiny::div(
        class = "mt-3 pt-3 border-top",
        shiny::p(
          class = "small text-body-secondary mb-2",
          "No rides to hand? Load three simulated meters with known faults."
        ),
        shiny::div(
          class = "actions",
          bslib::input_task_button(
            "demo",
            "Load demo rides",
            label_busy = "Building demo\u2026",
            type = "outline-secondary"
          )
        )
      )
    ),
    bslib::card(
      bslib::card_header(
        shiny::span(class = "step-number", "2"),
        "Check the labels, then load"
      ),
      guidance(
        "Fix any wrong labels, remove files that do not belong, then load. Two ",
        "files with the same run and source are refused, because they would be ",
        "averaged into one meter without warning."
      ),
      shiny::uiOutput("demo_note"),
      shiny::uiOutput("spec_issues"),
      # Hidden until step 1 has listed something. An empty DT still reserves
      # several hundred pixels for a table it has no rows for, and a Load
      # button under it reads as an action that has been tried and failed
      # rather than one waiting on the step above.
      shiny::conditionalPanel(
        condition = "output.has_spec === true",

        # The separator sits with the table it fills rather than beside the
        # folder box: it is the character that splits a filename into run and
        # source, so its consequence is two columns below it, and changing it
        # re-parses the names on the spot.
        shiny::div(
          class = "d-flex gap-3 align-items-end flex-wrap",
          shiny::div(
            style = "width: 110px;",
            shiny::textInput("sep", "Separator", value = "_", width = "100%")
          ),
          shiny::p(
            class = "small text-body-secondary mb-3",
            style = "max-width: 52ch;",
            "The character that splits a filename into run and source, the ",
            shiny::tags$code("_"),
            " in ",
            shiny::tags$code("run1_kickr.fit"),
            ". Changing it re-reads both columns."
          )
        ),
        shiny::p(
          class = "small text-body-secondary mb-2",
          style = "max-width: 68ch;",
          "Double-click a run or source to edit it. Select rows and press ",
          "Remove selected to leave files out. Listing the folder again ",
          "discards edits."
        ),
        DT::DTOutput("spec"),
        shiny::div(
          class = "actions mt-3",
          shiny::actionButton(
            "remove_rows",
            "Remove selected",
            class = "btn-outline-secondary"
          )
        ),
        shiny::div(
          class = "actions mt-3 pt-3 border-top",
          bslib::input_task_button(
            "load",
            "Load these rides",
            label_busy = "Reading files\u2026"
          )
        ),
        shiny::div(class = "mt-3", shiny::uiOutput("load_status"))
      )
    )
  )
}

#' @noRd
ui_alignment <- function() {
  bslib::nav_panel(
    "Alignment",
    shiny::uiOutput("align_headline"),
    shiny::uiOutput("align_alert"),
    shiny::uiOutput("edge_alert"),
    bslib::card(
      bslib::card_header("Power traces"),
      full_screen = TRUE,
      guidance(
        "Traces from the same ride should share a shape, even with a vertical ",
        "gap between them."
      ),
      shiny::plotOutput("timeseries", height = "auto")
    ),
    full_table(
      shiny::tableOutput("align_table"),
      shiny::uiOutput("align_provenance")
    ),
    details(
      "How the offsets were found",
      shiny::p(
        "Two meters on the same ride can disagree about the time. Differencing ",
        "on their timestamps then compares power from different moments, which ",
        "inflates the spread and shifts the bias while the result still looks ",
        "plausible. Each source is matched against a reference by ",
        "cross-correlating the power traces."
      ),
      shiny::p(
        shiny::tags$code("offset"),
        " is the correction applied, in seconds. ",
        shiny::tags$code("correlation"),
        " is how well the traces matched there, and ",
        shiny::tags$code("sharpness"),
        " how far that peak stood above the rest. ",
        shiny::tags$code("status"),
        " is ",
        shiny::tags$code("aligned"),
        " when the offset ",
        "was established, ",
        shiny::tags$code("assumed"),
        " for the reference ",
        "itself, and ",
        shiny::tags$code("unresolved"),
        " when the data could not ",
        "settle it, and the recorded timestamps stand."
      )
    )
  )
}

#' @noRd
ui_agreement <- function() {
  bslib::nav_panel(
    "Agreement",
    shiny::uiOutput("headline"),
    shiny::uiOutput("chain_notes"),
    # Both full width, stacked rather than side by side: the table needs about
    # 910px for its six columns and the faceted plot at least as much again.
    # The table's own width: auto stops it stretching across a wide monitor.
    bslib::card(
      bslib::card_header("Every pair"),
      shiny::uiOutput("pair_table"),
      shiny::uiOutput("provenance")
    ),
    bslib::card(
      bslib::card_header("Bland-Altman"),
      full_screen = TRUE,
      guidance(
        "Each cell counts seconds: mean of the two meters across, difference ",
        "up. A flat cloud above or below zero is a constant offset; a tilt ",
        "means the gap changes with effort. Chained pairs have no shared ",
        "seconds, so they do not appear."
      ),
      shiny::uiOutput("bridge_ui"),
      shiny::plotOutput("agreement_plot", height = "auto")
    ),
    details(
      "How to read these numbers",
      shiny::p(
        "Gain is the difference as a percentage, because meters are usually ",
        "out by a few per cent, not a fixed number of watts. The limits of ",
        "agreement bound where most individual seconds fall. They describe ",
        "scatter. The interval beside each estimate gives the uncertainty in ",
        "the average."
      ),
      shiny::p(
        shiny::tags$b("Where each number came from. "),
        "Most pairs rode together, and their row names the run. A pair that ",
        "never rode together is linked through a bridge meter present in ",
        "both runs: each run gives a gain relative to the bridge, and the ",
        "difference between those gains is the comparison. Such a row is ",
        "marked ",
        shiny::tags$em("chained"),
        " and rests on the bridge having read ",
        "the same on both days, which nothing in the ride files can test. It ",
        "has no watts and no limits of agreement, because there is no second ",
        "the two meters both recorded."
      ),
      shiny::p(
        shiny::tags$b("Why the intervals are wide. "),
        "They come from a moving-block bootstrap rather than from treating ",
        "each second as independent evidence. What you are riding now is ",
        "nearly what you were riding a second ago, so an hour carries far ",
        "less independent information than its 3,600 rows suggest, and an ",
        "interval built on independence would be roughly an order of ",
        "magnitude too narrow."
      ),
      shiny::p(
        "A cloud fanning out to the right on the Bland-Altman plot is ",
        "ordinary: a fixed percentage error is more watts at more watts."
      )
    )
  )
}

#' @noRd
ui_levels <- function() {
  bslib::nav_panel(
    "By level",
    shiny::uiOutput("level_headline"),
    shiny::uiOutput("level_alert"),
    shiny::uiOutput("thin_alert"),
    guidance(
      "One average can hide a meter that agrees at endurance pace and drifts ",
      "at threshold."
    ),
    # Two views of one computation rather than a toggle: a constant percentage
    # error is a rising line in watts and a flat line in per cent, so the two
    # have to be readable side by side.
    bslib::navset_card_tab(
      full_screen = TRUE,
      title = "Across the power range",
      bslib::nav_panel(
        "In watts",
        shiny::plotOutput("by_level_plot", height = "auto")
      ),
      bslib::nav_panel(
        "In per cent",
        shiny::plotOutput("by_level_pc_plot", height = "auto")
      ),
      bslib::nav_panel(
        "The numbers",
        DT::DTOutput("by_level_table"),
        shiny::uiOutput("by_level_note")
      )
    ),
    details(
      "How to read this",
      shiny::p(
        "Seconds are binned by the ",
        shiny::tags$i("mean"),
        " of the two meters, ",
        "not by one nominated meter: binning on one and plotting its own ",
        "difference against that bin turns measurement noise into an ",
        "apparent slope."
      ),
      shiny::p(
        "A meter that is a constant percentage out reads further from its ",
        "partner the harder you ride, so its line rises in watts while ",
        "nothing about the meter has changed. The per-cent panel is flat for ",
        "that meter and tilts only for one whose error depends on how hard ",
        "you were riding."
      )
    )
  )
}

#' @noRd
ui_report <- function() {
  bslib::nav_panel(
    "Report",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      bslib::card(
        bslib::card_header("Save what is on screen"),
        guidance(
          "Renders the results from the other tabs to a standalone HTML file. ",
          "Nothing is recomputed."
        ),
        shiny::uiOutput("report_ui")
      ),
      bslib::card(
        bslib::card_header("What goes into it"),
        shiny::uiOutput("report_summary")
      )
    )
  )
}
