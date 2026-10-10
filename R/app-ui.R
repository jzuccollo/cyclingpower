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
      width = 280,
      title = "Analysis settings",

      shiny::div(
        class = "sidebar-note",
        "One set of knobs for the whole app: every number on Alignment, ",
        "Agreement and By level, and everything in the saved report, is ",
        "computed with these. Nothing here changes what is recorded or which ",
        "files are loaded. Start with the defaults, then move one to see ",
        "whether a conclusion survives the change."
      ),

      sidebar_heading("Comparison", "Agreement, By level and the report."),
      shiny::sliderInput("min_power", "Minimum power (W)", 0, 400, 0, step = 5),
      shiny::helpText(
        "Seconds where either meter reads below this are dropped. Coasting is ",
        "always dropped: two meters both reading zero agree trivially."
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
        "Intervals are resampled, so they move a little each time. If a ",
        "conclusion changes when you reseed, there are too few replicates ",
        "behind it."
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
      "Pair each meter to this page once and one clock stamps every sample, so ",
      "the recordings share a timebase by construction rather than having one ",
      "recovered from them afterwards. The files land in a directory the Rides ",
      "tab reads like any other."
    ),
    shiny::uiOutput("ble_status"),

    # Nothing below is drawn without Web Bluetooth: on Safari the pair button
    # would open no chooser and say nothing about why.
    shiny::conditionalPanel(
      condition = "input.ble_supported === true",

      bslib::card(
        bslib::card_header("The meters"),
        guidance(
          "The browser asks which device to use each time and will not open ",
          "that chooser by itself, so pairing three meters is three presses."
        ),
        shiny::div(
          class = "alert alert-warning",
          shiny::strong(
            "A meter can hold one Bluetooth connection at a time. "
          ),
          "While this page has the trainer, Zwift and TrainerRoad cannot: ",
          "pair the training app to it over ANT+ instead. That is also how erg ",
          "targets have to be commanded during a recording. Check the pairing ",
          "rather than assume it \u2014 a head unit will often take a trainer over ",
          "Bluetooth by default and quietly spend the connection this page ",
          "needs."
        ),
        # Wrapped, because a card body is a flex column and a bare button in
        # one stretches the full width of the card.
        shiny::div(
          class = "actions mb-3",
          shiny::actionButton(
            "pair_meter",
            "Pair a meter",
            class = "btn-primary"
          )
        ),
        # Painted by record.js from the connection state it already holds.
        # Round-tripping live watts through R to redraw this would put the
        # recorder's responsiveness behind the analysis tabs' work.
        shiny::div(id = "meter_status"),
        shiny::uiOutput("meter_labels")
      ),

      bslib::card(
        bslib::card_header("The recording"),
        shiny::div(
          class = "alert alert-warning",
          shiny::strong("Zero-offset every meter now. "),
          "After a few minutes of easy pedalling and immediately before you ",
          "start, following each manufacturer's instructions. The whole ",
          "comparison rests on it, and it cannot be done afterwards."
        ),
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
          "Files are named ",
          shiny::tags$code("<run>_<source>.csv"),
          ", which is what the Rides tab parses, so both labels take letters, ",
          "digits and hyphens only. An existing file is never written over."
        ),
        shiny::div(
          class = "actions mb-3",
          shiny::actionButton(
            "start_record",
            "Start recording",
            class = "btn-primary"
          ),
          shiny::actionButton(
            "stop_record",
            "Stop",
            class = "btn-outline-secondary"
          )
        ),
        shiny::uiOutput("record_status")
      ),

      bslib::card(
        bslib::card_header("Live trace"),
        guidance(
          "The last three minutes, drawn in the browser from the samples as ",
          "they arrive. It is a check that the meters are tracking each ",
          "other's shape, not the analysis \u2014 that comes off the files."
        ),
        shiny::tags$canvas(id = "record_trace")
      ),

      bslib::card(
        bslib::card_header("What to ride"),
        guidance(
          "Each block is here for something the analysis needs. Ride the same ",
          "session every time: a chained comparison rests on the bridge meter ",
          "reading the same in both runs."
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
          " writes the surges and coasts as free ride, which drops the trainer ",
          "out of erg for exactly the blocks that must not be ridden in it. An ",
          shiny::tags$code(".erg"),
          " course has no way to say that, so switch out of erg by hand for ",
          "them. Either way the targets have to be commanded over ANT+: this ",
          "page holds the trainer's Bluetooth."
        ),
        details(
          "Why these blocks",
          shiny::p(
            shiny::tags$b("The surges are not there to find the offset. "),
            "One clock stamping every meter makes it zero by construction. ",
            "But with no feature to lock onto, ",
            shiny::tags$code("check_alignment()"),
            " cannot tell that the clocks agree from that the traces cannot ",
            "be compared at all: it collapses both into ",
            shiny::tags$code("unresolved"),
            ", forces the offset to zero and warns. A recorder that stamped ",
            "every meter wrongly would pass that check exactly as a correct ",
            "one does. The surges are what make a resolved zero evidence ",
            "rather than a default."
          ),
          shiny::p(
            shiny::tags$b("The ladder runs up and then back down. "),
            "A meter reading high because it has warmed up gives different ",
            "numbers at the same target on the way down; a ",
            "level-dependent one repeats itself. Ridden in one direction only ",
            "the two are indistinguishable, and ",
            shiny::tags$code("level_dependence()"),
            " would report the drift as an error that depends on power."
          ),
          shiny::p(
            shiny::tags$b("Two minutes a step is a floor. "),
            "What runs out is not seconds but bootstrap blocks. Up and down ",
            "land in the same bands, so a band carries about twice the step, ",
            "and at a 60-second block two-minute steps leave it roughly four ",
            "blocks to resample. Shorter drops under three, and a bootstrap ",
            "drawing two blocks is not estimating anything. Going the other ",
            "way to four minutes buys about 17% narrower bands for twice the ",
            "ladder."
          ),
          shiny::p(
            shiny::tags$b("Erg makes the trainer the controlled variable, "),
            "holding its own reading at target, so within a step the trainer ",
            "is nearly flat and almost all the spread comes from the other ",
            "meter. That biases nothing \u2014 the level is the mean of the two \u2014 ",
            "but on the By level tab it is the trainer that decides which ",
            "band a second lands in."
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
        "The unit of analysis is a run: one session, ridden once, recorded by ",
        "two or more meters. Files named ",
        shiny::tags$code("<run><sep><source>"),
        " label themselves \u2014 ",
        shiny::tags$code("run1_kickr.fit"),
        " is the kickr's recording of run one. Anything else you label by ",
        "hand at step 2."
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
        "Browsing lists the folder straight away; typing a path needs the ",
        "button. Listing only reads the filenames \u2014 nothing is opened until ",
        "step 2."
      ),
      shiny::uiOutput("scan_status"),
      shiny::div(
        class = "mt-3 pt-3 border-top",
        shiny::p(
          class = "small text-body-secondary mb-2",
          "No rides of your own to hand? The demo builds three simulated ",
          "meters with known faults and loads them in one press."
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
        "These are the files found in the folder, not yet the data: nothing ",
        "has been read from disk. Correct anything mislabelled, drop what ",
        "does not belong, and load. Two files must never carry the same run ",
        "and source \u2014 they would be averaged into a single meter with nothing ",
        "in the output to say it had happened, so that combination is refused ",
        "rather than warned about."
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
            "The character between run and source in the filenames: the ",
            shiny::tags$code("_"),
            " of ",
            shiny::tags$code("run1_kickr.fit"),
            ". Change it and the two columns are read again."
          )
        ),
        shiny::p(
          class = "small text-body-secondary mb-2",
          style = "max-width: 68ch;",
          "Double-click a run or source cell to edit it. Select rows and use ",
          "Remove selected to leave a file out of the comparison entirely. ",
          "Listing the folder again replaces the table, discarding these edits."
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
        "The visual check that these are recordings of the same ride: traces ",
        "should track each other's shape, whatever vertical gap sits between ",
        "them."
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
        "Two meters recording the same ride do not necessarily agree about ",
        "what time it is, and differencing on nominal timestamps then ",
        "compares power from one moment against power from another. That ",
        "inflates the spread and shifts the bias, and the result looks ",
        "entirely plausible on the way out. Each source is matched against a ",
        "reference by cross-correlating the power traces."
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
        "settle it \u2014 in which case the recorded timestamps stand."
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
        "Each cell counts the seconds spent there: the two meters' mean ",
        "across, their difference up. Flat above or below zero is a ",
        "constant offset; a tilt means the gap changes with effort. Only ",
        "pairs that rode together appear here, because a chained ",
        "comparison has no shared seconds to plot."
      ),
      shiny::uiOutput("bridge_ui"),
      shiny::plotOutput("agreement_plot", height = "auto")
    ),
    details(
      "How to read these numbers",
      shiny::p(
        "Gain is the difference as a percentage, which is usually the more ",
        "natural way to read a meter, since meters tend to be a few per cent ",
        "out rather than a fixed number of watts out. The limits of ",
        "agreement bound the range within which most individual seconds ",
        "fall \u2014 they describe the scatter, not the uncertainty in the ",
        "average, which is what the interval beside each estimate gives."
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
        "ordinary \u2014 a fixed percentage error is more watts at more watts."
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
      "A single average hides the pattern that usually matters: meters that ",
      "agree at endurance pace and drift at threshold. A meter reading high ",
      "in one half of the range and low in the other can post an overall ",
      "bias near zero and still be wrong everywhere."
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
          "Renders the results shown in the other tabs to a standalone HTML ",
          "file, with the same explanation alongside them. Nothing is ",
          "recomputed, so the document cannot disagree with the screen."
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
