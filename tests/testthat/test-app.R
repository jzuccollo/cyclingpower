# The app moved into R/ so that this file could exist. testServer() runs the
# real server function against the real reactive graph -- no browser, but no
# mock either: what is exercised here is the code the app runs.
#
# The radio path is still untested and still cannot be tested. See
# test-record.R.

test_that("scanning a directory finds the rides and labels them", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")

  dir <- demo_rides()

  shiny::testServer(app_server, {
    session$setInputs(dir = dir, sep = "_", scan = 1)

    expect_null(files()$error)
    expect_length(files()$paths, 4)
    expect_setequal(spec_rv()$source, c("assioma", "kickr", "quarq"))
    expect_setequal(spec_rv()$run, c("run1", "run2"))
  })
})

test_that("a directory with no rides in it says so rather than failing", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")

  empty <- withr::local_tempdir()

  shiny::testServer(app_server, {
    session$setInputs(dir = empty, sep = "_", scan = 1)
    expect_match(files()$error, "No .fit, .gpx or .csv files")

    session$setInputs(dir = file.path(empty, "nope"), scan = 2)
    expect_match(files()$error, "No directory at")
  })
})

test_that("loading runs the pipeline and the agreement reaches the screen", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("shinyFiles")

  dir <- demo_rides()

  shiny::testServer(app_server, {
    # The sidebar defaults, which the UI would supply. settings() is debounced
    # by 700 ms, so the clock has to be advanced past it or nothing downstream
    # of the sidebar ever fires.
    session$setInputs(
      min_power = 0,
      block = 60,
      replicates = 500,
      seed = 1,
      max_offset = 30,
      smooth = 30,
      width = 25,
      min_seconds = 30
    )
    session$setInputs(dir = dir, sep = "_", scan = 1)
    session$setInputs(load = 1)
    session$elapse(1000)

    expect_setequal(unique(rides()$source), c("assioma", "kickr", "quarq"))

    # The planted distortions come back through the app exactly as they do
    # through the package: a 7 s offset on kickr, and a 2.5% gain under it.
    expect_true(7 %in% alignment_report(aligned())$offset)

    pair <- agreement()[agreement()$source_b == "kickr", ]
    expect_equal(pair$gain_pc, 2.56, tolerance = 0.1)
  })
})

test_that("the interface builds and serves the recorder's scripts", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")
  skip_if_not_installed("shinyFiles")

  head <- as.character(htmltools::renderTags(app_ui())$head)

  # The scripts are addressed through the resource path launch_app() registers.
  # Left as bare filenames they resolve only when the app is run from a
  # directory, so this would 404 with the Record tab still rendering and the
  # Pair button silently doing nothing.
  expect_match(head, "cyclingpower/power-packet.js", fixed = TRUE)
  expect_match(head, "cyclingpower/record.js", fixed = TRUE)
})

test_that("the recorder's ids are not namespaced", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("bslib")

  # record.js addresses these four directly: #pair_meter and #stop_record in
  # its capture-phase click listener, #meter_status and #record_trace when it
  # paints. A module namespace would rename all four and break pairing and the
  # ordered stop, with nothing failing on the R side.
  html <- as.character(htmltools::renderTags(ui_record())$html)

  for (id in c("pair_meter", "stop_record", "meter_status", "record_trace")) {
    expect_match(html, paste0("\"", id, "\""), fixed = TRUE)
  }
})
