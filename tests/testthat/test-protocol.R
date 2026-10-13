test_that("the protocol is the session the vignette specifies", {
  protocol <- comparison_protocol()

  expect_equal(
    sum(protocol$seconds),
    30 + 4 * (8 + 20) + 5 * 120 + 60 + 5 * 120 + 30
  )
  expect_equal(sum(protocol$seconds[protocol$mode == "erg"]), 10 * 120)
})

test_that("the ladder carries about four bootstrap blocks per band", {
  # The floor on step length, and the reason it is not min_seconds. Up and down
  # land in the same bands, so a band holds twice the step; at 60-second blocks
  # anything under three is resampling too few distinct blocks to mean much.
  protocol <- comparison_protocol()
  step <- unique(protocol$seconds[protocol$mode == "erg"])

  expect_length(step, 1)
  expect_gte(2 * step / 60, 3)
})

test_that("there is no closing sync block", {
  # It would only detect a clock drifting mid-ride, and one clock stamping every
  # meter cannot drift against itself.
  protocol <- comparison_protocol()

  expect_equal(sum(grepl("Sync", unique(protocol$block))), 1)
  expect_equal(protocol$block[1], "Zero and settle")
  expect_equal(protocol$block[nrow(protocol)], "Stop together")
})

test_that("shorter steps shorten only the ladder", {
  full <- comparison_protocol(step_seconds = 240)
  half <- comparison_protocol(step_seconds = 120)

  expect_equal(
    sum(half$seconds),
    sum(full$seconds) - 10 * 120
  )
  expect_equal(
    half$seconds[half$mode == "free"],
    full$seconds[full$mode == "free"]
  )
})

test_that("the ladder covers the same targets in both directions", {
  protocol <- comparison_protocol()
  up <- protocol$target_pc[protocol$block == "Ladder up"]
  down <- protocol$target_pc[protocol$block == "Ladder down"]

  expect_equal(up, rev(down))
  expect_false(is.unsorted(up))
})

test_that("the sync blocks are ridden out of erg", {
  # A trainer in erg fights the surge and rounds off the coast-to-power edge
  # check_alignment() needs to resolve rather than default, so this is the one
  # property of the protocol that cannot be relaxed.
  protocol <- comparison_protocol()
  sync <- protocol[grepl("Sync", protocol$block), ]

  expect_true(all(sync$mode == "free"))
  expect_equal(sum(protocol$mode == "erg"), 10)
})

test_that("each sync block gives the requested coast-to-power edges", {
  protocol <- comparison_protocol(surges = 6)
  sync <- protocol[protocol$block == "Sync surges", ]

  # The block opens on a surge because what precedes it already ends coasting,
  # so every surge in the block has a coast in front of it.
  before <- c(0, utils::head(sync$target_pc, -1))
  expect_equal(sum(sync$target_pc > 0 & before == 0), 6)
})

test_that("a zwo releases erg for exactly the free blocks", {
  path <- withr::local_tempfile(fileext = ".zwo")
  protocol <- comparison_protocol()
  write_workout(path, ftp = 250, protocol = protocol)

  doc <- xml2::read_xml(path)
  intervals <- xml2::xml_children(xml2::xml_find_first(doc, "workout"))

  expect_equal(
    xml2::xml_name(intervals),
    ifelse(protocol$mode == "erg", "SteadyState", "FreeRide")
  )
  expect_equal(
    as.numeric(xml2::xml_attr(intervals, "Duration")),
    protocol$seconds
  )
})

test_that("zwo targets are the protocol's fractions of FTP", {
  path <- withr::local_tempfile(fileext = ".zwo")
  write_workout(path, ftp = 300)

  doc <- xml2::read_xml(path)
  steady <- xml2::xml_find_all(doc, "workout/SteadyState")

  expect_equal(
    as.numeric(xml2::xml_attr(steady, "Power")),
    c(0.55, 0.70, 0.85, 1.00, 1.10, 1.10, 1.00, 0.85, 0.70, 0.55)
  )
})

test_that("an erg course is a square wave in absolute watts", {
  path <- withr::local_tempfile(fileext = ".erg")
  protocol <- comparison_protocol()
  write_workout(path, ftp = 200, protocol = protocol)

  lines <- readLines(path)
  data <- lines[
    (which(lines == "[COURSE DATA]") + 1):(which(lines == "[END COURSE DATA]") -
      1)
  ]
  course <- utils::read.delim(text = data, header = FALSE)

  expect_equal(nrow(course), 2 * nrow(protocol))
  expect_false(is.unsorted(course$V1))
  # The file carries minutes to four decimals, so a whole second is well inside
  # the tolerance and there is nothing to be gained from writing more of them.
  expect_equal(max(course$V1), sum(protocol$seconds) / 60, tolerance = 1e-4)

  # Each block is written as two rows at one wattage, so the target holds flat
  # across the block rather than ramping into the next one.
  expect_equal(course$V2[c(TRUE, FALSE)], course$V2[c(FALSE, TRUE)])
  expect_equal(course$V2[c(TRUE, FALSE)], round(200 * protocol$target_pc))
})

test_that("an unknown extension is refused rather than guessed at", {
  expect_error(
    write_workout(withr::local_tempfile(fileext = ".fit"), ftp = 250),
    "No workout format"
  )
})
