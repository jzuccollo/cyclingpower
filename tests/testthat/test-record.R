# The recorder is tested in two halves, and one half is not tested at all.
#
# The packet parser and the file writing are both deterministic and are tested
# properly below: bytes in, a known number out; samples in, a file that reads
# back through the same pipeline every other ride goes through.
#
# The radio path — requestDevice, the GATT connection, notifications, the
# reconnection loop, the wake lock — is NOT tested and cannot be. It needs a
# physical meter, a paired browser and a user gesture, none of which exist in
# testthat or in CI. That is a real departure from this suite's discipline of
# planting a known distortion and asserting the machinery recovers it, and it
# is stated here rather than papered over, because the rest of the package's
# credibility rests on that discipline being visible where it holds and where
# it does not. What is left is a ride the recorder produced, checked by hand
# against the head units it replaced.

# ---- The packet parser --------------------------------------------------

# Run under V8 rather than reimplemented in R. A second implementation would
# test itself, not the parser the browser actually runs, which is the one that
# can be wrong.
parser <- function() {
  skip_if_not_installed("V8")
  ctx <- V8::v8()
  ctx$source(system.file(
    "shiny",
    "www",
    "power-packet.js",
    package = "cyclingpower"
  ))
  ctx
}

# Bytes to a parsed packet, through a DataView built in JavaScript so the
# parser sees exactly what a characteristic hands it.
parse_bytes <- function(ctx, bytes) {
  ctx$assign(
    "parsed",
    V8::JS(sprintf(
      "parseCyclingPower(new DataView(new Uint8Array([%s]).buffer))",
      paste(bytes, collapse = ", ")
    ))
  )
  ctx$get("parsed")
}

test_that("power is read from the mandatory field whatever the flags say", {
  ctx <- parser()

  # Flags 0x0000: nothing optional present. 250 W is 0xFA 0x00 little-endian.
  bare <- parse_bytes(ctx, c(0x00, 0x00, 0xFA, 0x00))
  expect_equal(bare$power, 250)
  expect_equal(bare$length, 4)

  # Flags 0x0020: crank revolution data present. Power is still at offset 2,
  # because both mandatory fields sit ahead of every optional one.
  with_crank <- parse_bytes(
    ctx,
    c(0x20, 0x00, 0xFA, 0x00, 0x10, 0x00, 0x00, 0x08)
  )
  expect_equal(with_crank$power, 250)
})

test_that("power is signed", {
  ctx <- parser()

  # -5 W: 0xFB 0xFF. Read as unsigned this is 65,531, which would arrive
  # downstream as a spike three orders of magnitude above the ride.
  parsed <- parse_bytes(ctx, c(0x00, 0x00, 0xFB, 0xFF))
  expect_equal(parsed$power, -5)
})

test_that("the flags place every optional field", {
  ctx <- parser()

  # Balance (1 byte) and crank revolutions (4) present: offsets 4 and 5, and a
  # packet nine bytes long.
  parsed <- parse_bytes(
    ctx,
    c(0x21, 0x00, 0xC8, 0x00, 0x32, 0x10, 0x00, 0x00, 0x08)
  )
  expect_equal(parsed$offsets$pedalPowerBalance, 4)
  expect_equal(parsed$offsets$crankRevolutionData, 5)
  expect_null(parsed$offsets$accumulatedTorque)
  expect_equal(parsed$length, 9)

  # Wheel revolutions cost six bytes and push the crank fields along by them.
  wheeled <- parse_bytes(
    ctx,
    c(
      0x30,
      0x00,
      0xC8,
      0x00,
      0x01,
      0x00,
      0x00,
      0x00,
      0x00,
      0x08,
      0x10,
      0x00,
      0x00,
      0x08
    )
  )
  expect_equal(wheeled$offsets$wheelRevolutionData, 4)
  expect_equal(wheeled$offsets$crankRevolutionData, 10)
  expect_equal(wheeled$length, 14)

  # The bits that qualify another field rather than announcing one of their own
  # must shift nothing. 0x1000 is the offset compensation indicator.
  indicated <- parse_bytes(ctx, c(0x00, 0x10, 0xC8, 0x00))
  expect_true(indicated$indicators$offsetCompensationIndicator)
  expect_equal(indicated$length, 4)
})

test_that("a sequence of packets parses to the power that was in them", {
  ctx <- parser()

  planted <- c(0, 1, 199, 250, 1000, 2000)
  read_back <- vapply(
    planted,
    function(watts) {
      packet <- c(0x00, 0x00, watts %% 256, watts %/% 256)
      parse_bytes(ctx, packet)$power
    },
    numeric(1)
  )

  expect_equal(read_back, planted)
})

# ---- File custody -------------------------------------------------------

test_that("a recording reads back through the ordinary pipeline", {
  dir <- withr::local_tempdir()

  recording <- start_recording(dir, "run1", c("kickr", "assioma"))
  recording <- record_samples(
    recording,
    data.frame(
      source = rep(c("kickr", "assioma"), each = 3),
      time = rep(
        c(
          "2026-03-01T09:00:00.000Z",
          "2026-03-01T09:00:01.000Z",
          "2026-03-01T09:00:02.000Z"
        ),
        times = 2
      ),
      power = c(200, 205, 210, 195, 199, 204)
    )
  )
  written <- stop_recording(recording)

  expect_equal(written$rows, c(3L, 3L))
  expect_equal(basename(written$path), c("run1_kickr.csv", "run1_assioma.csv"))

  # The filenames carry the labels, which is the whole handoff: nothing is
  # passed between the recorder and the analysis but the files.
  spec <- ride_spec_from_names(written$path)
  expect_equal(spec$run, c("run1", "run1"))
  expect_equal(spec$source, c("kickr", "assioma"))

  rides <- read_rides(spec)
  expect_equal(nrow(rides), 6)
  expect_s3_class(rides$time, "POSIXct")
  expect_equal(rides$power[rides$source == "kickr"], c(200, 205, 210))
})

test_that("sub-second stamps survive and land in the right second", {
  dir <- withr::local_tempdir()

  # What a 4 Hz meter produces. The stamps must survive to the millisecond:
  # rounded to whole seconds by the recorder instead, all five readings would
  # be rows claiming the same instant, and the two in the second half of the
  # second belong to the second after it.
  recording <- start_recording(dir, "run1", c("fast", "slow"))
  recording <- record_samples(
    recording,
    data.frame(
      source = c(rep("fast", 4), "slow"),
      time = c(
        "2026-03-01T09:00:00.000Z",
        "2026-03-01T09:00:00.200Z",
        "2026-03-01T09:00:00.600Z",
        "2026-03-01T09:00:00.800Z",
        "2026-03-01T09:00:00.000Z"
      ),
      power = c(180, 200, 220, 240, 190)
    )
  )
  written <- stop_recording(recording)

  rides <- read_rides(ride_spec_from_names(written$path))
  fast <- rides$time[rides$source == "fast"]
  expect_equal(
    as.numeric(fast - fast[[1]], units = "secs"),
    c(0, 0.2, 0.6, 0.8),
    tolerance = 1e-6
  )

  # align_rides() rounds onto its one-second grid and averages the duplicates,
  # so the four arrive as two seconds each holding their mean. That is the grid
  # doing its job, and it can only do it on stamps the recorder did not round
  # away first.
  aligned <- align_rides(rides, max_offset = 0)
  fast_grid <- aligned[aligned$source == "fast", ]
  expect_equal(fast_grid$t, c(0, 1))
  expect_equal(fast_grid$power, c(mean(c(180, 200)), mean(c(220, 240))))
})

test_that("samples are on disk before the recording is closed", {
  dir <- withr::local_tempdir()

  recording <- start_recording(dir, "run1", "kickr")
  recording <- record_samples(
    recording,
    data.frame(
      source = "kickr",
      time = "2026-03-01T09:00:00.000Z",
      power = 200
    )
  )

  # Read while the connection is still open, which is the case a killed browser
  # tab leaves behind. Held in memory and written at the end, this file would
  # be empty and the ride would be gone.
  path <- file.path(dir, "run1_kickr.csv")
  expect_equal(readLines(path), c("time,power", "2026-03-01T09:00:00.000Z,200"))

  stop_recording(recording)
})

test_that("a dropout leaves a gap rather than a run of zeros", {
  dir <- withr::local_tempdir()

  # Two meters, one of which stops sending for ten seconds in the middle. Only
  # samples that arrived are written, so the gap is absent rather than zero.
  seconds <- 0:29
  stamps <- sprintf("2026-03-01T09:00:%02d.000Z", seconds)
  present <- !(seconds %in% 10:19)

  recording <- start_recording(dir, "run1", c("kickr", "assioma"))
  recording <- record_samples(
    recording,
    data.frame(
      source = c(rep("kickr", 30), rep("assioma", sum(present))),
      time = c(stamps, stamps[present]),
      power = c(rep(200, 30), rep(195, sum(present)))
    )
  )
  written <- stop_recording(recording)

  expect_equal(written$rows, c(30L, 20L))

  aligned <- read_rides(ride_spec_from_names(written$path)) |>
    align_rides(max_offset = 0)

  # NA over the dropout, not zero. Zeros would be dropped as coasting, so a
  # lost connection would enter the comparison as ten seconds of agreement.
  missing <- aligned$power[aligned$source == "assioma" & aligned$t %in% 10:19]
  expect_true(all(is.na(missing)))
  expect_equal(sum(!is.na(aligned$power[aligned$source == "assioma"])), 20)

  # And those seconds reach no comparison at all.
  paired <- cyclingpower:::paired_seconds(aligned, min_power = 0)
  expect_false(any(paired$t %in% 10:19))
  expect_equal(nrow(paired), 20)
})

test_that("labels that would not survive the filename are refused", {
  dir <- withr::local_tempdir()

  # ride_spec_from_names() splits on the underscore and demands exactly two
  # parts, so a label carrying one produces files the scan refuses wholesale.
  expect_error(start_recording(dir, "run_1", "kickr"), "run")
  expect_error(start_recording(dir, "run1", "left_pedal"), "source")
  expect_error(start_recording(dir, "run1", ""), "source")
  expect_error(
    start_recording(dir, "run1", c("kickr", "kickr")),
    "one file"
  )

  # Nothing was created by any of the refusals.
  expect_equal(length(list.files(dir)), 0)
})

test_that("an existing recording is never written over", {
  dir <- withr::local_tempdir()

  first <- start_recording(dir, "run1", "kickr")
  stop_recording(first)

  # Appending a second session to the first would read as one ride with an
  # enormous gap through the middle and nothing to say otherwise.
  expect_error(start_recording(dir, "run1", "kickr"), "already exist")
})
