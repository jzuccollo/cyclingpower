# The print methods exist so the first thing a reader sees is the finding
# rather than thirteen columns. What is asserted here is that the sentence
# agrees with the tibble it sits above -- a headline that contradicts the
# numbers underneath would be worse than no headline at all.

demo_aligned <- function() {
  read_rides(demo_rides()) |> align_rides()
}

test_that("the headline direction matches the sign of the gain", {
  # demo_rides() plants kickr at 0.975 of assioma, so assioma reads high.
  out <- withr::with_seed(1, power_agreement(demo_aligned()))
  kickr <- out[out$source_b == "kickr", ]

  expect_gt(kickr$gain_pc, 0)
  expect_match(
    capture.output(print(kickr))[[1]],
    "assioma reads 2\\.\\d% higher than kickr"
  )
})

test_that("a negative gain turns the interval to face the same way", {
  # Reversing the pair reverses the gain, and the interval has to follow or
  # the sentence reads "2.6% lower (95% CI 2.4 to 2.7)" with the bounds on the
  # opposite side of zero from the estimate.
  out <- withr::with_seed(1, power_agreement(demo_aligned()))
  row <- out[out$source_b == "kickr", ]
  row$gain_pc <- -row$gain_pc
  flipped <- c(-row$gain_hi, -row$gain_lo)
  row$gain_lo <- flipped[[1]]
  row$gain_hi <- flipped[[2]]

  line <- capture.output(print(row))[[1]]
  expect_match(line, "lower than kickr")

  # Both bounds printed positive, bracketing the printed estimate.
  bounds <- as.numeric(regmatches(
    line,
    gregexpr("-?\\d+\\.\\d", line)
  )[[1]])
  expect_true(all(bounds > 0))
})

test_that("a chained pair carries its assumption into the print", {
  chained <- withr::with_seed(1, chain_sources(demo_aligned()))
  expect_true(any(chained$basis == "chained"))

  printed <- paste(capture.output(print(chained)), collapse = " ")
  expect_match(printed, "chained via assioma")
  expect_match(printed, "assumes the bridge read identically on both days")
})

test_that("a comparison with nothing chained does not claim otherwise", {
  # The same two meters in both runs: A is the bridge, but every pair already
  # rode together, so nothing is routed through it and the assumption does not
  # apply. It must not be printed anyway.
  ride <- read_file(system.file(
    "extdata",
    "power_recorded.csv",
    package = "cyclingpower"
  ))
  both_runs <- withr::with_seed(1, {
    dplyr::bind_rows(
      as_rides(
        list(A = ride, B = make_source(ride, gain = 1.02, noise = 3)),
        run = "run1"
      ),
      as_rides(
        list(A = ride, B = make_source(ride, gain = 1.02, noise = 3, seed = 2)),
        run = "run2"
      )
    )
  })
  direct <- withr::with_seed(
    1,
    chain_sources(align_rides(both_runs), bridge = "A")
  )

  printed <- paste(capture.output(print(direct)), collapse = " ")
  expect_false(any(direct$basis == "chained"))
  expect_no_match(printed, "assumes the bridge")
})

test_that("printing leaves the object usable", {
  out <- withr::with_seed(1, power_agreement(demo_aligned()))
  expect_s3_class(out, "tbl_df")
  expect_equal(nrow(dplyr::filter(out, .data$run == "run1")), 1)
})
