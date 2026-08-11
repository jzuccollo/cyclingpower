# The demo files are documentation as much as data: the app's guidance text
# describes what they show. These assert the planted distortions are still
# recoverable, so the demo cannot quietly stop demonstrating anything.

demo_aligned <- function() {
  demo_rides(tempfile()) |>
    list.files(full.names = TRUE) |>
    ride_spec_from_names() |>
    read_rides() |>
    align_rides()
}

test_that("demo files describe two runs of three meters bridged by assioma", {
  spec <- demo_rides(tempfile()) |>
    list.files(full.names = TRUE) |>
    ride_spec_from_names()

  expect_setequal(spec$run, c("run1", "run2"))
  expect_setequal(spec$source, c("assioma", "kickr", "quarq"))

  aligned <- demo_aligned()
  expect_equal(bridge_candidates(aligned), "assioma")
})

test_that("the planted clock offset is recovered", {
  report <- alignment_report(demo_aligned())

  expect_equal(report$offset[report$source == "kickr"], 7)
  expect_equal(report$status[report$source == "kickr"], "aligned")
})

test_that("kickr reads the planted 2.5 per cent below assioma", {
  agreement <- withr::with_seed(
    1,
    power_agreement(demo_aligned(), replicates = 200)
  )
  kickr <- agreement[agreement$source_b == "kickr", ]

  # assioma / kickr, so the gain is 1 / 0.975 - 1.
  expect_equal(kickr$gain_pc, 100 * (1 / 0.975 - 1), tolerance = 0.1)
  expect_lt(kickr$gain_lo, kickr$gain_pc)
  expect_gt(kickr$gain_hi, kickr$gain_pc)
})

test_that("quarq's level-dependent error hides inside its overall bias", {
  aligned <- demo_aligned()

  overall <- withr::with_seed(1, power_agreement(aligned, replicates = 200))
  quarq <- overall[overall$source_b == "quarq", ]

  # The point of the demo: averaging a positive and a negative region leaves an
  # overall figure that looks like near-perfect agreement.
  expect_lt(abs(quarq$gain_pc), 1.5)

  by_level <- withr::with_seed(
    1,
    agreement_by_level(aligned, width = 25, replicates = 200)
  )
  bands <- by_level[by_level$source_b == "quarq", ]

  low <- bands$gain_pc[bands$mean_power < 150]
  high <- bands$gain_pc[bands$mean_power > 250]

  expect_true(all(low > 1))
  expect_true(all(high < -1))
})

test_that("kickr against quarq is chained rather than observed", {
  chained <- withr::with_seed(
    1,
    chain_sources(demo_aligned(), replicates = 200)
  )
  linked <- chained[chained$source_a == "kickr" & chained$source_b == "quarq", ]

  expect_equal(linked$basis, "chained")
  expect_equal(linked$via, "assioma")
})
