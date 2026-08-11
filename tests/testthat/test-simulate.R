test_that("both targets are hit exactly", {
  ride <- withr::with_seed(
    1,
    simulate_power(3600, target_ap = 180, target_np = 220)
  )

  expect_length(ride, 3600)
  expect_equal(average_power(ride), 180)
  expect_equal(normalised_power(ride), 220)
  expect_true(all(ride > 0))
})

test_that("targets are hit across a range of demands and lengths", {
  for (target_np in c(180, 200, 240, 300)) {
    ride <- withr::with_seed(2, simulate_power(1200, 180, target_np))
    expect_equal(average_power(ride), 180)
    expect_equal(normalised_power(ride), target_np)
  }
})

test_that("equal targets give a steady ride", {
  ride <- withr::with_seed(1, simulate_power(600, 200, 200))

  expect_equal(variability_index(ride), 1)
  expect_equal(stats::sd(ride), 0)
})

test_that("normalised power below average power is refused", {
  expect_error(
    simulate_power(600, target_ap = 200, target_np = 180),
    "cannot be below average power"
  )
})

test_that("smoothness changes the character but not the statistics", {
  punchy <- withr::with_seed(
    3,
    simulate_power(1800, 180, 220, smoothness = 0.9)
  )
  sustained <- withr::with_seed(
    3,
    simulate_power(1800, 180, 220, smoothness = 0.995)
  )

  expect_equal(average_power(punchy), 180)
  expect_equal(average_power(sustained), 180)
  expect_equal(normalised_power(punchy), 220)
  expect_equal(normalised_power(sustained), 220)

  # A sustained ride crosses its own average far less often than a punchy one.
  crossings <- function(x) sum(diff(x > mean(x)) != 0)
  expect_lt(crossings(sustained), crossings(punchy))
})

test_that("the result is reproducible under a seed and varies without one", {
  first <- withr::with_seed(7, simulate_power(600, 180, 210))
  again <- withr::with_seed(7, simulate_power(600, 180, 210))
  other <- withr::with_seed(8, simulate_power(600, 180, 210))

  expect_equal(first, again)
  expect_false(isTRUE(all.equal(first, other)))
})

test_that("the simulated ride is autocorrelated, as real riding is", {
  ride <- withr::with_seed(1, simulate_power(3600, 180, 220))

  # Independent draws would sit near zero here, and could not have produced a
  # normalised power above the average in the first place.
  expect_gt(stats::cor(ride[-1], ride[-length(ride)]), 0.9)
})
