paired_ratio <- function(aligned) {
  aligned |>
    dplyr::select("t", "source", "power") |>
    tidyr::pivot_wider(names_from = "source", values_from = "power") |>
    dplyr::filter(!is.na(.data$A), !is.na(.data$B), .data$A > 0) |>
    dplyr::mutate(ratio = .data$B / .data$A)
}

test_that("a known clock offset is recovered", {
  rides <- as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), lag = 7)
  ))

  report <- align_rides(rides) |> alignment_report()

  expect_equal(report$offset[report$source == "B"], 7)
  expect_equal(report$offset[report$source == "A"], 0)
  expect_gt(report$correlation[report$source == "B"], 0.99)
})

test_that("offsets in either direction are recovered", {
  for (lag in c(-11, -3, 4, 19)) {
    rides <- as_rides(list(
      A = fixture_ride(),
      B = make_source(fixture_ride(), lag = lag)
    ))
    report <- align_rides(rides) |> alignment_report()
    expect_equal(report$offset[report$source == "B"], lag)
  }
})

test_that("alignment puts the sources back into register", {
  rides <- as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), gain = 1.03, lag = 7)
  ))

  ratio <- paired_ratio(align_rides(rides))$ratio

  expect_equal(mean(ratio), 1.03)
  expect_lt(stats::sd(ratio), 1e-9)
})

test_that("skipping alignment reproduces the error it exists to prevent", {
  rides <- as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), gain = 1.03, lag = 7)
  ))

  # max_offset = 0 trusts the recorded timestamps, which is what the package
  # used to do unconditionally. The true gain is 1.03.
  ratio <- paired_ratio(align_rides(rides, max_offset = 0))$ratio

  expect_gt(abs(mean(ratio) - 1.03), 0.02)
  expect_gt(stats::sd(ratio), 0.1)
})

test_that("unrelated rides are flagged, not silently compared", {
  b <- fixture_ride()
  b$power <- withr::with_seed(42, stats::runif(nrow(b), 0, 400))

  rides <- as_rides(list(A = fixture_ride(), B = b))

  expect_warning(aligned <- align_rides(rides), "Could not establish")
  report <- alignment_report(aligned)
  expect_equal(report$status[report$source == "B"], "unresolved")
  expect_lt(report$correlation[report$source == "B"], 0.5)
})

test_that("a featureless ride falls back to timestamps rather than erroring", {
  # A steady effort is the normal case on a trainer, so this must not block.
  expect_warning(
    aligned <- align_rides(featureless_rides()),
    "steady effort with no features"
  )
  report <- alignment_report(aligned)
  expect_equal(report$offset, c(0, 0))
  expect_equal(report$status[report$source == "B"], "unresolved")
})

test_that("a well-identified offset is reported as aligned", {
  rides <- as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), lag = 5, noise = 10)
  ))

  report <- align_rides(rides) |> alignment_report()

  expect_equal(report$status[report$source == "B"], "aligned")
  expect_equal(report$status[report$source == "A"], "assumed")
})

test_that("duplicate timestamps are averaged, not fanned out", {
  ride <- fixture_ride()[1:100, ]
  doubled <- dplyr::bind_rows(ride, ride)

  aligned <- as_rides(list(A = doubled, B = doubled)) |>
    align_rides(max_offset = 0)

  expect_equal(nrow(aligned), 2 * 100)
  expect_equal(dplyr::n_distinct(aligned$t), 100)
})

test_that("pauses stay as gaps rather than being interpolated over", {
  ride <- fixture_ride()[1:200, ]
  paused <- ride[-(50:100), ]

  aligned <- as_rides(list(A = ride, B = paused)) |>
    align_rides(max_offset = 0)

  gap <- aligned |>
    dplyr::filter(.data$source == "B", .data$t %in% 49:99)

  expect_true(all(is.na(gap$power)))
})

test_that("jittered once-a-second stamps leave no empty seconds, but a dropout stays", {
  # A meter notifying every 0.99 s drifts across the second boundaries, so
  # rounding alone leaves empty seconds. Planted: constant power, one real
  # 10-second hole.
  at <- cumsum(rep(0.99, 400))
  at <- at[!(at > 200 & at < 210)]
  origin <- as.POSIXct("2026-01-01", tz = "UTC")
  x <- tibble::tibble(
    time = origin + at,
    run = "r",
    source = "s",
    power = 200
  )

  out <- resample_1hz(x, origin = origin)

  expect_equal(out$power[out$t < 195], rep(200, sum(out$t < 195)))
  expect_true(any(is.na(out$power[out$t > 201 & out$t < 209])))
})
