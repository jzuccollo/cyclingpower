# The smallest test that catches a knob the report quietly ignores. width and
# min_seconds and smooth all determine what a saved document says, and the
# document outlives the session that made it, so a parameter that stops short
# of compare_all() is a report disagreeing with the screen that produced it.

test_that("every setting reaches the results it governs", {
  aligned <- align_rides(as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), gain = 1.03, noise = 4)
  )))

  results <- withr::with_seed(
    1,
    compare_all(
      aligned,
      min_power = 50,
      width = 50,
      min_seconds = 200,
      block = 30,
      replicates = 200,
      seed = 1,
      max_offset = 45,
      smooth = 15
    )
  )

  expect_true(all(results$by_level$seconds >= 200))
  expect_equal(unique(diff(sort(unique(results$by_level$band)))), 50)

  expect_equal(
    results$settings,
    list(
      min_power = 50,
      width = 50,
      min_seconds = 200,
      block = 30,
      replicates = 200,
      seed = 1,
      max_offset = 45,
      smooth = 15
    )
  )
})

test_that("rendering writes nothing into the installed package", {
  # knitr writes comparison.knit.md and the figures beside whichever copy of the
  # template it is handed. Handed the installed one, that is a write into the
  # package's own directory, which anyone whose library is read-only cannot do.
  skip_if_not_installed("rmarkdown")
  skip_if_not(rmarkdown::pandoc_available())

  ride <- fixture_ride()[1:600, ]
  aligned <- align_rides(as_rides(list(
    A = ride,
    B = make_source(ride, gain = 1.03, noise = 4)
  )))
  results <- withr::with_seed(1, compare_all(aligned, replicates = 50))

  installed <- system.file("rmarkdown", package = "cyclingpower")
  before <- list.files(installed, recursive = TRUE)

  out <- file.path(withr::local_tempdir(), "report.html")
  render_comparison(results, out)

  expect_true(file.exists(out))
  expect_equal(list.files(installed, recursive = TRUE), before)
})

test_that("min_seconds defaults the same way through compare_all", {
  aligned <- align_rides(as_rides(list(
    A = fixture_ride(),
    B = make_source(fixture_ride(), gain = 1.03, noise = 4)
  )))

  direct <- withr::with_seed(1, agreement_by_level(aligned, replicates = 100))
  through <- withr::with_seed(1, compare_all(aligned, replicates = 100))

  expect_equal(nrow(through$by_level), nrow(direct))
  expect_equal(through$by_level$band, direct$band)
})
