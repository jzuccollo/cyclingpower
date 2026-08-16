test_that("a steady effort has normalised power equal to its average", {
  steady <- rep(250, 600)

  expect_equal(normalised_power(steady), 250)
  expect_equal(average_power(steady), 250)
  expect_equal(variability_index(steady), 1)
})

test_that("an hour at threshold scores 100", {
  # The definition TSS is calibrated against, so this pins the whole chain.
  hour <- rep(275, 3600)

  expect_equal(intensity_factor(hour, ftp = 275), 1)
  expect_equal(tss(hour, ftp = 275), 100)
  expect_equal(tss(rep(275, 1800), ftp = 275), 50)
})

test_that("coasting counts towards average power", {
  # Half an hour pedalling at 200 W then half an hour freewheeling. Counting
  # the zeros is what the standard definition does, and what variability index
  # and TSS are defined against.
  half_and_half <- c(rep(200, 1800), rep(0, 1800))

  expect_equal(average_power(half_and_half), 100)
  expect_equal(average_power(drop_zeros(half_and_half)), 200)

  # Normalised power weights the hard half more heavily, so it lands between
  # the overall average and the power actually ridden.
  expect_gt(normalised_power(half_and_half), 100)
  expect_lt(normalised_power(half_and_half), 200)
  expect_gt(variability_index(half_and_half), 1)
})

test_that("normalised power exceeds average power on a variable ride", {
  for (ride in list(
    c(rep(100, 300), rep(300, 300)),
    c(rep(0, 300), rep(250, 300)),
    rep(c(rep(150, 60), rep(350, 60)), 5)
  )) {
    expect_gt(normalised_power(ride), average_power(ride))
  }
})

test_that("second-by-second noise alone does not raise normalised power", {
  # The 30-second window averages independent noise away, so a jittery ride
  # around a steady average has a variability index of essentially 1. It is
  # sustained changes in effort, not sample-to-sample scatter, that NP rewards.
  jittery <- withr::with_seed(1, stats::rpois(3600, 200))

  expect_equal(variability_index(jittery), 1, tolerance = 1e-3)
})

test_that("the rolling window trails and needs 30 seconds to fill", {
  expect_true(is.na(normalised_power(rep(250, 29))))
  expect_equal(normalised_power(rep(250, 30)), 250)

  # Trailing rather than centred: a ramp's window looks back, so the mean of
  # the first full window is the mean of seconds 1 to 30.
  ramp <- seq_len(30)
  expect_equal(normalised_power(ramp), mean(ramp))
})

test_that("Coggan metrics on the reference ride", {
  ride <- fixture_ride()
  ftp <- 275

  expect_equal(average_power(ride$power), 188.68, tolerance = 1e-3)
  expect_equal(normalised_power(ride$power), 210.60, tolerance = 1e-3)
  expect_equal(intensity_factor(ride$power, ftp), 0.7658, tolerance = 1e-3)
  expect_equal(tss(ride$power, ftp), 58.66, tolerance = 1e-3)
  expect_equal(variability_index(ride$power), 1.1162, tolerance = 1e-3)
})

test_that("the default FTP is the one the rest of the package assumes", {
  # 250 W is what the app's input, the protocol examples and the recording
  # vignette all use. An hour at threshold scores 100 by definition, so this
  # both pins the default and checks it is wired through intensity_factor().
  expect_equal(tss(rep(250, 3600)), 100)
  expect_equal(intensity_factor(rep(250, 600)), 1)

  # And it is only a default.
  expect_equal(tss(rep(300, 3600), ftp = 300), 100)
})
