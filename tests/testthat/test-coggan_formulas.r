library(cyclingpoweR)


test_that("Coggan calculations", {
  test_data <- read_file(system.file("inst", "extdata", "power1.fit", package = "cyclingpoweR"))
  ftp <- 275

  expect_lt(abs(average_power(test_data) - 189), 1)
  expect_lt(abs(normalised_power(test_data) - 210), 1)
  expect_lt(abs(intensity_factor(test_data, ftp) - 0.76), 0.01)
  expect_lt(abs(tss(test_data, ftp) - 58), 1)
  expect_lt(abs(variability_index(test_data) - 1.11), 0.01)
})
