test_that("Coggan calculations", {
  test_data <- read_file("power1.fit")
  ftp <- 275

  expect_lt(abs(average_power(test_data$power) - 189), 1)
  expect_lt(abs(normalised_power(test_data$power) - 210), 1)
  expect_lt(abs(intensity_factor(test_data$power, ftp) - 0.76), 0.01)
  expect_lt(abs(tss(test_data$power, ftp) - 58), 1)
  expect_lt(abs(variability_index(test_data$power) - 1.11), 0.01)
})
