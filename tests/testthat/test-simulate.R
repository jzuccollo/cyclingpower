# test the simulate function
test_that("simulate_power works", {
  dummy_power <- simulate_power(200, 180, 200)
  expect_equal(normalised_power(dummy_power), 200)
  expect_equal(average_power(dummy_power), 180)
  expect_equal(length(dummy_power), 200)
  expect_error(simulate_power(200, 1, 1000))
})
