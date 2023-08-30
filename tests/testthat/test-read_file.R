test_that("Read FIT file", {
  test_data <- read_file("power1.fit")

  expect_s3_class(test_data, "tbl_df")
  expect_that(dim(test_data), equals(c(3601, 9)))
  expect_that(
    purrr::reduce(purrr::map(test_data, ~ sum(is.na(.))), `+`),
    equals(0)
  )
})
