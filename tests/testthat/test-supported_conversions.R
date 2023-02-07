test_that("supported_conversions() works", {
  expect_error(supported_conversions(), NA)
  expect_s3_class(supported_conversions(), "data.frame")
})
