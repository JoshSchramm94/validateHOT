data("acbc_interpolate")

test_that("Structure of df", {
  expect_equal(nrow(acbc_interpolate), 110)
  expect_equal(ncol(acbc_interpolate), 39)
})
