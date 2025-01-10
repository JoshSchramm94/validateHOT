df <- validateHOT::acbc_interpolate

test_that("Structure of df", {
  expect_equal(nrow(df), 110)
  expect_equal(ncol(df), 39)
})
