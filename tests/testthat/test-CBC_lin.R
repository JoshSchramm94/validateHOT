df <- validateHOT::cbc_linear

test_that("Structure of df", {
  expect_equal(nrow(df), 105)
  expect_equal(ncol(df), 36)
})
