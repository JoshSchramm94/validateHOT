df <- validateHOT::maxdiff

test_that("Structure of df", {
  expect_equal(nrow(df), 118)
  expect_equal(ncol(df), 20)
})
