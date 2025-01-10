data(maxdiff)

test_that("Structure of df", {
  expect_equal(nrow(maxdiff), 118)
  expect_equal(ncol(maxdiff), 20)
})
