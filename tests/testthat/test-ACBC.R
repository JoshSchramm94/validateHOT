data(acbc)

test_that("Structure of df", {
  expect_equal(nrow(acbc), 110)
  expect_equal(ncol(acbc), 37)
})
