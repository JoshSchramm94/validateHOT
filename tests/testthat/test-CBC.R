data(cbc)

test_that("Structure of df", {
  expect_equal(nrow(cbc), 105)
  expect_equal(ncol(cbc), 41)
})
