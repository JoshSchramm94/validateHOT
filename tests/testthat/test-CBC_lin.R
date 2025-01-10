data("cbc_linear")

test_that("Structure of df", {
  expect_equal(nrow(cbc_linear), 105)
  expect_equal(ncol(cbc_linear), 36)
})
