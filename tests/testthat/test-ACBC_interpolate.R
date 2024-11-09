library(validateHOT)
data("ACBC_interpolate")

test_that("Structure of DF", {
  expect_equal(nrow(ACBC_interpolate), 95)
  expect_equal(ncol(ACBC_interpolate), 41)
})

test_that("Variables numeric", {
  names <- colnames(
    ACBC_interpolate
  )[-(which(
    colnames(ACBC_interpolate) == "Group"
  ))]
  for (i in seq_along(names)) {
    expect_true(!is.character(ACBC_interpolate[names[i]]))
  }
})

test_that("No missings", {
  expect_false(anyNA(ACBC_interpolate))
})
