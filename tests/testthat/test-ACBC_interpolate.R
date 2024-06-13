library(validateHOT)
data("ACBC_interpolate")

test_that("Structure of DF", {
  expect_equal(base::nrow(ACBC_interpolate), 95)
  expect_equal(base::ncol(ACBC_interpolate), 41)
})

test_that("Variables numeric", {
  names <- base::colnames(ACBC_interpolate)[-(base::which(base::colnames(ACBC_interpolate) == "Group"))]
  for (i in base::seq_along(names)) {
    expect_true(!base::is.character(ACBC_interpolate[names[i]]))
  }
})

test_that("No missings", {
  expect_false(base::anyNA(ACBC_interpolate))
})
