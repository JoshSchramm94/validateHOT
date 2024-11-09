library(validateHOT)
data(CBC_lin)

test_that("Structure of DF", {
  expect_equal(nrow(CBC_lin), 79)
  expect_equal(ncol(CBC_lin), 17)
})

test_that("Variables numeric", {
  names <- colnames(CBC_lin)[-(which(
    colnames(CBC_lin) == "Group"
  ))]
  for (i in seq_along(names)) {
    expect_true(!is.character(CBC_lin[names[i]]))
  }
})

test_that("No missings", {
  expect_false(anyNA(CBC_lin))
})
