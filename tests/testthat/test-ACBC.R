library(validateHOT)
data(ACBC)

test_that("Structure of DF", {
  expect_equal(nrow(ACBC), 95)
  expect_equal(ncol(ACBC), 39)
})

test_that("Variables numeric", {
  names <- colnames(
    ACBC
  )[-(which(
    colnames(ACBC) == "Group"
  ))]
  for (i in seq_along(names)) {
    expect_true(!is.character(ACBC[names[i]]))
  }
})

test_that("No missings", {
  expect_false(anyNA(ACBC))
})
