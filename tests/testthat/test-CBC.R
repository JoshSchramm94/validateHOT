library(validateHOT)
data(CBC)

test_that("Structure of DF", {
  expect_equal(nrow(CBC), 79)
  expect_equal(ncol(CBC), 23)
})

test_that("Variables numeric", {
  names <- colnames(CBC)[-(which(colnames(CBC) == "Group"))]
  for (i in seq_along(names)) {
    expect_true(!is.character(CBC[names[i]]))
  }
})

test_that("No missings", {
  expect_false(anyNA(CBC))
})
