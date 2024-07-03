library(validateHOT)
data(CBC)

test_that("Structure of DF", {
  expect_equal(base::nrow(CBC), 79)
  expect_equal(base::ncol(CBC), 23)
})

test_that("Variables numeric", {
  names <- base::colnames(CBC)[-(base::which(base::colnames(CBC) == "Group"))]
  for (i in base::seq_along(names)) {
    expect_true(!base::is.character(CBC[names[i]]))
  }
})

test_that("No missings", {
  expect_false(base::anyNA(CBC))
})
