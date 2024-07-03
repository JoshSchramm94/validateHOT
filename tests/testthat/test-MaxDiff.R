library(validateHOT)
data(MaxDiff)

test_that("Structure of DF", {
  expect_equal(base::nrow(MaxDiff), 70)
  expect_equal(base::ncol(MaxDiff), 21)
})

test_that("Variables numeric", {
  names <- base::colnames(MaxDiff)[-(base::which(base::colnames(MaxDiff) == "Group"))]
  for (i in base::seq_along(names)) {
    expect_true(!base::is.character(MaxDiff[names[i]]))
  }
})

test_that("No missings", {
  expect_false(base::anyNA(MaxDiff))
})
