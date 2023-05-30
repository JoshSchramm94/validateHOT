library(ValiDatHOT)
data(ACBC)

test_that("Structure of DF", {
  expect_equal(base::nrow(ACBC), 80)
  expect_equal(base::ncol(ACBC), 34)
})

test_that("Variables numeric",{
  names <- base::colnames(ACBC)[-(base::which(base::colnames(ACBC)=="Group"))]
  for (i in 1:length(names)){
    expect_true(!base::is.character(ACBC[names[i]]))
  }
})

test_that("No missings",{
  expect_false(base::anyNA(ACBC))
})