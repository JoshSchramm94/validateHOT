HOT <- createHOT(
  data = MaxDiff,
  none = 19,
  id = 1,
  prod.levels = list(3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18),
  choice = 20,
  method = "MaxDiff"
)

# examples
test_that("Example 1 should work ", {
  expect_no_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))
})

test_that("Example 2 should work ", {
  expect_no_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 4, fixed = c("Option_1", "Option_5"), approach = "thres"))
})


# None
test_that("Error if none is missing", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), size = 3, approach = "thres"))
})

test_that("Error if none is part of opts", {
  expect_error(turf(data = HOT, opts = c(Option_1:None), none = None, size = 3, approach = "thres"))
})

test_that("Error if none contains NA ", {
  HOT2 <- HOT

  HOT2$None[34] <- NA

  expect_error(turf(data = HOT2, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))
})

test_that("Error if none is not numeric ", {
  HOT2 <- HOT

  HOT2$None <- as.character(HOT2$None)

  expect_error(turf(data = HOT2, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))
})

# Size
test_that("Error if size is not numeric", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = "3", approach = "thres"))
})

test_that("Error if size is decimal", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3.14, approach = "thres"))
})

test_that("Error if size is missing", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, approach = "thres"))
})


test_that("Error if size is larger than length of opts", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), size = (as.integer(length(HOT %>% dplyr::select(Option_1:Option_16))) + 1), none = None, approach = "thres"))
})

# opts
test_that("Error if opts is missing", {
  expect_error(turf(data = HOT, size = 3, none = None, approach = "thres"))
})

test_that("Error if opts is missing", {
  expect_error(turf(data = HOT, size = 3, none = None, approach = "thres"))
})


test_that("Error if opts contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(turf(data = HOT2, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))
})


test_that("Error if opts is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- as.character(HOT2$Option_2)

  expect_error(turf(data = HOT2, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))
})


# Output
test_that("Data Frame as output ", {
  expect_true(is.data.frame(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres")))
})

test_that("length of output should equal number of possible combinations ", {
  expect_equal(
    nrow(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres")),
    nrow(as.data.frame(t(utils::combn(length(HOT %>% select(Option_1:Option_16)), 3))))
  )
})

test_that("Number of colums of output should be equal to number of alternatives plus 3 ", {
  expect_equal(
    ncol(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres")),
    (length(HOT %>% dplyr::select(Option_1:Option_16)) + 3)
  )
})

test_that("All options should have the same names in output ", {
  # change names just for robustness testing
  HOT2 <- HOT

  colnames(HOT2)[which(colnames(HOT2) == "Option_1"):(which(colnames(HOT2) == "Option_16"))] <- paste0("alt_", c(1:16))

  # store item names
  item.names <- colnames(HOT2)[which(colnames(HOT2) == "alt_1"):(which(colnames(HOT2) == "alt_16"))]

  expect_true(all(item.names %in% colnames(turf(data = HOT2, opts = c(alt_1:alt_16), none = None, size = 3, approach = "thres"))))
})

test_that("Number of rows should be reduced accordingly if fixed is used ", {
  expect_equal(
    nrow(turf(data = HOT, opts = c(Option_1:Option_16), fixed = c("Option_1", "Option_16"), none = None, size = 4, approach = "thres")),
    nrow(as.data.frame(t(utils::combn((length(HOT %>% dplyr::select(Option_1:Option_16)) - 2), 2))))
  )
})

test_that("Check whether fixed workds - all combinations must have a 1 for fixed ", {
  t1 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, fixed = "Option_4", approach = "thres")

  expect_true(all(t1$Option_4 == 1))
})

test_that("Check whether fixed workds - all combinations must have a 1 for fixed (also works with column index) ", {
  t1 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, fixed = 5, approach = "thres")

  expect_true(all(t1$Option_4 == 1))
})

test_that("Fixed needs to be part of opts ", {
  HOT2 <- HOT %>%
    mutate(Option_18 = 1)

  expect_error(turf(data = HOT2, opts = c(Option_1:Option_16), none = None, size = 3, fixed = "Option_18", approach = "thres"))
})

test_that("First three columns are set ", {
  expect_equal(
    c(colnames(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))[1:3]),
    c("combo", "reach", "freq")
  )
})

test_that("Check results - Reach never larger than 100, freq never larger than size ", {
  t1 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 2, approach = "thres")

  expect_true(max(t1$reach) <= 100)
  expect_true(max(t1$freq) <= 2)
})

test_that("Check data type of output ", {
  t1 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 2, approach = "thres")

  # combo == character
  expect_true(is.character(t1[, 1]))

  # reach == numeric
  expect_true(is.numeric(t1[, 2]))

  # freq == numeric
  expect_true(is.numeric(t1[, 3]))

  vars <- HOT %>%
    dplyr::select(Option_1:Option_16) %>%
    colnames()

  for (i in vars) {
    expect_true(is.numeric(t1[[i]]))
  }
})

test_that("Fixed can not be larger than size ", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_3), none = None, size = 2, fixed = c(2:4), approach = "thres"))
})

test_that("Fixed can not be larger than opts ", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_3), none = None, size = 4, fixed = c(2:5), approach = "thres"))
})


# approach
test_that("'approach' has to be 'thres' or 'fc' ", {
  expect_no_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres"))
  expect_no_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "fc"))
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "abc"))
})

test_that("'approach' can not be empty ", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3))
})

test_that("'fc' can not be larger than 'thres' in terms of statistics ", {
  t1 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "thres")

  t2 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "fc")

  expect_true(all(t1$reach >= t2$reach))

  expect_true(all(t1$freq >= t2$freq))
})

test_that("if 'fc' 'reach' and 'freq' ", {
  t2 <- turf(data = HOT, opts = c(Option_1:Option_16), none = None, size = 3, approach = "fc")

  expect_true(all(round(t2$reach, digits = 3) == round((t2$freq * 100), digits = 3)))
})

# other data format is working as well

test_that("turf also working with other data ", {
  df <- data.frame(
    Option_1 = stats::rnorm(10000, mean = 0, sd = 1),
    Option_2 = stats::rnorm(10000, mean = 0, sd = 1),
    Option_3 = stats::rnorm(10000, mean = 0, sd = 1),
    Option_4 = stats::rnorm(10000, mean = 0, sd = 1),
    Option_5 = stats::rnorm(10000, mean = 0, sd = 1),
    thres = rep(0, 10000)
  )

  expect_no_error(turf(data = df, opts = c(Option_1:Option_5), none = thres, size = 3, approach = "thres"))
  expect_no_error(turf(data = df, opts = c(Option_1:Option_5), none = thres, size = 3, approach = "fc"))
})

test_that("turf also working with Likert scale ", {
  df <- data.frame(
    Option_1 = round(stats::runif(10000, min = 1, max = 5), digits = 0),
    Option_2 = round(stats::runif(10000, min = 1, max = 5), digits = 0),
    Option_3 = round(stats::runif(10000, min = 1, max = 5), digits = 0),
    Option_4 = round(stats::runif(10000, min = 1, max = 5), digits = 0),
    Option_5 = round(stats::runif(10000, min = 1, max = 5), digits = 0),
    thres = rep(2.99, 10000)
  )

  expect_no_error(turf(data = df, opts = c(Option_1:Option_5), none = thres, size = 3, approach = "thres"))
  expect_no_error(turf(data = df, opts = c(Option_1:Option_5), none = thres, size = 3, approach = "fc"))
})


# prohib
test_that("Error if prohib is not part of opts ", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), size = 3, none = None, prohib = list(c("Option_xy")), approach = "thres"))
})

test_that("Error if prohib is not a list ", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), size = 3, none = None, prohib = c("Option_2", "Option_4", "Option_7", "Option_8"), approach = "thres"))
})

test_that("Error if prohib is  a list ", {
  expect_no_error(turf(data = HOT, opts = c(Option_1:Option_16), size = 3, none = None, prohib = list(c("Option_2", "Option_4", "Option_7")), approach = "thres"))
})

test_that("Error if all of 'prohib' in 'fixed' ", {
  expect_error(turf(data = HOT, opts = c(Option_1:Option_16), size = 3, none = None, fixed = c("Option_2", "Option_4"), prohib = list(c("Option_2", "Option_4")), approach = "thres"))
})

test_that("No Error if all of 'fixed' are in 'prohib', as long as size is different ", {
  expect_no_error(turf(data = HOT, opts = c(Option_1:Option_16), size = 3, none = None, fixed = c("Option_2", "Option_4"), prohib = list(c("Option_2", "Option_4", "Option_5")), approach = "thres"))
})
