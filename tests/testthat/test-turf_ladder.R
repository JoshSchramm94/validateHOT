# check for error messages for missing arguments -------------------------------
test_that("Error if opts is missing ", {
  expect_error(turf_ladder(
    data = maxdiff,
    # opts = c(option_01:option_16),
    none = none,
    approach = "thres"
  ))
})

test_that("Error if none is missing ", {
  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    # none = none,
    approach = "thres"
  ))
})

test_that("Error if approach is missing ", {
  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    # approach = "thres"
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------

test_that("Error if none is part of opts", {
  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:none),
    none = none,
    approach = "thres"
  ))
})

test_that("Error if none contains NA ", {
  maxdiff$none[34] <- NA

  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    approach = "thres"
  ))
})

test_that("Error if none is not numeric ", {
  maxdiff$none <- as.character(maxdiff$none)

  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    approach = "thres"
  ))
})


test_that("Error if opts contains NA ", {
  maxdiff$option_01[34] <- NA

  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    approach = "thres"
  ))
})

test_that("Error if opts is not numeric ", {
  maxdiff$option_02 <- as.character(maxdiff$option_02)

  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    approach = "thres"
  ))
})

test_that("Error if fixed is not part of opts ", {
  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_15),
    fixed = "option_16",
    none = none,
    approach = "thres"
  ))
})

test_that("Error if approach is wrong ", {
  expect_error(turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    approach = "test"
  ))
})

# end --------------------------------------------------------------------------

# other checks -----------------------------------------------------------------

test_that("Check whether fixed item is working ", {
  t1 <- turf_ladder(
    data = maxdiff,
    opts = c(option_01:option_16),
    none = none,
    fixed = c("option_01", "option_16"),
    approach = "thres"
  )

  expect_true(all(t1$option_01 == 1 & t1$option_16 == 1))
})

test_that("turf_ladder() also working with Likert scale ", {
  set.seed(2023)

  df <- data.frame(
    option_1 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_2 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_3 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_4 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_5 = round(runif(10000, min = 1, max = 5), digits = 0),
    thres = rep(2.99, 10000)
  )

  t1 <- turf_ladder(
    data = df,
    opts = c(option_1:option_5),
    none = thres,
    approach = "thres"
  )

  expect_true(nrow(t1) > 1)
})
# end --------------------------------------------------------------------------
