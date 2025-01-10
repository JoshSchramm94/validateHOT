hot <- create_hot(
  data = maxdiff,
  none = "none",
  id = "id",
  prod.levels = list(2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17),
  method = "maxdiff",
  choice = "hot"
)
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if opts is missing ", {
  expect_error(turf(
    data = hot,
    # opts = c(option_1:option_16),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if none is missing ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    # none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if size is missing ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    # size = 3L,
    approach = "thres"
  ))
})

test_that("Error if approach is missing ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = 3L,
    # approach = "thres"
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------

test_that("Error if none is part of opts", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:none),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if none contains NA ", {
  hot$none[34] <- NA

  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if none is not numeric ", {
  hot$none <- as.character(hot$none)

  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if size is not numeric", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = "3L",
    approach = "thres"
  ))
})

test_that("Error if size is larger than opts ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_3),
    none = none,
    size = 4L,
    approach = "thres"
  ))
})

test_that("Error if opts contains NA ", {
  hot$option_1[34] <- NA

  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if opts is not numeric ", {
  hot$option_2 <- as.character(hot$option_2)

  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if fixed is not part of opts ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_15),
    fixed = "option_16",
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if prohib is not a list ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    prohib = "option_16",
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if prohib is not part of opts ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_15),
    prohib = list("option_16"),
    none = none,
    size = 3L,
    approach = "thres"
  ))
})

test_that("Error if approach is wrong ", {
  expect_error(turf(
    data = hot,
    opts = c(option_1:option_16),
    none = none,
    size = 3L,
    approach = "test"
  ))
})

# end --------------------------------------------------------------------------

# other checks -----------------------------------------------------------------

test_that("length of output should equal number of possible combinations ", {
  expect_equal(
    nrow(turf(
      data = hot, opts = c(option_1:option_16),
      none = none,
      size = 3L,
      approach = "thres"
    )),
    nrow(as.data.frame(
      t(
        combn(
          length(hot %>%
            dplyr::select(option_1:option_16)), 3
        )
      )
    ))
  )
})

test_that("Number of rows should be reduced accordingly if fixed is used ", {
  expect_equal(
    nrow(turf(
      data = hot, opts = c(option_1:option_16),
      none = none,
      fixed = c("option_1", "option_16"),
      size = 4L,
      approach = "thres"
    )),
    nrow(as.data.frame(
      t(
        combn(
          (length(hot %>%
            dplyr::select(option_1:option_16)) - 2), 2
        )
      )
    ))
  )
})

test_that("Check whether fixed item is working ", {
  t1 <- turf(
    data = hot, opts = c(option_1:option_16),
    none = none,
    fixed = c("option_1", "option_16"),
    size = 4L,
    approach = "thres"
  )

  expect_true(all(t1$option_1 == 1 & t1$option_16 == 1))
})

test_that("turf() also working with Likert scale ", {
  set.seed(2023)

  df <- data.frame(
    option_1 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_2 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_3 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_4 = round(runif(10000, min = 1, max = 5), digits = 0),
    option_5 = round(runif(10000, min = 1, max = 5), digits = 0),
    thres = rep(2.99, 10000)
  )

  t1 <- turf(
    data = df,
    opts = c(option_1:option_5),
    none = thres,
    size = 3,
    approach = "thres"
  )

  expect_true(nrow(t1) > 1)
})
# end --------------------------------------------------------------------------
