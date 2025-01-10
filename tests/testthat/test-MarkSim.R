# create data for tests
hot <- create_hot(
  data = maxdiff,
  id = "id",
  none = "none",
  prod.levels = list(2, 9, 10, 14, 15, 16, 17),
  method = "maxdiff",
  varskeep = "group",
  choice = "hot"
)
# end --------------------------------------------------------------------------

# check for error messages for missing arguments -------------------------------
test_that("Error if opts is missing ", {
  expect_error(marksim(
    data = hot,
    # opts = c(option_1:none),
    method = "sop",
    res = "agg"
  ))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if opts has just length 1 ", {
  expect_error(marksim(
    data = hot,
    opts = option_1,
    method = "sop",
    res = "agg"
  ))
})

test_that("Error if opts contains NA ", {
  hot$option_2[34] <- NA

  expect_error(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop",
    res = "agg"
  ))
})

test_that("Error if opts is not numeric ", {
  hot$option_2 <- as.character(hot$option_2)

  expect_error(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop",
    res = "agg"
  ))
})



test_that("Warning if group contains NA ", {
  hot$group[34] <- NA

  expect_warning(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop",
    group = group,
    res = "agg"
  ))
})

test_that("wrong input to method ", {
  expect_error(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "test",
    res = "agg"
  ))
})

test_that("wrong input to res ", {
  expect_error(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop",
    res = "test"
  ))
})

# end --------------------------------------------------------------------------

# group input equals group output ----------------------------------------------
test_that("group output equals group input ", {
  expect_equal(str(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop",
    res = "agg",
    group = group
  )[[1]]), str(hot$group))
})

test_that("group output equals group input - character input ", {
  hot$group2 <- c("group 1", "group 2")

  expect_equal(
    str(marksim(
      data = hot,
      opts = c(option_1:none),
      res = "agg",
      method = "sop",
      group = group2
    )[[1]]), str(hot$group2)
  )
})

test_that("group output equals group input - labelled input ", {
  hot$group2 <- c(1:2)
  hot$group2 <- labelled::labelled(hot$group2,
    labels = c("group 1" = 1, "group 2" = 2)
  )
  expect_true(labelled::is.labelled(marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop",
    res = "agg",
    group = group2
  )[[1]]))
})
# end --------------------------------------------------------------------------

# other checks -----------------------------------------------------------------

test_that("If method not specified, default is 'sop' ", {
  test1 <- marksim(
    data = hot,
    opts = c(option_1:none)
  )

  test2 <- marksim(
    data = hot,
    opts = c(option_1:none),
    method = "sop"
  )

  expect_true(all(test1 == test2))
})

test_that("marksim() also working with data.frame not
          created with create_hot()", {
  set.seed(2023)

  newHOT <- data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = sample(c(1:5), 10, replace = T)
  )

  expect_equal(sum(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[2]]), 100)
})

test_that("If res not specified, default is agg ", {
  test1 <- marksim(
    data = hot,
    opts = c(option_1:none)
  )

  test2 <- marksim(
    data = hot,
    opts = c(option_1:none),
    res = "agg"
  )

  expect_true(all(test1 == test2))
})
# end --------------------------------------------------------------------------
