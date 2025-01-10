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
test_that("Error if none is missing ", {
  expect_error(accuracy(data = hot, opts = c(option_1:none), choice = choice))
})

test_that("Error if opts is missing ", {
  expect_error(accuracy(data = hot, choice = choice, none = none))
})

test_that("Error if choice is missing ", {
  expect_error(accuracy(data = hot, opts = c(option_1:none), none = none))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if opts has just length 1 ", {
  expect_error(accuracy(
    data = hot, opts = option_1,
    choice = choice, none = none
  ))
})

test_that("Error if none is not part of opts ", {
  expect_error(accuracy(
    data = hot, opts = c(option_1:option_3),
    choice = choice, none = none
  ))
})

test_that("Error if opts contains NA ", {
  hot_testthat <- hot

  hot_testthat$option_2[34] <- NA

  expect_error(accuracy(
    data = hot_testthat, opts = c(option_1:none),
    choice = choice, none = none
  ))
})

test_that("Error if choice contains NA ", {
  hot_testthat <- hot

  hot_testthat$choice[34] <- NA

  expect_error(accuracy(
    data = hot_testthat, opts = c(option_1:none),
    choice = choice, none = none
  ))
})

test_that("Error if opts is not numeric ", {
  hot_testthat <- hot

  hot_testthat$option_2 <- as.character(hot_testthat$option_2)

  expect_error(accuracy(
    data = hot_testthat, opts = c(option_1:none),
    choice = choice, none = none
  ))
})

test_that("Error if choice is not numeric ", {
  hot_testthat <- hot

  hot_testthat$choice <- as.character(hot_testthat$choice)

  expect_error(accuracy(
    data = hot_testthat, opts = c(option_1:none),
    choice = choice, none = none
  ))
})

test_that("Warning if group contains NA ", {
  hot2 <- hot

  hot2$group[34] <- NA

  expect_warning(accuracy(
    data = hot2, opts = c(option_1:none),
    choice = choice, none = none,
    group = group
  ))
})

# end --------------------------------------------------------------------------

# group input equals group output ----------------------------------------------
test_that("group output equals group input ", {
  expect_equal(str(accuracy(
    data = hot, opts = c(option_1:none),
    choice = choice, none = none,
    group = group
  )[[1]]), str(hot$group))
})

test_that("group output equals group input - character input ", {
  hot$group2 <- c("group 1", "group 2")
  expect_equal(
    str(accuracy(
      data = hot, opts = c(option_1:none),
      choice = choice, none = none,
      group = group2
    )[[1]]), str(hot$group2)
  )
})

test_that("group output equals group input - labelled input ", {
  hot$group2 <- c(1:2)
  hot$group2 <- labelled::labelled(hot$group2,
    labels = c("group 1" = 1, "group 2" = 2)
  )
  expect_true(labelled::is.labelled(accuracy(
    data = hot,
    opts = c(option_1:none),
    choice = choice, none = none,
    group = group2
  )[[1]]))
})
# end --------------------------------------------------------------------------

# check for different input data type ------------------------------------------

test_that("accuracy() also working with data.frame not created
          with create_hot() ", {
  set.seed(2023)

  new_hot_df <- data.frame(
    option_1 = runif(10, min = -5, max = 5),
    option_2 = runif(10, min = -5, max = 5),
    option_3 = runif(10, min = -5, max = 5),
    option_4 = runif(10, min = -5, max = 5),
    option_5 = runif(10, min = -5, max = 5),
    choice = sample(c(1:5), 10, replace = T)
  )
  expect_true(is.numeric(accuracy(
    data = new_hot_df,
    opts = c(option_1:option_5),
    choice = choice,
    none = option_3
  )[[1]]))
})

# end --------------------------------------------------------------------------

# check for same results as in Metrics package ---------------------------------

test_that("Test whether results equals Metrics::accuracy ", {
  metr <- hot %>%
    mutate(
      pred = max.col(.[c(2:9)]),
      pred = ifelse(pred == 8, 2, 1),
      choice = ifelse(choice == 8, 2, 1)
    ) %>%
    select(pred, choice)

  actual <- c(unname(unlist(metr$choice)))
  actual <- ifelse(actual == 1, 1, 0)

  predicted <- c(unname(unlist(metr$pred)))
  predicted <- ifelse(predicted == 1, 1, 0)

  expect_equal(
    round(as.numeric(accuracy(
      data = hot,
      opts = c(option_1:none),
      choice = choice,
      none = none
    )), digits = 2),
    round(Metrics::accuracy(actual, predicted) * 100, digits = 2)
  )
})
# end --------------------------------------------------------------------------
