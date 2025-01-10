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
  expect_error(reach(data = hot, opts = c(option_1, option_2, option_6)))
})

test_that("Error if opts is missing ", {
  expect_error(reach(data = hot, none = none))
})
# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if none is missing ", {
  expect_error(reach(data = hot, opts = c(option_1, option_2, option_6)))
})

test_that("Error if opts is missing ", {
  expect_error(reach(
    data = hot,
    opts = c(option_1, option_2, option_6, none),
    none = none
  ))
})

test_that("Error if alternatives contains NA ", {
  hot_test <- hot

  hot_test$option_2[34] <- NA

  expect_error(reach(
    data = hot_test,
    opts = c(option_1, option_2, option_6),
    none = none
  ))
})

test_that("Error if alternatives is not numeric ", {
  hot_test <- hot

  hot_test$option_2 <- as.character(hot_test$option_2)

  expect_error(reach(
    data = hot_test,
    opts = c(option_1, option_2, option_6),
    none = none
  ))
})

# end --------------------------------------------------------------------------

# group input equals group output ----------------------------------------------
test_that("group output equals group input ", {
  expect_equal(str(reach(
    data = hot,
    opts = c(option_1, option_2, option_6),
    none = none,
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

test_that("reach() also working with data.frame not created with
          create_hot()", {
  set.seed(2023)

  new_hot_df <- data.frame(
    option_1 = runif(10, min = -5, max = 5),
    option_2 = runif(10, min = -5, max = 5),
    option_3 = runif(10, min = -5, max = 5),
    option_4 = runif(10, min = -5, max = 5),
    option_5 = runif(10, min = -5, max = 5),
    choice = sample(c(1:5), 10, replace = T)
  )

  expect_true(is.numeric(reach(
    data = new_hot_df,
    opts = c(option_1:option_4),
    none = option_5
  )[[1]]))
})

# end --------------------------------------------------------------------------
