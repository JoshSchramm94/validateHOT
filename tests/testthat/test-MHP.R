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
  expect_error(mhp(data = hot, choice = choice))
})

test_that("Error if choice is missing ", {
  expect_error(mhp(data = hot, opts = c(option_1:none)))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if opts has just length 1 ", {
  expect_error(mhp(
    data = hot,
    opts = option_1,
    choice = choice
  ))
})

test_that("Error if opts contains NA ", {
  hot_testthat <- hot

  hot_testthat$option_2[34] <- NA

  expect_error(mhp(
    data = hot_testthat,
    opts = c(option_1:none),
    choice = choice
  ))
})

test_that("Error if choice contains NA ", {
  hot_testthat <- hot

  hot_testthat$choice[34] <- NA

  expect_error(mhp(
    data = hot_testthat,
    opts = c(option_1:none),
    choice = choice
  ))
})

test_that("Error if opts is not numeric ", {
  hot_testthat <- hot

  hot_testthat$option_2 <- as.character(hot_testthat$option_2)

  expect_error(mhp(
    data = hot_testthat,
    opts = c(option_1:none),
    choice = choice
  ))
})

test_that("Error if choice is not numeric ", {
  hot_testthat <- hot

  hot_testthat$choice <- as.character(hot_testthat$choice)

  expect_error(mhp(
    data = hot_testthat,
    opts = c(option_1:none),
    choice = choice
  ))
})

test_that("Warning if group contains NA ", {
  hot_testthat <- hot

  hot_testthat$group[34] <- NA

  expect_warning(mhp(
    data = hot_testthat,
    opts = c(option_1:none),
    choice = choice,
    group = group
  ))
})

# end --------------------------------------------------------------------------

# group input equals group output ----------------------------------------------
test_that("group output equals group input ", {
  expect_equal(str(mhp(
    data = hot,
    opts = c(option_1:none),
    choice = choice,
    group = group
  )[[1]]), str(hot$group))
})

test_that("group output equals group input - character input ", {
  hot$group2 <- c("group 1", "group 2")
  expect_equal(str(mhp(
    data = hot,
    opts = c(option_1:none),
    choice = choice,
    group = group2
  )[[1]]), str(hot$group2))
})

test_that("group output equals group input - labelled input ", {
  hot$group2 <- c(1:2)
  hot$group2 <- labelled::labelled(hot$group2,
    labels = c("group 1" = 1, "group 2" = 2)
  )
  expect_true(labelled::is.labelled(mhp(
    data = hot,
    opts = c(option_1:none),
    choice = choice,
    group = group2
  )[[1]]))
})
# end --------------------------------------------------------------------------

# check for different input data type ------------------------------------------

test_that("mhp() also working with data.frame not created
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
  expect_true(is.numeric(mhp(
    data = new_hot_df,
    opts = c(option_1:option_5),
    choice = choice
  )[[1]]))
})

# end --------------------------------------------------------------------------
