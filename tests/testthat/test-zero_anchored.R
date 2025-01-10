# check for error messages for missing arguments -------------------------------
test_that("Error if items are missing ", {
  expect_error(zero_anchored(
    data = maxdiff,
    # items = c(option_01:option_16),
    res = "agg"
  ))
})


test_that("Error if res is missing ", {
  expect_error(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    # res = "agg"
  ))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------
test_that("Error if items has just length 1 ", {
  expect_error(zero_anchored(
    data = maxdiff,
    items = option_01,
    res = "agg"
  ))
})

test_that("Error if anchor is not part of opts ", {
  expect_error(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    anchor = none,
    res = "agg"
  ))
})

test_that("Error if opts contains NA ", {
  maxdiff$option_02[34] <- NA

  expect_error(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    res = "agg"
  ))
})

test_that("Error if opts is not numeric ", {
  maxdiff$option_02 <- as.character(maxdiff$option_02)

  expect_error(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    res = "agg"
  ))
})

test_that("Warning if group contains NA ", {
  maxdiff$group[34] <- NA

  expect_warning(zero_anchored(
    data = maxdiff,
    items = c(option_01:none),
    anchor = none,
    res = "agg",
    group = group
  ))
})

test_that("wrong input to res ", {
  expect_error(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    res = "test"
  ))
})

test_that("Multiple variables provided to anchor ", {
  expect_error(zero_anchored(
    data = maxdiff,
    items = c(option_01:none),
    anchor = c(option_01, none),
    res = "agg"
  ))
})

# end --------------------------------------------------------------------------

# group input equals group output ----------------------------------------------
test_that("group output equals group input ", {
  expect_equal(str(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    res = "agg",
    group = group
  )[[1]]), str(maxdiff$group))
})

test_that("group output equals group input - character input ", {
  maxdiff$group2 <- c("group 1", "group 2")
  expect_equal(
    str(zero_anchored(
      data = maxdiff,
      items = c(option_01:option_16),
      res = "agg",
      group = group2
    )[[1]]), str(maxdiff$group2)
  )
})

test_that("group output equals group input - labelled input ", {
  maxdiff$group2 <- c(1:2)
  maxdiff$group2 <- labelled::labelled(maxdiff$group2,
    labels = c("group 1" = 1, "group 2" = 2)
  )
  expect_true(labelled::is.labelled(zero_anchored(
    data = maxdiff,
    items = c(option_01:option_16),
    res = "agg",
    group = group2
  )[[1]]))
})
# end --------------------------------------------------------------------------
