HOT <- createHOT(
  data = MaxDiff, none = 19,
  id = 1,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Error if none is missing", {
  expect_error(freqassort(data = HOT, opts = c(Option_1, Option_2, Option_6)))
})

test_that("Error if opts is missing", {
  expect_error(freqassort(data = HOT, none = None))
})

test_that("Error if none is part of opts", {
  expect_error(freqassort(data = HOT, opts = c(Option_1:None), none = None))
})

test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(freqassort(data = HOT2,
                            opts = c(Option_1, Option_2, Option_6),
                            none = None, group = Group))
})

test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(freqassort(data = HOT2, opts = c(Option_1, Option_2, Option_6),
                          none = None, group = Group))
})

test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- base::as.character(HOT2$Option_2)

  expect_error(freqassort(data = HOT2, opts = c(Option_1, Option_2, Option_6),
                          none = None, group = Group))
})


test_that("Error if none contains NA ", {
  HOT2 <- HOT

  HOT2$None[34] <- NA

  expect_error(freqassort(data = HOT2, opts = c(Option_1, Option_2, Option_6),
                          none = None, group = Group))
})

test_that("Error if none is not numeric ", {
  HOT2 <- HOT

  HOT2$None <- base::as.character(HOT2$None)

  expect_error(freqassort(data = HOT2, opts = c(Option_1, Option_2, Option_6),
                          none = None, group = Group))
})


test_that("Structure of Output data.frame ", {
  expect_true(base::is.data.frame(freqassort(data = HOT,
                                             opts = c(Option_1, Option_2,
                                                      Option_6), none = None)))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(freqassort(data = HOT,
                                           opts = c(Option_1, Option_2,
                                                    Option_6), none = None)))
})

test_that("Length of output equals number of groups - no group ", {
  expect_equal(base::nrow(freqassort(data = HOT,
                                     opts = c(Option_1, Option_2,
                                                          Option_6),
                                     none = None)), 1)
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(base::nrow(freqassort(data = HOT, opts = c(Option_1, Option_2,
                                                          Option_6),
                                     none = None, group = Group)),
               base::length(base::unique(HOT$Group)))
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(base::nrow(freqassort(data = HOT, opts = c(Option_1, Option_2,
                                                          Option_6),
                                     none = None, group = c(Group, Group2))),
               (base::length(base::unique(HOT$Group)) *
                  base::length(base::unique(HOT$Group2))))
})

test_that("Numeric output - no group ", {
  expect_true(base::is.numeric(freqassort(data = HOT,
                                          opts = c(Option_1, Option_2,
                                                   Option_6),
                                          none = None)[[1]]))
})

test_that("Numeric output - 1 group ", {
  expect_true(base::is.numeric(freqassort(data = HOT,
                                          opts = c(Option_1, Option_2,
                                                   Option_6), none = None,
                                          group = Group)[[2]]))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(freqassort(data = HOT, opts = c(Option_1, Option_2,
                                                          Option_6),
                                     none = None, group = Group)[[1]]),
               utils::str(HOT$Group))
})

test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(freqassort(data = HOT, opts = c(Option_1, Option_2,
                                                          Option_6),
                                     none = None, group = Group2)[[1]]),
               utils::str(HOT$Group2))
})

test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(freqassort(data = HOT,
                                               opts = c(Option_1, Option_2,
                                                        Option_6), none = None,
                                               group = Group2)[[1]]))
})

test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(freqassort(data = HOT, opts = c(Option_1, Option_2,
                                                          Option_6),
                                     none = None,
                                     group = c(Group, Group2))[[1]]),
               utils::str(HOT$Group))
  expect_true(labelled::is.labelled(freqassort(data = HOT,
                                               opts = c(Option_1, Option_2,
                                                        Option_6),
                                               none = None,
                                               group = c(Group, Group2))[[2]]))
  expect_true(base::is.numeric(freqassort(data = HOT,
                                          opts = c(Option_1, Option_2,
                                                   Option_6), none = None,
                                          group = c(Group, Group2))[[3]]))
})

test_that("freqassort() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T)
  )
  expect_true(base::is.numeric(freqassort(data = newHOT,
                                          opts = c(Option_1:Option_4),
                                          none = Option_5)[[1]]))
  expect_true(tibble::is_tibble(freqassort(data = newHOT,
                                           opts = c(Option_1:Option_4),
                                           none = Option_5)))
  expect_false(base::anyNA(freqassort(data = newHOT,
                                      opts = c(Option_1:Option_4),
                                      none = Option_5)))
})

test_that("check whether examples are correct ", {
  expect_equal(base::round(base::as.numeric(freqassort(data = HOT,
                                                       opts = c(Option_1,
                                                                Option_2,
                                                                Option_6),
                                                       none = None)), 2), 1.44)
  expect_equal(base::round(base::as.numeric(freqassort(data = HOT,
                                                       opts = c(Option_1,
                                                                Option_2,
                                                                Option_6),
                                                       none = None,
                                                       group = Group)[[2]]),
                           2), c(1.83, 1.24, 1.27))
})
