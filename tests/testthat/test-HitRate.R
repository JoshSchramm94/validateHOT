HOT <- createHOT(
  data = MaxDiff, none = 19,
  id = 1,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Error if opts is missing", {
  expect_error(hitrate(data = HOT, choice = choice))
})

test_that("Error if opts has just length 1 ", {
  expect_error(hitrate(data = HOT, opts = Option_1, choice = choice))
})

test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(hitrate(data = HOT2, opts = c(Option_1:None),
                         choice = choice, group = Group))
})

test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(hitrate(data = HOT2, opts = c(Option_1:None),
                       choice = choice, group = Group))
})

test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- base::as.character(HOT2$Option_2)

  expect_error(hitrate(data = HOT2, opts = c(Option_1:None),
                       choice = choice, group = Group))
})

test_that("Error if choice contains NA ", {
  HOT2 <- HOT

  HOT2$choice[34] <- NA

  expect_error(hitrate(data = HOT2, opts = c(Option_1:None),
                       choice = choice, group = Group))
})

test_that("Error if choice is not numeric ", {
  HOT2 <- HOT

  HOT2$choice <- base::as.character(HOT2$choice)

  expect_error(hitrate(data = HOT2, opts = c(Option_1:None),
                       choice = choice, group = Group))
})

test_that("Structure of Output data.frame ", {
  expect_true(base::is.data.frame(hitrate(data = HOT, opts = c(Option_1:None),
                                          choice = choice)))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(hitrate(data = HOT, opts = c(Option_1:None),
                                        choice = choice)))
})

test_that("Length of output equals number of groups - no group ", {
  expect_equal(base::nrow(hitrate(data = HOT, opts = c(Option_1:None),
                                  choice = choice)), 1)
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(base::nrow(hitrate(data = HOT, opts = c(Option_1:None),
                                  choice = choice, group = Group)),
               base::length(base::unique(HOT$Group)))
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(base::nrow(hitrate(data = HOT, opts = c(Option_1:None),
                                  choice = choice, group = c(Group, Group2))),
               (base::length(base::unique(HOT$Group)) * base::length(
                 base::unique(HOT$Group2))))
})

test_that("Numeric output - no group ", {
  expect_true(base::is.numeric(hitrate(data = HOT, opts = c(Option_1:None),
                                       choice = choice)[[1]]))
})

test_that("Numeric output - 1 group ", {
  expect_true(base::is.numeric(hitrate(data = HOT, opts = c(Option_1:None),
                                       choice = choice, group = Group)[[2]]))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(hitrate(data = HOT, opts = c(Option_1:None),
                                  choice = choice, group = Group)[[1]]),
               utils::str(HOT$Group))
})

test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(hitrate(data = HOT, opts = c(Option_1:None),
                                  choice = choice, group = Group2)[[1]]),
               utils::str(HOT$Group2))
})

test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(hitrate(data = HOT,
                                            opts = c(Option_1:None),
                                            choice = choice,
                                            group = Group2)[[1]]))
})

test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(hitrate(data = HOT, opts = c(Option_1:None),
                                  choice = choice,
                                  group = c(Group, Group2))[[1]]),
               utils::str(HOT$Group))
  expect_true(labelled::is.labelled(hitrate(data = HOT, opts = c(Option_1:None),
                                            choice = choice,
                                            group = c(Group, Group2))[[2]]))
  expect_true(base::is.numeric(hitrate(data = HOT, opts = c(Option_1:None),
                                       choice = choice,
                                       group = c(Group, Group2))[[3]]))
})

test_that("hitrate() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T)
  )
  expect_true(base::is.numeric(hitrate(data = newHOT,
                                       opts = c(Option_1:Option_5),
                                       choice = Choice)[[1]]))
  expect_true(tibble::is_tibble(hitrate(data = newHOT,
                                        opts = c(Option_1:Option_5),
                                        choice = Choice)))
  expect_false(base::anyNA(hitrate(data = newHOT, opts = c(Option_1:Option_5),
                                   choice = Choice)))
})

test_that("check whether examples are correct ", {
  expect_equal(base::round(base::as.numeric(hitrate(data = HOT,
                                                    opts = c(Option_1:None),
                                                    choice = choice)), 0),
               c(56, 6, 12, 39, 70))

  expect_equal(base::round(base::as.numeric(hitrate(data = HOT,
                                                    opts = c(Option_1:None),
                                                    choice = choice,
                                                    group = Group)[[2]]), 0),
               c(57, 60, 50))
  expect_equal(base::round(base::as.numeric(hitrate(data = HOT,
                                                    opts = c(Option_1:None),
                                                    choice = choice,
                                                    group = Group)[[3]]), 1),
               c(10.6, 10.0, 10.9))
  expect_equal(base::round(base::as.numeric(hitrate(data = HOT,
                                                    opts = c(Option_1:None),
                                                    choice = choice,
                                                    group = Group)[[4]]), 1),
               c(12.5, 12.5, 12.5))
  expect_equal(base::round(base::as.numeric(hitrate(data = HOT,
                                                    opts = c(Option_1:None),
                                                    choice = choice,
                                                    group = Group)[[5]]), 1),
               c(13, 15, 11))
  expect_equal(base::round(base::as.numeric(hitrate(data = HOT,
                                                    opts = c(Option_1:None),
                                                    choice = choice,
                                                    group = Group)[[6]]), 1),
               c(23, 25, 22))
  expect_equal(base::sum(hitrate(data = HOT, opts = c(Option_1:None),
                                 choice = choice, group = Group)[[6]]),
               base::nrow(HOT))
})
