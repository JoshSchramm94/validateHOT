HOT <- createHOT(
  data = MaxDiff, none = 19,
  id = 1,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Epsilon needs to be numeric ", {
  expect_error(kl(data = HOT, opts = c(Option_1:None),
                  choice = choice, base = "log2", epsilon = ".001"))
})

test_that("Epsilon handles different value ", {
  HOT$Group2 <- c(1:10)
  expect_no_error(kl(
    data = HOT, opts = c(Option_1:None),
    choice = choice, base = "log2", epsilon = .00003425,
    group = c(Group, Group2)
  ))
})

test_that("base needs to be 'log' or 'log2' ", {
  expect_error(kl(data = HOT, opts = c(Option_1:None),
                  choice = choice, base = "log3"))
})

test_that("base does not need to be specified ", {
  expect_no_error(kl(data = HOT, opts = c(Option_1:None), choice = choice))
})

test_that("No base gives same results as log base (default) ", {
  expect_true(kl(data = HOT, opts = c(Option_1:None), choice = choice)[1] ==
    kl(data = HOT, opts = c(Option_1:None), choice = choice, base = "log")[1])

  expect_true(kl(data = HOT, opts = c(Option_1:None), choice = choice)[2] ==
    kl(data = HOT, opts = c(Option_1:None), choice = choice, base = "log")[2])
})

test_that("Error if opts is missing ", {
  expect_error(kl(data = HOT, choice = choice, base = "log2"))
})

test_that("Error if opts has just length 1", {
  expect_error(kl(data = HOT, opts = Option_1, choice = choice, base = "log2"))
})

test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(kl(data = HOT2, opts = c(Option_1:None),
                    choice = choice, base = "log2", group = Group))
})

test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(kl(data = HOT2, opts = c(Option_1:None),
                  choice = choice, group = Group, base = "log2"))
})

test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- base::as.character(HOT2$Option_2)

  expect_error(kl(data = HOT2, opts = c(Option_1:None), choice = choice,
                  group = Group, base = "log2"))
})

test_that("Error if choice contains NA ", {
  HOT2 <- HOT

  HOT2$choice[34] <- NA

  expect_error(kl(data = HOT2, opts = c(Option_1:None), choice = choice,
                  group = Group, base = "log2"))
})

test_that("Error if choice is not numeric ", {
  HOT2 <- HOT

  HOT2$choice <- base::as.character(HOT2$choice)

  expect_error(kl(data = HOT2, opts = c(Option_1:None), choice = choice,
                  group = Group, base = "log2"))
})

test_that("Structure of Output data.frame ", {
  expect_true(base::is.data.frame(kl(data = HOT, opts = c(Option_1:None),
                                     choice = choice, base = "log2")))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(kl(data = HOT, opts = c(Option_1:None),
                                   choice = choice, base = "log2")))
})

test_that("Length of output equals number of groups - no group ", {
  expect_equal(base::nrow(kl(data = HOT, opts = c(Option_1:None),
                             choice = choice, base = "log2")), 1)
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(base::nrow(kl(data = HOT, opts = c(Option_1:None),
                             choice = choice, group = Group, base = "log2")),
               base::length(base::unique(HOT$Group)))
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(base::nrow(kl(data = HOT, opts = c(Option_1:None),
                             base = "log2", choice = choice,
                             group = c(Group, Group2))),
               (base::length(base::unique(HOT$Group)) *
                  base::length(base::unique(HOT$Group2))))
})

test_that("Always two values ", {
  expect_equal(base::ncol(kl(data = HOT, opts = c(Option_1:None),
                             choice = choice, base = "log2")), 2)
})

test_that("Check column names ", {
  expect_equal(colnames(kl(data = HOT, opts = c(Option_1:None),
                           choice = choice, base = "log2")),
               c("kl_o_p", "kl_p_o"))
})

test_that("Check missing symmetry ", {
  expect_true(kl(data = HOT, opts = c(Option_1:None),
                 choice = choice,
                 base = "log2")[[1]] != kl(data = HOT,
                                           opts = c(Option_1:None),
                                           choice = choice, base = "log2")[[2]])
  expect_true(kl(data = HOT, opts = c(Option_1:None),
                 choice = choice, base = "log")[[1]] != kl(
                   data = HOT,
                   opts = c(Option_1:None), choice = choice, base = "log")[[2]])
})

test_that("Numeric output - no group ", {
  expect_true(base::is.numeric(kl(data = HOT,
                                  opts = c(Option_1:None),
                                  choice = choice, base = "log2")[[1]]))
  expect_true(base::is.numeric(kl(data = HOT,
                                  opts = c(Option_1:None),
                                  choice = choice, base = "log2")[[2]]))
})

test_that("Numeric output - 1 group ", {
  expect_true(base::is.numeric(kl(data = HOT, opts = c(Option_1:None),
                                  choice = choice, base = "log2",
                                  group = Group)[[2]]))
  expect_true(base::is.numeric(kl(data = HOT, opts = c(Option_1:None),
                                  choice = choice, base = "log2",
                                  group = Group)[[3]]))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(kl(data = HOT, opts = c(Option_1:None),
                             choice = choice, base = "log2")[[1]]),
               utils::str(HOT$Group))
})

test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(kl(data = HOT, opts = c(Option_1:None),
                             choice = choice, group = Group2,
                             base = "log2")[[1]]), utils::str(HOT$Group2))
})

test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(kl(data = HOT, opts = c(Option_1:None),
                                       choice = choice, group = Group2,
                                       base = "log2")[[1]]))
})

test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- 0
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(kl(data = HOT, opts = c(Option_1:None),
                             choice = choice, group = Group,
                             base = "log2")[[1]]),
               utils::str(HOT$Group))
  expect_true(labelled::is.labelled(kl(data = HOT, opts = c(Option_1:None),
                                       choice = choice, base = "log2",
                                       group = c(Group, Group2))[[2]]))
  expect_true(base::is.numeric(kl(data = HOT, opts = c(Option_1:None),
                                  choice = choice, base = "log2",
                                  group = c(Group, Group2))[[3]]))
  expect_true(base::is.numeric(kl(data = HOT, opts = c(Option_1:None),
                                  choice = choice, base = "log2",
                                  group = c(Group, Group2))[[4]]))
})

test_that("kl() also working with data.frame not created with createHOT()", {
  base::set.seed(2023)

  newHOT <- base::data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = base::sample(c(1:5), 10, replace = T)
  )
  expect_true(base::is.numeric(kl(data = newHOT, opts = c(Option_1:Option_5),
                                  choice = Choice, base = "log2")[[1]]))
  expect_true(tibble::is_tibble(kl(data = newHOT, opts = c(Option_1:Option_5),
                                   choice = Choice, base = "log2")))
  expect_false(base::anyNA(kl(data = newHOT, opts = c(Option_1:Option_5),
                              choice = Choice, base = "log2")))
})

test_that("check whether examples are correct ", {
  expect_equal(base::round(base::as.numeric(kl(data = HOT,
                                               opts = c(Option_1:None),
                                               choice = choice,
                                               base = "log2")), 2),
               c(0.18, 0.17))
  expect_equal(base::round(base::as.numeric(kl(data = HOT,
                                               opts = c(Option_1:None),
                                               choice = choice, group = Group,
                                               base = "log2")[[2]]), 2),
               c(1.09, 0.23, 0.60))
  expect_equal(base::round(base::as.numeric(kl(data = HOT,
                                               opts = c(Option_1:None),
                                               choice = choice,
                                               group = Group,
                                               base = "log2")[[3]]), 2),
               c(0.29, 0.61, 1.34))
})

test_that("Results differ depending on method ", {
  expect_true(kl(data = HOT, opts = c(Option_1:None), choice = choice,
                 base = "log2")[[1]] != kl(data = HOT, opts = c(Option_1:None),
                                           choice = choice, base = "log")[[1]])
  expect_true(kl(data = HOT, opts = c(Option_1:None), choice = choice,
                 base = "log2")[[2]] != kl(data = HOT, opts = c(Option_1:None),
                                           choice = choice, base = "log")[[2]])
})
