HOT <- createHOT(
  data = MaxDiff, none = 19,
  id = 1,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Error if none is missing", {
  expect_error(specificity(data = HOT, opts = c(Option_1:None), choice = choice))
})

test_that("Error if opts is missing", {
  expect_error(specificity(data = HOT, choice = choice, none = None))
})

test_that("Error if opts has just length 1", {
  expect_error(specificity(data = HOT, opts = Option_1, choice = choice, none = None))
})

test_that("Error if none is not part of opts", {
  expect_error(specificity(data = HOT, opts = c(Option_1:Option_3), choice = choice, none = None))
})

test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(specificity(data = HOT2, opts = c(Option_1:None), choice = choice, none = None, group = Group))
})

test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(specificity(data = HOT2, opts = c(Option_1:None), choice = choice, none = None, group = Group))
})

test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- as.character(HOT2$Option_2)

  expect_error(specificity(data = HOT2, opts = c(Option_1:None), choice = choice, none = None, group = Group))
})

test_that("Error if choice contains NA ", {
  HOT2 <- HOT

  HOT2$choice[34] <- NA

  expect_error(specificity(data = HOT2, opts = c(Option_1:None), choice = choice, none = None, group = Group))
})

test_that("Error if choice is not numeric ", {
  HOT2 <- HOT

  HOT2$choice <- as.character(HOT2$choice)

  expect_error(specificity(data = HOT2, opts = c(Option_1:None), choice = choice, none = None, group = Group))
})

test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None)))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None)))
})

test_that("Length of output equals number of groups - no group ", {
  expect_equal(nrow(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None)), 1)
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(nrow(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group)), length(unique(HOT$Group)))
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(nrow(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = c(Group, Group2))), (length(unique(HOT$Group)) * length(unique(HOT$Group2))))
})

test_that("Numeric output - no group ", {
  expect_true(is.numeric(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None)[[1]]))
})

test_that("Numeric output - 1 group ", {
  expect_true(is.numeric(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group)[[2]]))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group)[[1]]), utils::str(HOT$Group))
})

test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group2)[[1]]), utils::str(HOT$Group2))
})

test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group2)[[1]]))
})

test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = c(Group, Group2))[[1]]), utils::str(HOT$Group))
  expect_true(labelled::is.labelled(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = c(Group, Group2))[[2]]))
  expect_true(is.numeric(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = c(Group, Group2))[[3]]))
})

test_that("specificity() also working with data.frame not created with createHOT()", {
  set.seed(2023)

  newHOT <- data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = sample(c(1:5), 10, replace = T)
  )
  expect_true(is.numeric(specificity(data = newHOT, opts = c(Option_1:Option_5), choice = Choice, none = Option_3)[[1]]))
  expect_true(tibble::is_tibble(specificity(data = newHOT, opts = c(Option_1:Option_5), choice = Choice, none = Option_5)))
  expect_false(anyNA(specificity(data = newHOT, opts = c(Option_1:Option_5), choice = Choice, none = Option_5)))
})

test_that("check whether examples are correct ", {
  expect_equal(round(as.numeric(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None)), 0), 32)
  expect_equal(round(as.numeric(specificity(data = HOT, opts = c(Option_1:None), choice = choice, none = None, group = Group)[[2]]), 0), c(20, 50, 12))
})
