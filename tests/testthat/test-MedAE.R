HOT <- createHOT(
  data = MaxDiff, none = 19,
  id = 1,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Error if opts is missing", {
  expect_error(medae(data = HOT, choice = choice))
})

test_that("Error if opts has just length 1 ", {
  expect_error(medae(data = HOT, opts = Option_1, choice = choice))
})

test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(medae(data = HOT2, opts = c(Option_1:None), choice = choice, group = Group))
})

test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(medae(data = HOT2, opts = c(Option_1:None), choice = choice, group = Group))
})

test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- as.character(HOT2$Option_2)

  expect_error(medae(data = HOT2, opts = c(Option_1:None), choice = choice, group = Group))
})

test_that("Error if choice contains NA ", {
  HOT2 <- HOT

  HOT2$choice[34] <- NA

  expect_error(medae(data = HOT2, opts = c(Option_1:None), choice = choice, group = Group))
})

test_that("Error if choice is not numeric ", {
  HOT2 <- HOT

  HOT2$choice <- as.character(HOT2$choice)

  expect_error(medae(data = HOT2, opts = c(Option_1:None), choice = choice, group = Group))
})

test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(medae(data = HOT, opts = c(Option_1:None), choice = choice)))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(medae(data = HOT, opts = c(Option_1:None), choice = choice)))
})

test_that("Length of output equals number of groups - no group ", {
  expect_equal(nrow(medae(data = HOT, opts = c(Option_1:None), choice = choice)), 1)
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(nrow(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = Group)), length(unique(HOT$Group)))
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(nrow(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = c(Group, Group2))), (length(unique(HOT$Group)) * length(unique(HOT$Group2))))
})

test_that("Numeric output - no group ", {
  expect_true(is.numeric(medae(data = HOT, opts = c(Option_1:None), choice = choice)[[1]]))
})

test_that("Numeric output - 1 group ", {
  expect_true(is.numeric(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = Group)[[2]]))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = Group)[[1]]), utils::str(HOT$Group))
})

test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = Group2)[[1]]), utils::str(HOT$Group2))
})

test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = Group2)[[1]]))
})

test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = c(Group, Group2))[[1]]), utils::str(HOT$Group))
  expect_true(labelled::is.labelled(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = c(Group, Group2))[[2]]))
  expect_true(is.numeric(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = c(Group, Group2))[[3]]))
})

test_that("medae() also working with data.frame not created with createHOT()", {
  set.seed(2023)

  newHOT <- data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = sample(c(1:5), 10, replace = T)
  )
  expect_true(is.numeric(medae(data = newHOT, opts = c(Option_1:Option_5), choice = Choice)[[1]]))
  expect_true(tibble::is_tibble(medae(data = newHOT, opts = c(Option_1:Option_5), choice = Choice)))
  expect_false(anyNA(medae(data = newHOT, opts = c(Option_1:Option_5), choice = Choice)))
})

test_that("check whether examples are correct ", {
  expect_equal(round(as.numeric(medae(data = HOT, opts = c(Option_1:None), choice = choice)), 2), 1.99)
  expect_equal(round(as.numeric(medae(data = HOT, opts = c(Option_1:None), choice = choice, group = Group)[[2]]), 2), c(2.67, 3.18, 6.11))
})


test_that("Test whether results equals Metrics::mdae ", {
  actual <- HOT %>%
    dplyr::mutate(dplyr::across(Option_1:None, function(x) (exp(x) / rowSums(exp(.[c(2:9)]))))) %>%
    dplyr::summarise(dplyr::across(Option_1:None, ~ mean(.x))) %>%
    unlist() %>%
    unname()

  predicted <- unname(c(prop.table(table(HOT$choice))))

  expect_equal(round(as.numeric(medae(data = HOT, opts = c(Option_1:None), choice = choice)), digits = 2), round(Metrics::mdae(actual, predicted) * 100, digits = 2))
})
