HOT <- createHOT(
  data = MaxDiff, none = 19,
  id = 1,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  choice = 20, method = "MaxDiff", varskeep = 21
)

test_that("Error if opts is missing", {
  expect_error(marksim(data = HOT))
})

##
test_that("Error if opts has just length 1 ", {
  expect_error(marksim(data = HOT, opts = Option_1, method = "sop"))
})

test_that("Error if opts has just length 1 ", {
  expect_error(marksim(data = HOT, opts = Option_1, method = "fc"))
})
##

##
test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(marksim(
    data = HOT2, opts = c(Option_1:None), group = Group,
    method = "sop"
  ))
})

test_that("Warning if group contains NA ", {
  HOT2 <- HOT

  HOT2$Group[34] <- NA

  expect_warning(marksim(
    data = HOT2, opts = c(Option_1:None), group = Group,
    method = "fc"
  ))
})
##

##
test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(marksim(
    data = HOT2, opts = c(Option_1:None), group = Group,
    method = "sop"
  ))
})

test_that("Error if alternatives contains NA ", {
  HOT2 <- HOT

  HOT2$Option_2[34] <- NA

  expect_error(marksim(
    data = HOT2, opts = c(Option_1:None), group = Group,
    method = "fc"
  ))
})
##

##
test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- as.character(HOT2$Option_2)

  expect_error(marksim(
    data = HOT2, opts = c(Option_1:None), group = Group,
    method = "sop"
  ))
})

test_that("Error if alternatives is not numeric ", {
  HOT2 <- HOT

  HOT2$Option_2 <- as.character(HOT2$Option_2)

  expect_error(marksim(
    data = HOT2, opts = c(Option_1:None), group = Group,
    method = "fc"
  ))
})

##
test_that("Error if wrong method specified ", {
  expect_error(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "abc"
  ))
})

test_that("No error if no method specified ", {
  expect_no_error(marksim(data = HOT, opts = c(Option_1:None)))
})

##

##
test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )))
})

test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )))
})
##


##
test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )))
})
##

##
test_that("Length of output equals number of alternatives ", {
  expect_equal(
    nrow(marksim(
      data = HOT, opts = c(Option_1:None),
      method = "sop"
    )),
    length(HOT %>% dplyr::select(c(Option_1:None)))
  )
})

test_that("Length of output equals number of alternatives ", {
  expect_equal(
    nrow(marksim(
      data = HOT, opts = c(Option_1:None),
      method = "fc"
    )),
    length(HOT %>% dplyr::select(c(Option_1:None)))
  )
})
##


##
test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(
    nrow(marksim(
      data = HOT, opts = c(Option_1:None),
      group = Group, method = "sop"
    )),
    (length(unique(HOT$Group)) * length(HOT %>% dplyr::select(c(Option_1:None))))
  )
})

test_that("Length of output equals number of groups - 1 group ", {
  expect_equal(
    nrow(marksim(
      data = HOT, opts = c(Option_1:None),
      group = Group, method = "fc"
    )),
    (length(unique(HOT$Group)) * length(HOT %>% dplyr::select(c(Option_1:None))))
  )
})
##


##
test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(
    nrow(marksim(
      data = HOT, opts = c(Option_1:None),
      group = c(Group, Group2),
      method = "sop"
    )),
    (length(unique(HOT$Group)) * length(unique(HOT$Group2)) * length(HOT %>% dplyr::select(c(Option_1:None))))
  )
})

test_that("Length of output equals number of groups - 2 group ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(
    nrow(marksim(
      data = HOT, opts = c(Option_1:None),
      group = c(Group, Group2),
      method = "fc"
    )),
    (length(unique(HOT$Group)) * length(unique(HOT$Group2)) * length(HOT %>% dplyr::select(c(Option_1:None))))
  )
})
##

##
test_that("Check output format ", {
  expect_true(is.character(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[1]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[2]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[5]]))

  expect_true(is.character(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[1]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[2]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[5]]))
})

##
test_that("Check output - 1 group ", {
  expect_true(is.character(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "sop"
  )[[2]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "sop"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "sop"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "sop"
  )[[5]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "sop"
  )[[6]]))


  expect_true(is.character(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "fc"
  )[[2]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "fc"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "fc"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "fc"
  )[[5]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "fc"
  )[[6]]))
})

##
test_that("group output equals group input ", {
  expect_equal(utils::str(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "sop"
  )[[1]]), utils::str(HOT$Group))

  expect_equal(utils::str(marksim(
    data = HOT, opts = c(Option_1:None), group = Group,
    method = "fc"
  )[[1]]), utils::str(HOT$Group))
})
##

##
test_that("group output equals group input - character input ", {
  HOT$Group2 <- c("Group 1", "Group 2")
  expect_equal(utils::str(marksim(
    data = HOT, opts = c(Option_1:None), group = Group2,
    method = "sop"
  )[[1]]), utils::str(HOT$Group2))

  expect_equal(utils::str(marksim(
    data = HOT, opts = c(Option_1:None), group = Group2,
    method = "fc"
  )[[1]]), utils::str(HOT$Group2))
})

##

##
test_that("group output equals group input - labelled input ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(marksim(
    data = HOT, opts = c(Option_1:None), group = Group2,
    method = "sop"
  )[[1]]))

  expect_true(labelled::is.labelled(marksim(
    data = HOT, opts = c(Option_1:None), group = Group2,
    method = "fc"
  )[[1]]))
})
##

##
test_that("group output equals group input - multiple grouping variables ", {
  HOT$Group2 <- c(1:2)
  HOT$Group2 <- labelled::labelled(HOT$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(utils::str(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[1]]), utils::str(HOT$Group))
  expect_true(labelled::is.labelled(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[2]]))
  expect_true(is.character(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[5]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[6]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "sop"
  )[[7]]))


  expect_equal(utils::str(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[1]]), utils::str(HOT$Group))
  expect_true(labelled::is.labelled(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[2]]))
  expect_true(is.character(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[5]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[6]]))
  expect_true(is.numeric(marksim(
    data = HOT, opts = c(Option_1:None), group = c(Group, Group2),
    method = "fc"
  )[[7]]))
})
##

##
test_that("marksim() also working with data.frame not created with createHOT()", {
  set.seed(2023)

  newHOT <- data.frame(
    Option_1 = stats::runif(10, min = -5, max = 5),
    Option_2 = stats::runif(10, min = -5, max = 5),
    Option_3 = stats::runif(10, min = -5, max = 5),
    Option_4 = stats::runif(10, min = -5, max = 5),
    Option_5 = stats::runif(10, min = -5, max = 5),
    Choice = sample(c(1:5), 10, replace = T)
  )
  expect_true(is.character(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[1]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[2]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[5]]))
  expect_true(tibble::is_tibble(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )))
  expect_false(anyNA(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )))
  expect_equal(sum(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "sop"
  )[[2]]), 100)

  expect_true(is.character(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )[[1]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )[[2]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )[[3]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )[[4]]))
  expect_true(is.numeric(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )[[5]]))
  expect_true(tibble::is_tibble(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )))
  expect_false(anyNA(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )))
  expect_equal(sum(marksim(
    data = newHOT, opts = c(Option_1:Option_5),
    method = "fc"
  )[[2]]), 100)
})
##

##
test_that("check whether examples are correct ", {
  expect_equal(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[1]], HOT %>% dplyr::select(c(Option_1:None)) %>% colnames())
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[2]], 1), c(18.3, 11.3, 4.1, 32.5, 1.9, 10.4, 5.6, 16.0))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[3]], 1), c(4.1, 2.7, 1.5, 4.4, 0.9, 2.7, 1.7, 3.3))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[4]], 1), c(10.2, 6.0, 1.2, 23.8, 0.1, 5.1, 2.1, 9.5))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[5]], 1), c(26.3, 16.6, 7.0, 41.2, 3.7, 15.6, 9.0, 22.4))

  expect_equal(sum(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[2]]), 100)
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[4]], 1) < round(marksim(data = HOT, opts = c(Option_1:None))[[5]], 1), rep(TRUE, 8))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[2]], 1) < round(marksim(data = HOT, opts = c(Option_1:None))[[5]], 1), rep(TRUE, 8))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "sop"
  )[[2]], 1) > round(marksim(data = HOT, opts = c(Option_1:None))[[4]], 1), rep(TRUE, 8))


  expect_equal(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[1]], HOT %>% dplyr::select(c(Option_1:None)) %>% colnames())
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[2]], 1), c(18.6, 10.0, 2.9, 38.6, 1.4, 8.6, 2.9, 17.1))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[3]], 1), c(4.7, 3.6, 2.0, 5.9, 1.4, 3.4, 2.0, 4.5))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[4]], 1), c(9.4, 2.9, -1.1, 27.1, -1.4, 2.0, -1.1, 8.3))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[5]], 1), c(27.7, 17.1, 6.8, 50.1, 4.2, 15.2, 6.8, 26.0))

  expect_equal(sum(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[2]]), 100)
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[4]], 1) < round(marksim(data = HOT, opts = c(Option_1:None))[[5]], 1), rep(TRUE, 8))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[2]], 1) < round(marksim(data = HOT, opts = c(Option_1:None))[[5]], 1), rep(TRUE, 8))
  expect_equal(round(marksim(
    data = HOT, opts = c(Option_1:None),
    method = "fc"
  )[[2]], 1) > round(marksim(data = HOT, opts = c(Option_1:None))[[4]], 1), rep(TRUE, 8))
})
##

##
test_that("if no method specified, share of preference should be used ", {
  expect_equal(
    marksim(
      data = HOT, opts = c(Option_1:None),
      method = "sop"
    ),
    marksim(data = HOT, opts = c(Option_1:None))
  )
})

test_that("if no method specified, share of preference should be used ", {
  expect_equal(
    marksim(
      data = HOT, opts = c(Option_1:None),
      method = "fc"
    )[[2]] ==
      marksim(data = HOT, opts = c(Option_1:None))[[2]],
    rep(FALSE, 8)
  )
})


test_that("first choice and share of preference different results (at least for
          example data) ", {
  expect_equal(
    marksim(
      data = HOT, opts = c(Option_1:None),
      method = "fc"
    )[[2]] ==
      marksim(
        data = HOT, opts = c(Option_1:None),
        method = "sop"
      )[[2]],
    rep(FALSE, 8)
  )
})
