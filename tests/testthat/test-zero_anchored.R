test_that("items missing ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    res = "agg"
  ))
})

test_that("Only 1 item ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    item = Option_01,
    res = "agg"
  ))
})

test_that("Item not numeric ", {
  data2 <- MaxDiff
  data2$Option_01 <- as.character(data2$Option_01)
  expect_error(zero_anchored(
    data = data2,
    items = c(Option_01:Option_16),
    res = "agg"
  ))
})

test_that("Group contains NAs - warning ", {
  data2 <- MaxDiff
  data2$Group[34] <- NA
  expect_warning(zero_anchored(
    data = data2,
    items = c(Option_01:Option_16),
    group = Group,
    res = "agg"
  ))
})

test_that("Items no NAs ", {
  data2 <- MaxDiff
  data2$Option_01[34] <- NA
  expect_error(zero_anchored(
    data = data2,
    items = c(Option_01:Option_16),
    res = "agg"
  ))
})

test_that("anchor needs to be numeric ", {
  data2 <- MaxDiff
  data2$none <- as.character(data2$none)
  expect_error(zero_anchored(
    data = data2,
    items = c(Option_01:none),
    anchor = "none",
    res = "agg"
  ))
})


test_that("group output equals group input ", {
  expect_equal(utils::str(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    group = Group,
    res = "agg"
  )[[1]]), utils::str(MaxDiff$Group))
})

test_that("group output equals group input - character input ", {
  MaxDiff$Group2 <- rep(c("Group 1", "Group 2"), length.out = nrow(MaxDiff))
  expect_equal(utils::str(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    group = Group2,
    res = "agg"
  )[[1]]), utils::str(MaxDiff$Group2))
})

test_that("group output equals group input - labelled input ", {
  MaxDiff$Group2 <- rep(c(1:2), length.out = nrow(MaxDiff))
  MaxDiff$Group2 <- labelled::labelled(MaxDiff$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(labelled::is.labelled(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    group = Group2,
    res = "agg"
  )[[1]]), labelled::is.labelled(MaxDiff$Group2))
})


test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(
    zero_anchored(
      data = MaxDiff,
      items = c(Option_01:Option_16),
      res = "agg"
    )
  ))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(
    zero_anchored(
      data = MaxDiff,
      items = c(Option_01:Option_16),
      res = "agg"
    )
  ))
})


test_that("check whether examples are correct ", {
  tday <- validateHOT::MaxDiff
  expect_equal(round(zero_anchored(
    data = tday,
    items = c(Option_01:Option_16),
    res = "agg"
  )[[2]], 1), c(-7.0, 5.2, 8.8, 0.6, -1.3, 17.5, -3.1, -2.9, -9.7, 3.3, 0.3, 4.5, 14.9, -22.5, 0.8, -9.4))
})

test_that("check whether examples are correct ", {
  expect_equal(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    res = "agg"
  )[[1]], c(paste0("Option_0", c(1:9)), paste0("Option_", c(10:16))))
})

test_that("check whether examples are correct ", {
  tday <- validateHOT::MaxDiff
  expect_equal(round(as.numeric(zero_anchored(
    data = tday,
    items = c(Option_01:Option_16),
    res = "agg"
  )[[3]]), 1), c(40.5, 23.0, 19.7, 30.5, 30.2, 30.1, 31.7, 26.1, 26.0, 22.3, 24.6, 22.2, 18.1, 19.8, 26.3, 27.7))
})

test_that("anchor set to 0 ", {
  expect_equal(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = none,
    res = "agg"
  )[[17, 2]], 0)
})

test_that("anchor also working with column index ", {
  expect_equal(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = which(colnames(MaxDiff) == "none"),
    res = "agg"
  )[[17, 2]], 0)
})

test_that("anchor also working with column index ", {
  expect_equal(
    zero_anchored(
      data = MaxDiff,
      items = c(Option_01:none),
      anchor = which(colnames(MaxDiff) == "none"),
      res = "agg"
    ),
    zero_anchored(
      data = MaxDiff,
      items = c(Option_01:none),
      anchor = "none",
      res = "agg"
    )
  )
})

test_that("res missing ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = which(colnames(MaxDiff) == "none")
  ))
})

test_that("res noz specified to ind or agg ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = which(colnames(MaxDiff) == "none"),
    res = "xyz"
  ))
})

test_that("group can not be specified when res set to ind ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = which(colnames(MaxDiff) == "none"),
    res = "ind",
    group = "Group"
  ))
})


test_that("rows of inputs equals rows of output ", {
  expect_equal(nrow(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = which(colnames(MaxDiff) == "none"),
    res = "ind"
  )), nrow(MaxDiff))
})


test_that("none not part of items ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    anchor = which(colnames(MaxDiff) == "none"),
    res = "agg"
  ))
})

test_that("none not larger than 1 ", {
  expect_error(zero_anchored(
    data = MaxDiff,
    items = c(Option_01:none),
    anchor = c(which(colnames(MaxDiff) == "none"), "Option_16"),
    res = "agg"
  ))
})
