test_that("items missing ", {
  expect_error(prob_scores(
    data = MaxDiff,
    set.size = 4,
    res = "agg"
  ))
})

test_that("Only 1 item ", {
  expect_error(prob_scores(
    data = MaxDiff,
    item = Option_01,
    set.size = 4,
    res = "agg"
  ))
})

test_that("Item not numeric ", {
  data2 <- MaxDiff
  data2$Option_01 <- as.character(data2$Option_01)
  expect_error(prob_scores(
    data = data2,
    items = c(Option_01:Option_16),
    set.size = 4,
    res = "agg"
  ))
})

test_that("Group contains NAs - warning ", {
  data2 <- MaxDiff
  data2$Group[34] <- NA
  expect_warning(prob_scores(
    data = data2,
    items = c(Option_01:Option_16),
    set.size = 4,
    group = Group,
    res = "agg"
  ))
})

test_that("Items no NAs ", {
  data2 <- MaxDiff
  data2$Option_01[34] <- NA
  expect_error(prob_scores(
    data = data2,
    items = c(Option_01:Option_16),
    set.size = 4,
    res = "agg"
  ))
})


test_that("set.size missing ", {
  expect_error(prob_scores(
    items = c(Option_01:Option_16),
    res = "agg"
  ))
})

test_that("set.size not numeric ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = "a",
    res = "agg"
  ))
})

test_that("set.size larger than items ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 17,
    res = "agg"
  ))
})

test_that("anchor needs to be numeric ", {
  data2 <- MaxDiff
  data2$none <- as.character(data2$none)
  expect_error(prob_scores(
    data = data2,
    items = c(Option_01:none),
    set.size = 4,
    anchor = "none",
    res = "agg"
  ))
})



test_that("group output equals group input ", {
  expect_equal(utils::str(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    group = Group,
    res = "agg"
  )[[1]]), utils::str(MaxDiff$Group))
})

test_that("group output equals group input - character input ", {
  MaxDiff$Group2 <- rep(c("Group 1", "Group 2"), length.out = nrow(MaxDiff))
  expect_equal(utils::str(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    group = Group2,
    res = "agg"
  )[[1]]), utils::str(MaxDiff$Group2))
})

test_that("group output equals group input - labelled input ", {
  MaxDiff$Group2 <- rep(c(1:2), length.out = nrow(MaxDiff))
  MaxDiff$Group2 <- labelled::labelled(MaxDiff$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_equal(labelled::is.labelled(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    group = Group2,
    res = "agg"
  )[[1]]), labelled::is.labelled(MaxDiff$Group2))
})


test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(
    prob_scores(
      data = MaxDiff,
      items = c(Option_01:Option_16),
      set.size = 4,
      res = "agg"
    )
  ))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(
    prob_scores(
      data = MaxDiff,
      items = c(Option_01:Option_16),
      set.size = 4,
      res = "agg"
    )
  ))
})


test_that("check whether examples are correct ", {
  expect_equal(round(as.numeric(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    res = "agg"
  )[[2]]), 1), c(7.2, 5.9, 6.4, 7.2, 6.1, 9.9, 5.8, 5.7, 5.6, 6.0, 5.9, 5.9, 7.4, 2.8, 6.6, 5.7))
})

test_that("check whether examples are correct ", {
  expect_equal(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    res = "agg"
  )[[1]], c(paste0("Option_0", c(1:9)), paste0("Option_", c(10:16))))
})

test_that("check whether examples are correct ", {
  expect_equal(round(as.numeric(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    res = "agg"
  )[[3]]), 1), c(11.5, 5.9, 6.7, 12.7, 8.5, 15.0, 7.7, 8.0, 8.9, 7.0, 8.5, 7.2, 6.3, 4.6, 8.2, 9.7))
})

test_that("anchor set to 100 ", {
  expect_equal(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = none,
    res = "agg"
  )[[17, 2]], 100)
})

test_that("anchor also working with column index ", {
  expect_equal(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = which(colnames(MaxDiff) == "none"),
    res = "agg"
  )[[17, 2]], 100)
})

test_that("anchor also working with column index ", {
  expect_equal(
    prob_scores(
      data = MaxDiff,
      items = c(Option_01:none),
      set.size = 4,
      anchor = which(colnames(MaxDiff) == "none"),
      res = "agg"
    ),
    prob_scores(
      data = MaxDiff,
      items = c(Option_01:none),
      set.size = 4,
      anchor = "none",
      res = "agg"
    )
  )
})

test_that("res missing ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = which(colnames(MaxDiff) == "none")
  ))
})

test_that("res noz specified to ind or agg ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = which(colnames(MaxDiff) == "none"),
    res = "xyz"
  ))
})

test_that("group can not be specified when res set to ind ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = which(colnames(MaxDiff) == "none"),
    res = "ind",
    group = "Group"
  ))
})


test_that("rows of inputs equals rows of output ", {
  expect_equal(nrow(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = which(colnames(MaxDiff) == "none"),
    res = "ind"
  )), nrow(MaxDiff))
})


test_that("none not part of items ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:Option_16),
    set.size = 4,
    anchor = which(colnames(MaxDiff) == "none"),
    res = "agg"
  ))
})

test_that("none not larger than 1 ", {
  expect_error(prob_scores(
    data = MaxDiff,
    items = c(Option_01:none),
    set.size = 4,
    anchor = c(which(colnames(MaxDiff) == "none"), "Option_16"),
    res = "agg"
  ))
})
