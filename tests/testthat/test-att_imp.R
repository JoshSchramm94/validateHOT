test_that("Coding missing ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    res = "agg"
  ))
})

test_that("Coding not 0, 1, or 2 ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 3),
    res = "agg"
  ))
})

test_that("Coding not 0, 1, or 2 ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, "a"),
    res = "agg"
  ))
})

test_that("Coding 1 and interpolate.levels is missing ", {
  expect_error(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lin")
    ),
    coding = c(0, 0, 1),
    res = "agg"
  ))
})

test_that("Linear coded variables not equal to length of interpolate.levels ", {
  expect_error(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1"),
      c("Att3_Lin")
    ),
    coding = c(0, 1, 1),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  ))
})

test_that("Linear coded variables has more than 1 level ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lev1", "Att3_Lev2")
    ),
    coding = c(0, 0, 1),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  ))
})

test_that("More than 1 variables code 2 ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lev1", "Att3_Lev2")
    ),
    coding = c(0, 2, 2),
    res = "agg"
  ))
})


test_that("Warning if group contains NA ", {
  CBC2 <- CBC

  CBC2$Group[34] <- NA

  expect_warning(att_imp(
    data = CBC2,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    group = Group,
    res = "agg"
  ))
})

test_that("Test input for interpolate.levels ", {
  expect_error(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lin")
    ),
    coding = c(0, 0, 1),
    interpolate.levels = c(10, 20, 30, 40, 50, 60, 70),
    res = "agg"
  ))
})

test_that("Test input for interpolate.levels ", {
  expect_error(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lin")
    ),
    coding = c(0, 0, 1),
    interpolate.levels = list(c(10, "a", 30, 40, 50, 60, 70)),
    res = "agg"
  ))
})

test_that("Input for interpolate.levels can not be larger than 'attrib' ", {
  expect_error(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1"),
      c("Att2_Lev1"),
      c("Att3_Lin")
    ),
    coding = c(1, 1, 1),
    interpolate.levels = list(
      c(10, 20, 30, 40, 50, 60, 70),
      c(10, 20, 30, 40, 50, 60, 70),
      c(10, 20, 30, 40, 50, 60, 70),
      c(10, 20, 30, 40, 50, 60, 70)
    ),
    res = "agg"
  ))
})

test_that("Interpolate.levels only for linear coded variables ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lev1", "Att3_Lev2")
    ),
    coding = c(0, 0, 0),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  ))
})

test_that("Interpolate.levels only for linear coded variables ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1"),
      c("Att3_Lev1")
    ),
    coding = c(0, 0, 0),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  ))
})


test_that("Structure of Output data.frame ", {
  expect_true(is.data.frame(
    att_imp(
      data = CBC,
      attrib = list(
        c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
        c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
        c(
          "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
          "Att3_Lev6", "Att3_Lev7"
        )
      ),
      coding = c(0, 0, 0),
      res = "agg"
    )
  ))
})

test_that("Structure of Output tibble ", {
  expect_true(tibble::is_tibble(
    att_imp(
      data = CBC,
      attrib = list(
        c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
        c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
        c(
          "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
          "Att3_Lev6", "Att3_Lev7"
        )
      ),
      coding = c(0, 0, 0),
      res = "agg"
    )
  ))
})

test_that("group output equals group input ", {
  expect_equal(utils::str(
    att_imp(
      data = CBC,
      attrib = list(
        c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
        c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
        c(
          "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
          "Att3_Lev6", "Att3_Lev7"
        )
      ),
      coding = c(0, 0, 0),
      group = Group,
      res = "agg"
    )[[1]]
  ), utils::str(CBC$Group))
})

test_that("group output equals group input - character input ", {
  CBC$Group2 <- rep(c("Group 1", "Group 2"), length.out = nrow(CBC))
  expect_equal(utils::str(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    group = Group2,
    res = "agg"
  )[[1]]), utils::str(CBC$Group2))
})

test_that("group output equals group input - labelled input ", {
  CBC$Group2 <- rep(c(1:2), length.out = nrow(CBC))
  CBC$Group2 <- labelled::labelled(CBC$Group2,
    labels = c("Group 1" = 1, "Group 2" = 2)
  )
  expect_true(labelled::is.labelled(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    group = Group2,
    res = "agg"
  )[[1]]))
})

test_that("check whether examples are correct ", {
  expect_equal(round(as.numeric(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    res = "agg"
  )[[2]]), 1), c(35.7, 27.7, 36.6))

  expect_equal(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    res = "agg"
  )[[1]], c("att_imp_1", "att_imp_2", "att_imp_3"))

  expect_equal(round(as.numeric(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    res = "agg"
  )[[3]]), 1), c(11.3, 10.0, 9.3))
})


test_that("check whether examples are correct - CBC_lin ", {
  expect_equal(round(as.numeric(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lin")
    ),
    coding = c(0, 0, 1),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  )[[2]]), 1), c(36.9, 31.9, 31.3))

  expect_equal(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lin")
    ),
    coding = c(0, 0, 1),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  )[[1]], c("att_imp_1", "att_imp_2", "att_imp_3"))

  expect_equal(round(as.numeric(att_imp(
    data = CBC_lin,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c("Att3_Lin")
    ),
    coding = c(0, 0, 1),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    res = "agg"
  )[[3]]), 1), c(12.4, 13.4, 16.4))
})


test_that("res missing ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0)
  ))
})

test_that("res noz specified to ind or agg ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    res = "xyz"
  ))
})

test_that("group can not be specified when res set to ind ", {
  expect_error(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    res = "ind", group = "Group"
  ))
})


test_that("rows of inputs equals rows of output ", {
  expect_equal(nrow(att_imp(
    data = CBC,
    attrib = list(
      c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
      c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
      c(
        "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
        "Att3_Lev6", "Att3_Lev7"
      )
    ),
    coding = c(0, 0, 0),
    res = "ind"
  )), nrow(CBC))
})


test_that("each row equals 100 ", {
  expect_equal(
    rowSums(att_imp(
      data = CBC,
      attrib = list(
        c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
        c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
        c(
          "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4", "Att3_Lev5",
          "Att3_Lev6", "Att3_Lev7"
        )
      ),
      coding = c(0, 0, 0),
      res = "ind"
    )),
    rep(100, nrow(CBC))
  )
})
