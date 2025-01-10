# check for error messages for missing arguments -------------------------------
test_that("Error if coding is missing ", {
  expect_error(att_imp(
    data = cbc,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      paste0("price_", c(1:6))
    ),
    # coding = c(rep(0, times = 9)),
    res = "agg"
  ))
})

test_that("Error if res is missing ", {
  expect_error(att_imp(
    data = cbc,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      paste0("price_", c(1:6))
    ),
    coding = c(rep(0, times = 9)),
    # res = "agg"
  ))
})

test_that("Error if attrib is missing ", {
  expect_error(att_imp(
    data = cbc,
    # attrib = list(
    #   paste0("att1_lev", c(1:3)),
    #   paste0("att2_lev", c(1:2)),
    #   paste0("att3_lev", c(1:4)),
    #   paste0("att4_lev", c(1:4)),
    #   paste0("att5_lev", c(1:2)),
    #   paste0("att6_lev", c(1:4)),
    #   paste0("att7_lev", c(1:6)),
    #   paste0("att8_lev", c(1:6)),
    #   paste0("price_", c(1:6))
    # ),
    coding = c(rep(0, times = 9)),
    res = "agg"
  ))
})

test_that("coding provided but interpolate.levels missing ", {
  expect_error(att_imp(
    data = cbc_linear,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      "price"
    ),
    coding = c(rep(0, times = 8), 1),
    # interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    res = "agg"
  ))
})

# end --------------------------------------------------------------------------

# check for error messages for wrong input -------------------------------------

test_that("coding not 0, 1, 2 ", {
  expect_error(att_imp(
    data = cbc_linear,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      "price"
    ),
    coding = c(rep(0, times = 8), 3),
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    res = "agg"
  ))
})

test_that("more than 2 piecewise coded attributes provided ", {
  expect_error(att_imp(
    data = acbc,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      paste0("price_", c(1:2))
    ),
    coding = c(0, 2, rep(0, times = 6), 2),
    res = "agg"
  ))
})

test_that("Linear coded variables not equal to length of
          interpolate.levels ", {
  expect_error(att_imp(
    data = cbc_linear,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      "price"
    ),
    coding = c(rep(0, times = 7), 1, 1),
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    res = "agg"
  ))
})

test_that("attrib includes non-numeric variables ", {
  cbc_linear$att1_lev1 <- as.character(cbc_linear$att1_lev1)

  expect_error(att_imp(
    data = cbc_linear,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      "price"
    ),
    coding = c(rep(0, times = 8), 1),
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    res = "agg"
  ))
})

test_that("interpolate.levels must be a list ", {
  expect_error(att_imp(
    data = cbc_linear,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      "price"
    ),
    coding = c(rep(0, times = 8), 1),
    interpolate.levels = c(seq(from = 175.99, to = 350.99, by = 35)),
    res = "agg"
  ))
})

# end --------------------------------------------------------------------------

# group input equals group output ----------------------------------------------
test_that("group output equals group input ", {
  expect_equal(str(att_imp(
    data = cbc,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      paste0("price_", c(1:6))
    ),
    coding = c(rep(0, times = 9)),
    res = "agg",
    group = group
  )[[1]]), str(cbc$group))
})

test_that("group output equals group input - character input ", {
  cbc$group2 <- rep(c("group 1", "group 2"), length.out = nrow(cbc))

  expect_equal(str(att_imp(
    data = cbc,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      paste0("price_", c(1:6))
    ),
    coding = c(rep(0, times = 9)),
    res = "agg",
    group = group2
  )[[1]]), str(cbc$group2))
})

test_that("group output equals group input - labelled input ", {
  cbc$group2 <- rep(c(1:2), length.out = nrow(cbc))
  cbc$group2 <- labelled::labelled(cbc$group2,
    labels = c("group 1" = 1, "group 2" = 2)
  )
  expect_true(labelled::is.labelled(att_imp(
    data = cbc,
    attrib = list(
      paste0("att1_lev", c(1:3)),
      paste0("att2_lev", c(1:2)),
      paste0("att3_lev", c(1:4)),
      paste0("att4_lev", c(1:4)),
      paste0("att5_lev", c(1:2)),
      paste0("att6_lev", c(1:4)),
      paste0("att7_lev", c(1:6)),
      paste0("att8_lev", c(1:6)),
      paste0("price_", c(1:6))
    ),
    coding = c(rep(0, times = 9)),
    res = "agg",
    group = group2
  )[[1]]))
})
# end --------------------------------------------------------------------------
