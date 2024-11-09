test_that("With none one column more ", {
  expect_equal((length(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  )) -
    length(createHOT(
      data = MaxDiff,
      id = 1,
      prod.levels = list(3, 10, 11, 15, 16, 17, 18),
      choice = 20,
      method = "MaxDiff"
    ))
  ), 1)
})

test_that("Coding not needed for MaxDiff ", {
  expect_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    coding = rep(0, 7),
    method = "MaxDiff"
  ))
})

test_that("interpolate.leves not needed for MaxDiff ", {
  expect_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    interpolate.levels = list(c(20:30)),
    method = "MaxDiff"
  ))
})

test_that("piece.p not needed for MaxDiff ", {
  expect_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    piece.p = list(c(14, 15)),
    method = "MaxDiff"
  ))
})

test_that("lin.p not needed for MaxDiff ", {
  expect_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    lin.p = c(14, 15),
    method = "MaxDiff"
  ))
})


test_that("prod.levels not a list ", {
  expect_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = c(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  ))
})

test_that("coding missed CBC ", {
  expect_error(createHOT(
    data = CBC,
    id = 1,
    none = 21,
    prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
    method = "CBC",
    choice = 22
  ))
})

test_that("No piecewise coding for CBC ", {
  expect_error(createHOT(
    data = CBC,
    id = 1,
    none = 21,
    prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
    coding = c(0, 0, 2),
    method = "CBC",
    choice = 22
  ))
})


test_that("lin.p not missing ", {
  expect_error(createHOT(
    data = CBC,
    id = 1,
    none = 21,
    prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
    coding = c(0, 0, 1),
    method = "CBC",
    choice = 22
  ))
})

test_that("lin.p missing although coding states linear ", {
  expect_error(createHOT(
    data = CBC_lin,
    id = 1,
    none = 15,
    prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
    coding = c(0, 0, 1),
    method = "CBC",
    choice = 16
  ))
})

test_that("coding missed ACBC ", {
  expect_error(createHOT(
    data = ACBC,
    id = 1,
    none = 37,
    prod.levels = list(
      c(5, 11, 15, 17, 21, 25, 32, 34, 15.99),
      c(6, 9, 15, 17, 23, 27, 31, 34, 12.99),
      c(8, 12, 16, 19, 23, 24, 28, 34, 12.99),
      c(7, 12, 14, 18, 22, 24, 28, 33, 9.99),
      c(4, 10, 13, 17, 23, 27, 28, 34, 7.99),
      c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)
    ),
    interpolate.levels = list(c(2.093, 27.287)),
    piece.p = list(
      c(35, 36), c(35, 36), c(35, 36), c(35, 36),
      c(35, 36), c(35, 36)
    ),
    method = "ACBC",
    choice = 38
  ))
})

test_that("piece.p missed ACBC ", {
  expect_error(createHOT(
    data = ACBC,
    id = 1,
    none = 37,
    prod.levels = list(
      c(5, 11, 15, 17, 21, 25, 32, 34, 15.99),
      c(6, 9, 15, 17, 23, 27, 31, 34, 12.99),
      c(8, 12, 16, 19, 23, 24, 28, 34, 12.99),
      c(7, 12, 14, 18, 22, 24, 28, 33, 9.99),
      c(4, 10, 13, 17, 23, 27, 28, 34, 7.99),
      c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)
    ),
    interpolate.levels = list(c(2.093, 27.287)),
    coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 38
  ))
})

test_that("lin.p missed ACBC ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37),
      c(35, 36), c(35, 36), c(33, 34), c(33, 34), c(33, 34)
    ),
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})


test_that("lin.p specfied but no 1 in coding ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})

test_that("piece.p specfied but no 2 in coding ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 0),
    method = "ACBC",
    choice = 40
  ))
})

test_that("lin.p specified but missing 1 in coding ", {
  expect_error(createHOT(
    data = CBC_lin,
    id = 1,
    none = 15,
    prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
    coding = c(0, 0, 0),
    lin.p = 14,
    method = "CBC",
    choice = 16
  ))
})


test_that("Different numbers for coding not working ", {
  expect_error(createHOT(
    data = CBC_lin,
    id = 1,
    none = 15,
    prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
    coding = c(0, 0, 3),
    lin.p = 14,
    method = "CBC",
    choice = 20
  ))
})


test_that("Different numbers for coding not working ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36),
      c(35, 36), c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 3),
    method = "ACBC",
    choice = 40
  ))
})


test_that("variable of type character within prod.levels ", {
  MaxDiff2 <- MaxDiff
  MaxDiff2[[3]] <- as.character(MaxDiff2[[3]])

  expect_error(createHOT(
    data = MaxDiff2,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  ))
})

test_that("CBC test - variable of type character within prod.levels ", {
  CBC2 <- CBC
  CBC2[[4]] <- as.character(CBC2[[4]])

  expect_error(createHOT(
    data = CBC2,
    id = 1,
    none = 21,
    prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
    choice = 22,
    coding = c(0, 0, 0),
    method = "CBC"
  ))
})


test_that("MaxDiff - no character input in prod.levels ", {
  expect_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, "11", 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  ))
})

test_that("CBC test - no character input in prod.levels ", {
  expect_error(createHOT(
    data = CBC,
    id = 1,
    none = 21,
    prod.levels = list(c(4, 9, 19), c(8, 12, "17"), c(5, 10, 17)),
    choice = 22,
    coding = c(0, 0, 0),
    method = "CBC"
  ))
})



test_that("Interpolate levels needs to be a list ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = c(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})


test_that("Interpolate levels needs to be a list with numeric input ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(
        1.99, 6.99, 9.99, "10.99",
        12.99, 17.99, 25.99
      )
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})

test_that("Needs to specify correct method ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "abcd",
    choice = 40
  ))
})

test_that("Method needs to be specified ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    choice = 40
  ))
})

test_that("Coding only can have numeric input ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, "0", 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})


test_that("lin. p needs to be a vector with numeric variables ", {
  ACBC_interpolate2 <- ACBC_interpolate
  ACBC_interpolate2[[9]] <- as.character(ACBC_interpolate2[[9]])

  expect_error(createHOT(
    data = ACBC_interpolate2,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})


test_that("lin. p needs to be specified ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})



test_that("piece.p needs to be a list ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = c(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})


test_that("piece.p needs to be a list with numeric input ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, "36"), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})

test_that("piece.p needs to be a list with numeric input ", {
  ACBC_interpolate2 <- ACBC_interpolate
  ACBC_interpolate2[[36]] <- as.character(ACBC_interpolate2[[36]])

  expect_error(createHOT(
    data = ACBC_interpolate2,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})



test_that("Number of products equals number of output starting with Option_", {
  HOT <- createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  )

  expect_true(length(
    colnames(HOT %>%
      dplyr::select(dplyr::starts_with("Option_")))
  ) == 7)
})

test_that("ACBC - Number of products equals number of output starting with Option_", {
  HOT <- createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  )

  expect_true(length(
    colnames(HOT %>%
      dplyr::select(dplyr::starts_with("Option_")))
  ) == 6)
})

test_that("First column id", {
  HOT <- createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  )

  expect_equal(colnames(HOT)[1], "ID")
})


test_that("Extrapolation not possible for piecewise ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 26.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})

test_that("Extrapolation not possible for linear ", {
  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 12, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  ))
})

test_that("if none specified also named in the output ", {
  HOT <- createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  )

  expect_true(any(colnames(HOT) == "None"))
})

test_that("if none not specified also not named in the output ", {
  HOT <- createHOT(
    data = ACBC_interpolate,
    id = 1,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  )

  expect_false(any(colnames(HOT) == "none"))
})

test_that("varskeep counted corectly ", {
  varskeep <- 41

  expect_true(abs(ncol(createHOT(
    data = ACBC_interpolate,
    id = 1,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40
  )) - ncol(createHOT(
    data = ACBC_interpolate,
    id = 1,
    prod.levels = list(
      c(5, 5, 12, 14, 18, 22, 29, 31, 15.99),
      c(6, 4, 12, 14, 20, 24, 28, 31, 12.99),
      c(8, 6, 13, 16, 20, 21, 25, 31, 12.99),
      c(7, 5, 11, 15, 19, 21, 25, 30, 9.99),
      c(4, 9, 10, 14, 20, 24, 25, 31, 7.99),
      c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
    ),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36),
      c(33, 34), c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    varskeep = varskeep,
    choice = 40
  ))) == length(varskeep))
})



test_that("Make sure varskeep is working ", {
  expect_no_error(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    method = "MaxDiff",
    choice = 20,
    varskeep = 21
  ))
})

test_that("Make sure varskeep is working ", {
  expect_no_error(createHOT(
    data = CBC,
    id = 1,
    none = 21,
    prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
    coding = c(0, 0, 0),
    method = "CBC",
    choice = 22,
    varskeep = 23
  ))
})


test_that("Make sure varskeep is working ", {
  expect_no_error(createHOT(
    data = CBC_lin,
    id = 1,
    none = 15,
    prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
    interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
    lin.p = 14,
    coding = c(0, 0, 1),
    method = "CBC",
    varskeep = 17,
    choice = 16
  ))
})


test_that("Make sure varskeep is working ", {
  prod1 <- c(5, 11, 15, 17, 21, 25, 32, 34, 15.99)
  prod2 <- c(6, 9, 15, 17, 23, 27, 31, 34, 12.99)
  prod3 <- c(8, 12, 16, 19, 23, 24, 28, 34, 12.99)
  prod4 <- c(7, 12, 14, 18, 22, 24, 28, 33, 9.99)
  prod5 <- c(4, 10, 13, 17, 23, 27, 28, 34, 7.99)
  prod6 <- c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)

  expect_no_error(createHOT(
    data = ACBC,
    id = 1,
    none = 37,
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    interpolate.levels = list(c(2.093, 27.287)),
    piece.p = list(
      c(35, 36), c(35, 36), c(35, 36), c(35, 36),
      c(35, 36), c(35, 36)
    ),
    coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 38,
    varskeep = 39
  ))
})



test_that("Make sure varskeep is working ", {
  prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
  prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
  prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
  prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
  prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
  prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)

  expect_no_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40,
    varskeep = 41
  ))
})


# Add missing tests
test_that("piece.p only takes numeric values ", {
  prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
  prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
  prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
  prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
  prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
  prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)

  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c("36", 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40,
    varskeep = 41
  ))
})


test_that("Variables in piece.p only have to be numeric ", {
  prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
  prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
  prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
  prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
  prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
  prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)

  ACBC_interpolate[3, 36] <- as.character(ACBC_interpolate[3, 36])

  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40,
    varskeep = 41
  ))
})

test_that("Variables in piece.p only have to be numeric ", {
  prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
  prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
  prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
  prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
  prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
  prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)

  ACBC_interpolate[3, 36] <- as.character(ACBC_interpolate[3, 36])

  expect_error(createHOT(
    data = ACBC_interpolate,
    id = 1,
    none = 39,
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    interpolate.levels = list(
      c(3, 5, 8, 10),
      c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
    ),
    piece.p = list(
      c(36, 37), c(35, 36), c(35, 36), c(33, 34),
      c(33, 34), c(33, 34)
    ),
    lin.p = 9,
    coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 40,
    varskeep = 41
  ))
})

test_that("no characters if coding equals 1 ", {
  expect_error(createHOT(
    data = CBC_lin,
    id = 1,
    none = 15,
    prod.levels = list(c("Att1_Lev1", 9, 60), c(8, 12, 40), c(5, 10, 45)),
    coding = c(0, 0, 1),
    lin.p = 14,
    method = "CBC",
    choice = 16
  ))
})


test_that("no characters if coding equals 2 ", {
  prod1 <- c("Att1_Lev2", 11, 15, 17, 21, 25, 32, 34, 15.99)
  prod2 <- c(6, 9, 15, 17, 23, 27, 31, 34, 12.99)
  prod3 <- c(8, 12, 16, 19, 23, 24, 28, 34, 12.99)
  prod4 <- c(7, 12, 14, 18, 22, 24, 28, 33, 9.99)
  prod5 <- c(4, 10, 13, 17, 23, 27, 28, 34, 7.99)
  prod6 <- c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)

  expect_error(createHOT(
    data = ACBC,
    id = 1,
    none = 37,
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    interpolate.levels = list(c(2.093, 27.287)),
    piece.p = list(
      c(35, 36), c(35, 36), c(35, 36), c(35, 36),
      c(35, 36), c(35, 36)
    ),
    coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
    method = "ACBC",
    choice = 38
  ))
})


test_that("ID input equals output ", {
  MaxDiff2 <- MaxDiff
  MaxDiff2$ID <- as.character(MaxDiff2$ID)
  HOT <- createHOT(
    data = MaxDiff2,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  )


  expect_equal(str(HOT$ID), str(MaxDiff2$ID))
})

test_that("ID input equals output ", {
  MaxDiff2 <- MaxDiff
  MaxDiff2$ID <- as.numeric(MaxDiff2$ID)
  HOT <- createHOT(
    data = MaxDiff2,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    choice = 20,
    method = "MaxDiff"
  )


  expect_equal(str(HOT$ID), str(MaxDiff2$ID))
})


test_that("Warning if 'choice' is not defined ", {
  expect_warning(createHOT(
    data = MaxDiff,
    id = 1,
    none = 19,
    prod.levels = list(3, 10, 11, 15, 16, 17, 18),
    method = "MaxDiff"
  ))
})
