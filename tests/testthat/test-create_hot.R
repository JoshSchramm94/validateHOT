# check for error messages for missing arguments -------------------------------
test_that("Error if prod.levels is missing ", {
  expect_error(create_hot(
    data = maxdiff,
    id = "id",
    none = "none",
    # prod.levels = list(2, 9, 10, 14, 15, 16, 17),
    method = "maxdiff",
    varskeep = "group",
    choice = "hot"
  ))
})

test_that("Error if id is missing ", {
  expect_error(create_hot(
    data = maxdiff,
    # id = "id",
    none = "none",
    prod.levels = list(2, 9, 10, 14, 15, 16, 17),
    method = "maxdiff",
    varskeep = "group",
    choice = "hot"
  ))
})

test_that("Error if method is missing ", {
  expect_error(create_hot(
    data = maxdiff,
    id = "id",
    none = "none",
    prod.levels = list(2, 9, 10, 14, 15, 16, 17),
    # method = "maxdiff",
    varskeep = "group",
    choice = "hot"
  ))
})

test_that("Warning if choice is missing ", {
  expect_warning(create_hot(
    data = maxdiff,
    id = "id",
    none = "none",
    prod.levels = list(2, 9, 10, 14, 15, 16, 17),
    method = "maxdiff",
    varskeep = "group",
    # choice = "hot"
  ))
})

# end --------------------------------------------------------------------------

# check for wrong input --------------------------------------------------------

test_that("Coding not needed for maxdiff ", {
  expect_error(create_hot(
    data = maxdiff,
    id = "id",
    none = "none",
    prod.levels = list(2, 9, 10, 14, 15, 16, 17),
    method = "maxdiff",
    varskeep = "group",
    choice = "hot",
    coding = c(rep(0, times = 7))
  ))
})

test_that("prod.levels not a list ", {
  expect_error(create_hot(
    data = maxdiff,
    id = "id",
    none = "none",
    prod.levels = c(2, 9, 10, 14, 15, 16, 17),
    method = "maxdiff",
    varskeep = "group",
    choice = "hot"
  ))
})

test_that("coding not specified for cbc ", {
  expect_error(create_hot(
    data = cbc,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 35),
      c(3, 5, 10, 14, 16, 18, 22, 27, 35),
      c(4, 6, 9, 14, 15, 20, 25, 30, 36),
      c(4, 5, 10, 11, 16, 19, 26, 32, 34),
      c(2, 6, 8, 14, 16, 17, 26, 31, 36),
      c(2, 5, 7, 12, 16, 20, 26, 29, 33)
    ),
    # coding = c(rep(0, times = 9)),
    method = "cbc",
    choice = "hot"
  ))
})

test_that("lin.p not specified if codong set to 1 ", {
  expect_error(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 1),
    # lin.p = 33,
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    method = "cbc",
    choice = "hot"
  ))
})

test_that("interpolate.levels not a list input ", {
  expect_error(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 1),
    lin.p = 33,
    interpolate.levels = c(seq(from = 175.99, to = 350.99, by = 35)),
    method = "cbc",
    choice = "hot"
  ))
})

test_that("interpolate.levels not specified if codong set to 1 ", {
  expect_error(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 1),
    lin.p = 33,
    # interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    method = "cbc",
    choice = "hot"
  ))
})

test_that("piece.p missed for acbc ", {
  prod1 <- c(3, 6, 10, 13, 16, 20, 24, 32, 248.55)
  prod2 <- c(3, 5, 10, 14, 16, 18, 22, 27, 237.39)
  prod3 <- c(4, 6, 9, 14, 15, 20, 25, 30, 273.15)
  prod4 <- c(4, 5, 10, 11, 16, 19, 26, 32, 213.55)
  prod5 <- c(2, 6, 8, 14, 16, 17, 26, 31, 266.10)
  prod6 <- c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)

  expect_error(create_hot(
    data = acbc,
    id = "id",
    none = "none",
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    coding = c(rep(0, times = 8), 2),
    interpolate.levels = list(c(121.95, 507.95)),
    # piece.p = list(
    #   list(c(33, 34), c(33, 34), c(33, 34),
    #   c(33, 34), c(33, 34), c(33, 34))
    # ),
    method = "acbc",
    choice = "hot"
  ))
})

test_that("piece.p not a nested list ", {
  prod1 <- c(3, 6, 10, 13, 16, 20, 24, 32, 248.55)
  prod2 <- c(3, 5, 10, 14, 16, 18, 22, 27, 237.39)
  prod3 <- c(4, 6, 9, 14, 15, 20, 25, 30, 273.15)
  prod4 <- c(4, 5, 10, 11, 16, 19, 26, 32, 213.55)
  prod5 <- c(2, 6, 8, 14, 16, 17, 26, 31, 266.10)
  prod6 <- c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)

  expect_error(create_hot(
    data = acbc,
    id = "id",
    none = "none",
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    coding = c(rep(0, times = 8), 2),
    interpolate.levels = list(c(121.95, 507.95)),
    piece.p = c(33, 34),
    method = "acbc",
    choice = "hot"
  ))
})

test_that("piece.p not a nested list ", {
  prod1 <- c(3, 6, 10, 13, 16, 20, 24, 32, 248.55)
  prod2 <- c(3, 5, 10, 14, 16, 18, 22, 27, 237.39)
  prod3 <- c(4, 6, 9, 14, 15, 20, 25, 30, 273.15)
  prod4 <- c(4, 5, 10, 11, 16, 19, 26, 32, 213.55)
  prod5 <- c(2, 6, 8, 14, 16, 17, 26, 31, 266.10)
  prod6 <- c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)

  expect_error(create_hot(
    data = acbc,
    id = "id",
    none = "none",
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    coding = c(rep(0, times = 8), 2),
    interpolate.levels = list(c(121.95, 507.95)),
    piece.p = list(c(33, 34)),
    method = "acbc",
    choice = "hot"
  ))
})

test_that("method not defined correctly ", {
  expect_error(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 1),
    lin.p = 33,
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    method = "test",
    choice = "hot"
  ))
})

test_that("coding not defined correctly ", {
  expect_error(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 3),
    lin.p = 33,
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    method = "cbc",
    choice = "hot"
  ))
})

test_that("character variables in prod.levels ", {
  maxdiff[, 2] <- as.character(maxdiff[, 2])

  expect_error(create_hot(
    data = maxdiff,
    id = "id",
    none = "none",
    prod.levels = list(2, 9, 10, 14, 15, 16, 17),
    method = "maxdiff",
    varskeep = "group",
    choice = "hot",
    coding = c(rep(0, times = 7))
  ))
})

test_that("warning if extrapolation is applied ", {
  expect_warning(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c(3, 6, 10, 13, 16, 20, 24, 32, 460),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 1),
    lin.p = 33,
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    method = "cbc",
    choice = "hot"
  ))
})

test_that("Error if extrapolation is applied for piecewise coded ", {

  prod1 <- c(3, 6, 10, 13, 16, 20, 24, 32, 248.55)
  prod2 <- c(3, 5, 10, 14, 16, 18, 22, 27, 237.39)
  prod3 <- c(4, 6, 9, 14, 15, 20, 25, 30, 273.15)
  prod4 <- c(4, 5, 10, 11, 16, 19, 26, 32, 213.55)
  prod5 <- c(2, 6, 8, 14, 16, 17, 26, 31, 266.10)
  prod6 <- c(2, 5, 7, 12, 16, 20, 26, 29, 100)

  expect_error(create_hot(
    data = acbc,
    id = "id",
    none = "none",
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    coding = c(rep(0, times = 8), 2),
    interpolate.levels = list(c(121.95, 507.95)),
    piece.p = list(
       list(
         c(33, 34), c(33, 34), c(33, 34),
         c(33, 34), c(33, 34), c(33, 34)
       )
    ),
    method = "acbc",
    choice = "hot"
  ))
})

test_that("No error if piecewise coded level equals range limits ", {

  prod1 <- c(3, 6, 10, 13, 16, 20, 24, 32, 507.95)
  prod2 <- c(3, 5, 10, 14, 16, 18, 22, 27, 237.39)
  prod3 <- c(4, 6, 9, 14, 15, 20, 25, 30, 273.15)
  prod4 <- c(4, 5, 10, 11, 16, 19, 26, 32, 213.55)
  prod5 <- c(2, 6, 8, 14, 16, 17, 26, 31, 266.10)
  prod6 <- c(2, 5, 7, 12, 16, 20, 26, 29, 121.95)

  expect_equal(class(create_hot(
    data = acbc,
    id = "id",
    none = "none",
    prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
    coding = c(rep(0, times = 8), 2),
    interpolate.levels = list(c(121.95, 507.95)),
    piece.p = list(
      list(
        c(33, 34), c(33, 34), c(33, 34),
        c(33, 34), c(33, 34), c(33, 34)
      )
    ),
    method = "acbc",
    choice = "hot"
  )), "data.frame")
})

test_that("only column indexes if linear-coded attributes ", {
  expect_error(create_hot(
    data = cbc_linear,
    id = "id",
    none = "none",
    prod.levels = list(
      c("att1_lev1", 6, 10, 13, 16, 20, 24, 32, 248.55),
      c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
      c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
      c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
      c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
      c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
    ),
    coding = c(rep(0, times = 8), 1),
    lin.p = 33,
    interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
    method = "cbc",
    choice = "hot"
  ))
})

# end --------------------------------------------------------------------------
