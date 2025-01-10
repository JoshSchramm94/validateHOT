#' ACBC data set with part-worth coded attributes, linear-coded attribute and
#' piecewise-coded price
#'
#' A data set with raw scores export for an adaptive choice-based conjoint
#' including a linear-coded attribute as well as piecewise-coded price.
#'
#' Data set is taken from the publication by Sablotny-Wackershauser et al.
#' (2024)
#'
#'
#' \describe{
#'   \item{id}{(integer) Unique identifier}
#'   \item{att1_lev1}{(numeric) Part-worth utility of level 1 of attribute 1}
#'   \item{att1_lev2}{(numeric) Part-worth utility of level 2 of attribute 1}
#'   \item{att1_lev3}{(numeric) Part-worth utility of level 3 of attribute 1}
#'   \item{att2}{(numeric) Part-worth utility of linear-coded attribute 2}
#'   \item{att3_lev1}{(numeric) Part-worth utility of level 1 of attribute 3}
#'   \item{att3_lev2}{(numeric) Part-worth utility of level 2 of attribute 3}
#'   \item{att3_lev3}{(numeric) Part-worth utility of level 3 of attribute 3}
#'   \item{att3_lev4}{(numeric) Part-worth utility of level 4 of attribute 3}
#'   \item{att4_lev1}{(numeric) Part-worth utility of level 1 of attribute 4}
#'   \item{att4_lev2}{(numeric) Part-worth utility of level 2 of attribute 4}
#'   \item{att4_lev3}{(numeric) Part-worth utility of level 3 of attribute 4}
#'   \item{att4_lev4}{(numeric) Part-worth utility of level 4 of attribute 4}
#'   \item{att5_lev1}{(numeric) Part-worth utility of level 1 of attribute 5}
#'   \item{att5_lev2}{(numeric) Part-worth utility of level 2 of attribute 5}
#'   \item{att6_lev1}{(numeric) Part-worth utility of level 1 of attribute 6}
#'   \item{att6_lev2}{(numeric) Part-worth utility of level 2 of attribute 6}
#'   \item{att6_lev3}{(numeric) Part-worth utility of level 3 of attribute 6}
#'   \item{att6_lev4}{(numeric) Part-worth utility of level 4 of attribute 6}
#'   \item{att7_lev1}{(numeric) Part-worth utility of level 1 of attribute 7}
#'   \item{att7_lev2}{(numeric) Part-worth utility of level 2 of attribute 7}
#'   \item{att7_lev3}{(numeric) Part-worth utility of level 3 of attribute 7}
#'   \item{att7_lev4}{(numeric) Part-worth utility of level 4 of attribute 7}
#'   \item{att7_lev5}{(numeric) Part-worth utility of level 5 of attribute 7}
#'   \item{att7_lev6}{(numeric) Part-worth utility of level 6 of attribute 7}
#'   \item{att8_lev1}{(numeric) Part-worth utility of level 1 of attribute 8}
#'   \item{att8_lev2}{(numeric) Part-worth utility of level 2 of attribute 8}
#'   \item{att8_lev3}{(numeric) Part-worth utility of level 3 of attribute 8}
#'   \item{att8_lev4}{(numeric) Part-worth utility of level 4 of attribute 8}
#'   \item{att8_lev5}{(numeric) Part-worth utility of level 5 of attribute 8}
#'   \item{att8_lev6}{(numeric) Part-worth utility of level 6 of attribute 8}
#'   \item{price_1}{(numeric) Part-worth utility for piecewise-coded
#'   price level 1}
#'   \item{price_2}{(numeric) Part-worth utility for piecewise-coded
#'   price level 2}
#'   \item{price_3}{(numeric) Part-worth utility for piecewise-coded
#'   price level 3}
#'   \item{price_4}{(numeric) Part-worth utility for piecewise-coded
#'   price level 4}
#'   \item{price_5}{(numeric) Part-worth utility for piecewise-coded
#'   price level 5}
#'   \item{none}{(numeric) Part-worth utility of outside good (no-buy option)}
#'   \item{hot}{(integer) Actual choice in the validation task}
#'   \item{group}{(integer) Grouping variable}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name acbc_interpolate
#' @usage data(acbc_interpolate)
#' @source \url{https://osf.io/m5x3a/}
#' @format A data frame with 110 rows and 39 variables
#' @examples
#'
#' # create total utility of validation tasks
#'
#' data(acbc_interpolate)
#'
#' prod1 <- c(3, 10, 9, 12, 15, 19, 23, 31, 248.55)
#' prod2 <- c(3, 9, 9, 13, 15, 17, 21, 26, 237.39)
#' prod3 <- c(4, 10, 8, 13, 14, 19, 24, 29, 273.15)
#' prod4 <- c(4, 9, 9, 10, 15, 18, 25, 31, 213.55)
#' prod5 <- c(2, 10, 7, 13, 15, 16, 25, 30, 266.10)
#' prod6 <- c(2, 9, 6, 11, 15, 19, 25, 28, 184.50)
#'
#' hot_acbc_interpolate <- create_hot(
#'   data = acbc_interpolate,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   coding = c(0, 1, rep(0, times = 6), 2),
#'   lin.p = 5,
#'   interpolate.levels = list(
#'     c(9, 10),
#'     c(121.95, 226.95, 272.95, 326.95, 507.95)
#'   ),
#'   piece.p = list(
#'     list(
#'       c(33, 34), c(33, 34), c(34, 35),
#'       c(32, 33), c(33, 34), c(32, 33)
#'     )
#'   ),
#'   method = "acbc",
#'   choice = "hot"
#' )
#'
#'
#' # measure the relative importance of the attributes
#'
#' att_imp(
#'   data = acbc_interpolate,
#'   attrib = list(
#'     paste0("att1_lev", c(1:3)),
#'     "att2",
#'     paste0("att3_lev", c(1:4)),
#'     paste0("att4_lev", c(1:4)),
#'     paste0("att5_lev", c(1:2)),
#'     paste0("att6_lev", c(1:4)),
#'     paste0("att7_lev", c(1:6)),
#'     paste0("att8_lev", c(1:6)),
#'     paste0("price_", c(1:5))
#'   ),
#'   interpolate.levels = list(c(9, 10)),
#'   coding = c(0, 1, rep(0, times = 6), 2),
#'   res = "agg"
#' )
#'
#'
#' # convert raw utilities into zero-centered diffs
#'
#' zc_diffs(
#'   data = acbc_interpolate,
#'   attrib = list(
#'     paste0("att1_lev", c(1:3)),
#'     "att2",
#'     paste0("att3_lev", c(1:4)),
#'     paste0("att4_lev", c(1:4)),
#'     paste0("att5_lev", c(1:2)),
#'     paste0("att6_lev", c(1:4)),
#'     paste0("att7_lev", c(1:6)),
#'     paste0("att8_lev", c(1:6)),
#'     paste0("price_", c(1:5))
#'   ),
#'   interpolate.levels = list(c(9, 10)),
#'   coding = c(0, 1, rep(0, times = 6), 2),
#'   res = "agg",
#'   none = "none"
#' )
#'
"acbc_interpolate"
