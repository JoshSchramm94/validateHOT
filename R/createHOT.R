#' @title  Function to create total utilities for validation task alternatives / market scenario
#'
#' @description Function used to create utilities for validation/holdout task or
#' for the alternatives in a market scenario.
#'
#' @param data A data frame with all relevant variables.
#' @param id A vector of unique identifier in `data`.
#' @param none An optional vector to specify `none`
#' alternative in `data`.
#' @param prod.levels A list to define the attribute levels of the
#' alternatives (`prod`). If linear-coded or piecewise-coded
#' attributes are included, column indexes are required for the input.
#' @param interpolate.levels A list of the levels of the attribute that should
#' be interpolated. These have to be the same as specified in model estimation
#' (e.g., if you scale or center attribute levels before estimation, insert the same levels).
#' Please make sure to provide the whole list. Only has to be specified for the
#' attributes that are coded as `1` (linear) or `2` (piecewise).
#' @param piece.p A list of the lower level and the upper
#' level that should be used for interpolating.
#' @param lin.p A vector to specify linear coded variables.
#' @param coding A vector to define attributes coding, `0` = part-worth
#' coding,`1` = linear coding, `2` = piecewise coding; please make sure to code
#' linear price of ACBC as piecewise since there are usually two values to interpolate.
#' @param method A character to specify the `method` of your study.
#' `method` has to be one of the following: `"MaxDiff"`, `"CBC"`, or `"ACBC"`.
#' @param varskeep A vector specifying variables that should be kept
#' in the data frame.
#' @param choice Actual choice in the holdout/validation task. Leave empty for
#' specifying market scenario (warning will be displayed, however).
#'
#' @details
#' To test the validation metrics of a holdout/validation task or to run a
#' market scenario, the scenario first has to be created, by summing up
#' the alternatives part-worth utilities.
#' This is done with the `createHOT()` function.
#' Make sure you upload the raw utilities of your study (either from Sawtooth Software
#' or ChoiceModelR, Sermas, 2022).
#' The function then creates the alternatives' utility based on the additive
#' utility model (Rao, 2014, p. 82). If you are working with alternative specific-designs,
#' insert `NA` if attribute is not specified.
#'
#' `data` has to be a data frame with raw scores of the attribute
#' levels.
#'
#' `id` has to be the column index or column name of the id (unique for each participant)
#' in data frame.
#'
#' `none` is required in case a `none` alternative is
#' included in holdout/validation task, specify
#' column index or column name of `none` alternative, otherwise leave it empty.
#'
#'
#' `prod.levels` specifies the attribute levels for each alternative.
#' Input for `prod.levels` has to be a list. In case
#' `method = "MaxDiff"`, list should only contain column indexes or
#' column names of the alternatives in the holdout/validation task or market scenario.
#' If values for one attribute are interpolated (assuming linear or
#' piecewise coding), the value to be interpolated has to be specified (numeric
#' input). In addition, `lin.p` and/or `piece.p`, `interpolate.levels`,
#' as well as `coding` have to be specified.
#'
#' `interpolate.levels` is required in case interpolating is used
#' (only if variables are coded as linear or piecewise).
#' If scaled or centered values were used for hierarchical Bayes
#' estimation, the exact same levels are required (all of them).
#' For example, if one linear coded attribute
#' had 5 levels, all 5 levels are required. In case for linear coded
#' price for `method = "ACBC"`, specify both lower bound and upper
#' bound and code as piecewise in `coding`.
#' For piecewise-coded price, specify each breakpoint.
#' Input for `interpolate.levels` has to be a list.
#'
#' `piece.p` is required in case a variable is coded as
#' piecewise (see coding). Positions of both lower and upper bound are required.
#' In case interpolated values (see
#' `prod.levels`) is equal to a lower or upper bound, this can be specified
#' either as lower or upper bound. Input for `piece.p` has to be a list.
#'
#' `lin.p` is required in case a variable is coded as linear
#' (see coding). Since for linear coding (except for price
#' in `method = "ACBC"`) only one coefficient is provided in the output,
#' just this column index or column name is required.
#'
#' `coding` is required if `method = "CBC"`
#' or `method = "ACBC"`. Use `0` for part-worth
#' coding, `1` for linear coding, and `2` for piecewise coding.
#' In case `method = "ACBC"` and linear price function is used, this
#' variable has to be coded as piecewise (`2`). In
#' case `method` is set to `"MaxDiff"`, leave
#' `coding` empty. Input for `coding` has to be a vector.
#'
#' `method` specifies the preference measurement method. Can be set to
#' `"MaxDiff"`, `"CBC"`, or `"ACBC"`.
#'
#' `varskeep` is required in case other variables should be kept
#' in the data frame (for example, a grouping variable). Input
#' for `varskeep` has to be a vector with the column index(es) or names of the
#' variable(s) that should be kept.
#'
#' `choice` specifies the column index or column name of the actual choice
#' in the holdout/validation task. If only a market scenario is specified,
#' leave `choice` empty, however, a warning will be displayed in this case.
#'
#' @return a data frame
#' @importFrom stats approx
#' @importFrom magrittr %>%
#' @importFrom dplyr select
#' @importFrom tidyselect all_of

#'
#' @references {
#'
#' Rao, V. R. (2014). \emph{Applied Conjoint Analysis}. Heidelberg: Springer
#' Berlin. \verb{https://doi.org/10.1007/978-3-540-87753-0}
#'
#' Sermas R (2022). \emph{ChoiceModelR: Choice Modeling in R}. R package version 1.3.0,
#' \verb{https://CRAN.R-project.org/package=ChoiceModelR}.
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' library(validateHOT)
#'
#' # MaxDiff example
#' HOT_MD <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   none = 19,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20
#' )
#'
#' # CBC example
#' HOT_CBC <- createHOT(
#'   data = CBC,
#'   id = 1,
#'   none = 21,
#'   prod.levels = list(c(4, 9, 19), c(8, 12, 17), c(5, 10, 17)),
#'   coding = c(0, 0, 0),
#'   method = "CBC",
#'   choice = 22
#' )
#'
#' # CBC example with linear coding
#' HOT_CBC_lin <- createHOT(
#'   data = CBC_lin,
#'   id = 1,
#'   none = 15,
#'   prod.levels = list(c(4, 9, 60), c(8, 12, 40), c(5, 10, 45)),
#'   coding = c(0, 0, 1),
#'   interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
#'   lin.p = 14,
#'   method = "CBC",
#'   varskeep = 17,
#'   choice = 16
#' )
#'
#' # ACBC example with linear price
#' prod1 <- c(5, 11, 15, 17, 21, 25, 32, 34, 15.99)
#' prod2 <- c(6, 9, 15, 17, 23, 27, 31, 34, 12.99)
#' prod3 <- c(8, 12, 16, 19, 23, 24, 28, 34, 12.99)
#' prod4 <- c(7, 12, 14, 18, 22, 24, 28, 33, 9.99)
#' prod5 <- c(4, 10, 13, 17, 23, 27, 28, 34, 7.99)
#' prod6 <- c(5, 9, 14, 17, 23, 27, 29, 33, 9.99)
#'
#' HOT_ACBC <- createHOT(
#'   data = ACBC,
#'   id = 1,
#'   none = 37,
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   coding = c(0, 0, 0, 0, 0, 0, 0, 0, 2),
#'   interpolate.levels = list(c(2.093, 27.287)),
#'   piece.p = list(
#'     c(35, 36), c(35, 36), c(35, 36),
#'     c(35, 36), c(35, 36), c(35, 36)
#'   ),
#'   method = "ACBC",
#'   choice = 38
#' )
#'
#' prod1 <- c(5, 5, 12, 14, 18, 22, 29, 31, 15.99)
#' prod2 <- c(6, 4, 12, 14, 20, 24, 28, 31, 12.99)
#' prod3 <- c(8, 6, 13, 16, 20, 21, 25, 31, 12.99)
#' prod4 <- c(7, 5, 11, 15, 19, 21, 25, 30, 9.99)
#' prod5 <- c(4, 9, 10, 14, 20, 24, 25, 31, 7.99)
#' prod6 <- c(5, 8, 11, 14, 20, 24, 26, 30, 9.99)
#'
#' HOT_ACBC_inter <- createHOT(
#'   data = ACBC_interpolate,
#'   id = 1,
#'   none = 39,
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   coding = c(0, 1, 0, 0, 0, 0, 0, 0, 2),
#'   lin.p = 9,
#'   piece.p = list(
#'     c(36, 37), c(35, 36), c(35, 36),
#'     c(33, 34), c(33, 34), c(33, 34)
#'   ),
#'   interpolate.levels = list(
#'     c(3, 5, 8, 10),
#'     c(1.99, 6.99, 9.99, 10.99, 12.99, 17.99, 25.99)
#'   ),
#'   method = "ACBC",
#'   choice = 40
#' )
#' }
#' @export
createHOT <- function(data, id, none = NULL,
                      prod.levels, coding = NULL,
                      interpolate.levels = NULL,
                      lin.p = NULL,
                      piece.p = NULL,
                      method = c("ACBC", "CBC", "MaxDiff"),
                      varskeep = NULL,
                      choice = NULL) {
  # define number of alternatives specified
  prod <- length(prod.levels)

  # test numeric input for coding
  if (!(is.null(coding))) {
    for (i in seq_along(coding)) {
      if (!(is.numeric(coding[i]))) {
        stop("Error: 'coding' only can have numeric input!")
      }
    }
  }

  if (!(is.null(coding))) {
    if (any(coding == 1) || any(coding == 2)) {
      for (q in seq_along(prod.levels)) {
        if (any(is.character(prod.levels[[q]]))) {
          stop(
            "Error: Input 'prod.levels' has to be numeric if linear or ",
            "piecewise coded variables are included. Please use column indexes ",
            "instead of column names if you specify columns!"
          )
        }
      }
    }
  }


  # test whether method is specified
  if (missing(method)) {
    stop("Error: 'method' is not defined!")
  }

  # test whether choice is specified
  if (missing(choice)) {
    warning("Warning: 'choice' is not defined!")
  }

  # test whether method is correctly specified
  if ((method != "ACBC") && (method != "CBC") && (method != "MaxDiff")) {
    stop(
      "Error: Please choose one of the supported methods: 'MaxDiff',",
      " 'ACBC', 'CBC'!"
    )
  }

  # coding required for CBC or ACBC
  if (method == "CBC" || method == "ACBC") {
    if (missing(coding)) {
      stop("Error: 'coding' is missing!")
    }
  }

  # test whether coding is empty if method == MaxDiff
  if (method == "MaxDiff" && !(is.null(coding))) {
    stop("Error: 'coding' is not required for ", method, "!")
  }

  # test whether interpolate levels is empty is specified if method == MaxDiff
  if (method == "MaxDiff" && !(is.null(interpolate.levels))) {
    stop("Error: 'interpolate.levels' is not required for ", method, "!")
  }

  # test whether piece.p is empty is specified if method == MaxDiff
  if (method == "MaxDiff" && !(is.null(piece.p))) {
    stop("Error: 'piece.p' is not required for ", method, "!")
  }

  # test whether lin.p is empty is specified if method == MaxDiff
  if (method == "MaxDiff" && !(is.null(lin.p))) {
    stop("Error: 'lin.p' is not required for ", method, "!")
  }

  # test whether coding only includes 0, 1, 2
  if ((method == "ACBC" || method == "CBC") &&
    any(coding != 0 & coding != 1 & coding != 2)) {
    stop(
      "Error: Please only use '0' (for part-worth), '1' (for linear)",
      ", or '2' (for piecewise)!"
    )
  }

  if (!is.null(coding)) {
    for (i in 1:prod) {
      if (length(prod.levels[[i]]) != length(coding)) {
        stop("Error: 'coding' and number of attributes must have the same length!")
      }
    }
  }

  # test whether CBC is specified and no coding equal to 2
  if (method == "CBC" && any(coding == 2)) {
    stop("Error: Piecewise coding not possible for ", method, "!")
  }

  # test whether CBC is used, one variable linear coded however
  # position not specified (or other way around)
  if (method == "CBC" && !(any(coding == 1)) && !(is.null(lin.p))) {
    stop("Error: 'lin.p' specified but no '1' in coding!")
  }

  # test whether ACBC is used, one variable linear coded however
  # position not specified  (or other way around)
  if (method == "ACBC" && !(any(coding == 1)) && !(is.null(lin.p))) {
    stop("Error: 'lin.p' specified but no '1' in coding!")
  }

  # test whether ACBC is used, one variable piecewise coded however
  # position not specified  (or other way around)
  if (method == "ACBC" && !(any(coding == 2)) &&
    !(is.null(piece.p))) {
    stop("Error: 'piece.p' specified but no '2' in coding!")
  }

  # test input of prod.levels
  if (!(is.list(prod.levels))) {
    stop("Error: Input of 'prod.levels' has to be a list!")
  }

  # test variables of prod.levels
  if (!(is.null(prod.levels))) {
    for (tt in seq_along(prod.levels)) {
      lng <- length(prod.levels[[tt]])
      if (lng == 1) {
        if (!is.na(prod.levels[[tt]])) {
          var <- prod.levels[[tt]]
          if (!(is.numeric(data[[var]]))) {
            stop(
              "Error: Variables included in 'prod.levels' ",
              "have to be numeric!"
            )
          }
        }
      }

      if (lng > 1) {
        for (lng_lev in 1:lng) {
          if (coding[lng_lev] != 1 && coding[lng_lev] != 2) {
            if (!is.na(prod.levels[[tt]][lng_lev])) {
              var <- prod.levels[[tt]][lng_lev]
              if (!(is.numeric(data[[var]]))) {
                stop(
                  "Error: Variables included in 'prod.levels' ",
                  "have to be numeric!"
                )
              }
            }
          }
        }
      }
    }
  }

  # test input of interpolate levels
  if (!(is.list(interpolate.levels)) &&
    !(is.null(interpolate.levels))) {
    stop("Error: Input of 'interpolate.levels' has to be a list!")
  }

  # test variables of interpolate.levels
  if (!(is.null(interpolate.levels))) {
    for (tt in seq_along(interpolate.levels)) {
      lng <- length(interpolate.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(is.numeric(interpolate.levels[[tt]][lng_lev]))) {
          stop(
            "Error: Input of 'interpolate.levels' has to be a list ",
            "with only numeric values!"
          )
        }
      }
    }
  }


  # test lin.p variables format
  if (!(is.null(lin.p))) {
    for (ll in seq_along(lin.p)) {
      if (!is.numeric(data[[lin.p[ll]]])) {
        stop("Error: Variables included in 'lin.p' have to be numeric!")
      }
    }
  }

  # test whether coding indicated linear coded variable, however,
  # not specified
  if (any(coding == 1) && is.null(lin.p)) {
    stop("Error: Please specify 'lin.p'!")
  }

  if (any(coding == 1 | coding == 2) && missing(interpolate.levels)) {
    stop("Error: 'interpolate.levels' is missing!")
  }

  # test whether coding indicated piecewise coded variable, however,
  # not specified
  if (any(coding == 2) && is.null(piece.p)) {
    stop("Error: Please specify 'piece.p'!")
  }

  # test input of piece.p
  if (!(is.null(piece.p)) && !(is.list(piece.p))) {
    stop("Error: Input of 'piece.p' has to be a list!")
  }

  # test variables specified in piece.p
  if (!(is.null(piece.p))) {
    for (tt in seq_along(piece.p)) {
      lng <- length(piece.p[[tt]])
      if (lng == 1) {
        var <- piece.p[[tt]]
        if (!(is.numeric(data[[var]]))) {
          stop(
            "Error: Variables included in 'piece.p' ",
            "have to be numeric!"
          )
        }
      }

      if (lng > 1) {
        for (lng_lev in 1:lng) {
          var <- piece.p[[tt]][lng_lev]
          if (!(is.numeric(data[[var]]))) {
            stop(
              "Error: Variables included in 'piece.p' ",
              "have to be numeric!"
            )
          }
        }
      }
    }
  }

  ######################################################
  # if MaxDiff is used, set coding for each alternative to 0
  if (method == "MaxDiff") {
    coding <- c(rep(0, length(prod)))
  }

  ######################################################

  # create empty data frame to store
  if (!(is.null(none))) {
    df <- data.frame(matrix(
      nrow = nrow(data),
      ncol = (prod + 1 + 1)
    ))
  } else if (is.null(none)) {
    df <- data.frame(matrix(
      nrow = nrow(data),
      ncol = (prod + 1)
    ))
  }

  # prepare output names
  names <- c("ID")

  for (q in 1:prod) {
    Prod <- paste0("Option_", q)
    names <- c(names, Prod)
  }

  if (!(is.null(none))) {
    names <- c(names, "None")
  }

  # assign column names to new created data frame 'df'
  colnames(df) <- names

  # delete names
  rm(names)

  # store the id in the new data frame
  df[, 1] <- data[, id]

  # overwrite all NAs by 0
  df[is.na(df)] <- 0


  for (row in seq_len(nrow(df))) { # repeat procedure for each row
    for (q in 1:prod) { # and for each prod
      helper <- 1 # define helper variables
      linear_pos <- 1 # define helper variables

      # loop for each level specified for an alternative (prod)
      for (pq in seq_along(prod.levels[[q]])) {
        if (!is.na(prod.levels[[q]][pq])) {
          if (coding[pq] == 0) { # only run for part-worth coded variable

            # in case of part-worth coding the utilitiy can be added
            df[row, (q + 1)] <- df[row, (q + 1)] +
              data[row, prod.levels[[q]][pq]]
          }

          if (coding[pq] == 1) { # loop for each linear coded attribute

            # extract the interpolate levels
            inter.levels <- interpolate.levels[[helper]]

            # error if xout is larger than maximum
            if (prod.levels[[q]][(pq)] > max(inter.levels)) {
              stop("Error: Extrapolation not possible!")
            }

            pos <- lin.p[linear_pos] # extract the column index of the variable

            # center the attribute levels
            lin.levels_eff <- c(scale(inter.levels,
              center = TRUE, scale = FALSE
            ))

            # get utility of lower bound of linear coded attribute
            lin.low <- lin.levels_eff[1] * data[row, pos]

            # get utility of upper bound of linear coded attribute
            lin.up <- lin.levels_eff[length(lin.levels_eff)] *
              data[row, pos]

            # finally extrapolate utility for specified value
            util <- as.numeric(stats::approx(
              x = c(inter.levels[1], inter.levels[length(inter.levels)]),
              y = c(lin.low, lin.up),
              xout = prod.levels[[q]][(pq)]
            )[2])

            # finally store the utility
            df[row, (q + 1)] <- df[row, (q + 1)] + util

            # and set both helper variables one up
            helper <- helper + 1
            linear_pos <- linear_pos + 1
          }

          if (coding[pq] == 2) { # loop for piecewise coded variable

            # extract the interpolate levels
            inter.levels <- interpolate.levels[[helper]]

            pos.l <- piece.p[[q]][1] # store position of lower bound
            pos.u <- piece.p[[q]][2] # store position of upper bound

            # extract price that should be interpolated
            interprice <- prod.levels[[q]][(pq)]

            # error if xout is larger than maximum
            if (interprice > max(inter.levels)) {
              stop("Error: Extrapolation not possible!")
            }

            # extract the breakpoint below the price to be interpolated
            lower_b <- max(inter.levels[inter.levels < interprice])

            # extract the breakpoint equal or above the price to be interpolated
            upper_b <- min(inter.levels[inter.levels >= interprice])

            # interpolate
            util <- as.numeric(stats::approx(
              x = c(lower_b, upper_b),
              y = c(data[row, pos.l], data[row, pos.u]),
              xout = interprice
            )[2])

            df[row, (q + 1)] <- df[row, (q + 1)] + util # add to utility

            helper <- helper + 1 # add 1 to helper variables
          }
        }
      }
    }
  }

  # finally add none utility
  if (!(is.null(none))) {
    df[, ncol(df)] <- data[, none]
  }

  # in case varskeep specified
  if (!(is.null(varskeep))) {
    vars <- c(
      (data %>% dplyr::select(tidyselect::all_of(id)) %>% colnames(.)),
      (data %>% dplyr::select(tidyselect::all_of(varskeep)) %>% colnames(.))
    )
    add <- data[, vars] # store variables
    colnames(add)[1] <- "ID" # rename ID for merging purposes
    df <- merge(x = df, y = add, by = "ID") # merge
  }

  # store the final choice
  if (!is.null(choice)) {
    vars <- c(
      (data %>% dplyr::select(tidyselect::all_of(id)) %>% colnames(.)),
      (data %>% dplyr::select(tidyselect::all_of(choice)) %>% colnames(.))
    )
    final_choice <- data[, vars]
    # rename variables for merging purposes
    colnames(final_choice) <- c("ID", "choice")
    df <- merge(x = df, y = final_choice, by = "ID") # merge
  }

  if (is.character(data[[id]])) {
    df[["ID"]] <- as.character(df[["ID"]])
  } else if (is.numeric(data[[id]])) {
    df[["ID"]] <- as.numeric(df[["ID"]])
  }

  # store the finished data frame in new object
  HOT <- df

  # return the newly created data frame
  return(HOT)
}
