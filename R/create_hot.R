#' @title  Function to create total utilities for validation task alternatives
#' or market scenario
#'
#' @param data A data.frame object.
#' @param id Column name of the unique identifier.
#' @param none An optional vector to specify column name of a `none`
#' alternative.
#' @param prod.levels A list to define the attribute levels of the
#' alternatives (`prod`). If linear-coded or piecewise-coded
#' attributes are included, column indexes are required for the input.
#' @param interpolate.levels A list of the levels of the attribute that should
#' be interpolated. These have to be the same as specified in model estimation
#' (e.g., if you scale or center attribute levels before estimation, insert the
#' scaled or centered levels). Please make sure to provide the whole list.
#' Only has to be specified for the attributes that are coded as `1` (linear)
#' or `2` (piecewise).
#' @param piece.p A nested list, with a list for each of the piecewise-coded
#' variables. List for a piecewise-coded attribute must be the columns that
#' shoud be the lower and the upper level that should be used for interpolating.
#' @param lin.p A vector to specify column index or column names of
#' linear coded variables.
#' @param coding A vector to define attributes coding, `0` = part-worth
#' coding,`1` = linear coding, `2` = piecewise coding; please make sure to code
#' linear price of ACBC as piecewise. For more details, see the example
#' provided below. If you want to treat a part-worth coded variable
#' continuously, use the code `2` for this variable and provide the values in
#' `interpolate.levels` accordingly.
#' @param method A character to specify the `method` of your study.
#' `method` has to be one of the following: `"maxdiff"`, `"cbc"`, or `"acbc"`.
#' @param varskeep A vector specifying column names of the variables that should
#' be kept in the data frame.
#' @param choice Actual choice in the validation task. Leave empty for
#' specifying market scenario (warning will be displayed, however).
#'
#' @details
#' To test the validation metrics of a validation task or to run a
#' market scenario, the scenario first has to be created by summing up
#' the alternatives raw utilities. This is done with the `create_hot()`
#' function.  Make sure you upload the raw utilities of your study (either
#' from Sawtooth Software or ChoiceModelR, Sermas, 2022).
#' The function then creates the alternatives' utility based on the additive
#' utility model (Rao, 2014, p. 82). If you are working with alternative
#' specific-designs, insert `NA` if attribute is not specified.
#'
#' `data` must to be a data.frame object with raw scores of the attribute
#' levels.
#'
#' `id` has to be the column index or column name of the id (unique for each
#' participant) in data frame.
#'
#' `none` to specify variable name of the `none` alternative if it is included
#' in the validation task. Leave it empty, if there was no `none` alternative
#' included.
#'
#' `prod.levels` specifies the attribute levels for each alternative.
#' Input for `prod.levels` has to be a list. If values for one attribute are
#' supposed to be interpolated (assuming linear or piecewise coding), the
#' value to be interpolated has to be specified (numeric input). In addition,
#' `lin.p` and/or `piece.p`, `interpolate.levels`, and `coding` have to be
#' specified.
#'
#' `interpolate.levels` is required in case interpolating is used (only if
#' variables are coded as linear or piecewise or if you want to treat a part-
#' worth coded variable as continuously). If scaled or centered values
#' were used for hierarchical Bayes estimation, the exact same levels are
#' required (all of them). For example, if one linear coded attribute
#' had 5 levels, all 5 levels are required. In case for linear coded price for
#' `method = "acbc"`, specify both lower bound and upper bound and code as
#' piecewise in `coding`. For piecewise-coded price, specify each breakpoint.
#' Input for `interpolate.levels` has to be a list.
#'
#' `piece.p` is required in case a variable is coded as piecewise (see coding).
#' Positions of both lower and upper bound are required. In case interpolated
#' values (see `prod.levels`) is equal to a lower or upper bound, this can be
#' specified either as lower or upper bound. Input for  `piece.p` has to be a
#' nested list and must contain a list for each piecewise-coded attribute.
#'
#' `lin.p` is required in case a variable is coded as linear
#' (see coding). Since for linear coding (except for price
#' in `method = "acbc"`) only one coefficient is provided in the output,
#' provide this column name accordingly.
#'
#' `coding` is required if `method = "cbc"` or `method = "acbc"`. Use `0` for
#' part-worth coding, `1` for linear coding, and `2` for piecewise coding.
#' In case `method = "acbc"` and linear price function is used, this variable
#' has to be coded as piecewise (`2`). In case `method` is set to `"maxdiff"`
#' leave `coding` empty. If a part-worth coded variable should be treated
#' continuously, set it to `2`. Input for `coding` has to be a vector.
#'
#' `method` specifies the preference measurement method. Can be set to
#' `"maxdiff"`, `"cbc"`, or `"acbc"`.
#'
#' `varskeep` is required in case other variables should be kept
#' in the data frame (for example, a grouping variable). Provide the column
#' names of the variable(s) that should be kept.
#'
#' `choice` specifies the column name of the actual choice
#' in the validation task. If only a market scenario is specified, leave
#' `choice` empty, however, a warning will be displayed in this case.
#'
#' Instead of the column names, one can also provide column indexes.
#'
#'
#' @references {
#'
#' Rao, V. R. (2014). *Applied Conjoint Analysis*. Heidelberg: Springer
#' Berlin. \verb{https://doi.org/10.1007/978-3-540-87753-0}
#'
#' Sermas R (2022). *ChoiceModelR: Choice Modeling in R*. R package
#' version 1.3.0, \verb{https://CRAN.R-project.org/package=ChoiceModelR}.
#'
#' }
#'
#' @examples
#' \dontrun{
#'
#' # MaxDiff example
#' hot_mxd <- create_hot(
#'   data = maxdiff,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(2, 9, 10, 14, 15, 16, 17),
#'   method = "maxdiff",
#'   varskeep = "group",
#'   choice = "hot"
#' )
#'
#' # CBC example
#' hot_cbc <- create_hot(
#'   data = cbc,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(
#'     c(3, 6, 10, 13, 16, 20, 24, 32, 35),
#'     c(3, 5, 10, 14, 16, 18, 22, 27, 35),
#'     c(4, 6, 9, 14, 15, 20, 25, 30, 36),
#'     c(4, 5, 10, 11, 16, 19, 26, 32, 34),
#'     c(2, 6, 8, 14, 16, 17, 26, 31, 36),
#'     c(2, 5, 7, 12, 16, 20, 26, 29, 33)
#'   ),
#'   coding = c(rep(0, times = 9)),
#'   method = "cbc",
#'   choice = "hot"
#' )
#'
#' # CBC example with linear coding
#' hot_cbc_linear <- create_hot(
#'   data = cbc_linear,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(
#'     c(3, 6, 10, 13, 16, 20, 24, 32, 248.55),
#'     c(3, 5, 10, 14, 16, 18, 22, 27, 237.39),
#'     c(4, 6, 9, 14, 15, 20, 25, 30, 273.15),
#'     c(4, 5, 10, 11, 16, 19, 26, 32, 213.55),
#'     c(2, 6, 8, 14, 16, 17, 26, 31, 266.10),
#'     c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
#'   ),
#'   coding = c(rep(0, times = 8), 1),
#'   lin.p = 33,
#'   interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
#'   method = "cbc",
#'   choice = "hot"
#' )
#'
#' # ACBC example with linear price
#' prod1 <- c(3, 6, 10, 13, 16, 20, 24, 32, 248.55)
#' prod2 <- c(3, 5, 10, 14, 16, 18, 22, 27, 237.39)
#' prod3 <- c(4, 6, 9, 14, 15, 20, 25, 30, 273.15)
#' prod4 <- c(4, 5, 10, 11, 16, 19, 26, 32, 213.55)
#' prod5 <- c(2, 6, 8, 14, 16, 17, 26, 31, 266.10)
#' prod6 <- c(2, 5, 7, 12, 16, 20, 26, 29, 184.50)
#'
#' hot_acbc <- create_hot(
#'   data = acbc,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(prod1, prod2, prod3, prod4, prod5, prod6),
#'   coding = c(rep(0, times = 8), 2),
#'   interpolate.levels = list(c(121.95, 507.95)),
#'   piece.p = list(
#'     list(
#'     c(33, 34), c(33, 34), c(33, 34),
#'     c(33, 34), c(33, 34), c(33, 34)
#'     )
#'   ),
#'   method = "acbc",
#'   choice = "hot"
#' )
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
#'     c(33, 34), c(33, 34), c(34, 35),
#'     c(32, 33), c(33, 34), c(32, 33)
#'     )
#'   ),
#'   method = "acbc",
#'   choice = "hot"
#' )
#' }
#' @export
create_hot <- function(data,
                       id,
                       none = NULL,
                       prod.levels,
                       coding = NULL,
                       interpolate.levels = NULL,
                       lin.p = NULL,
                       piece.p = NULL,
                       method = c("acbc", "cbc", "maxdiff"),
                       varskeep = NULL,
                       choice = NULL) {

  # check for missing arguments ------------------------------------------------
  if (missing(id)) {
    stop('Error: argument "id" must be provided.')
  }

  if (missing(prod.levels)) {
    stop('Error: argument "prod.levels" must be provided.')
  }

  if (missing(method)) {
    stop('Error: argument "method" must be provided.')
  }

  if (missing(choice)) {
    warning('Warning: argument "choice" is missing.')
  }

  # end ------------------------------------------------------------------------

  # check prod.levels argument -------------------------------------------------

  # if coding contains 1 or 2, column indexes needs to be provided
  if (!missing(coding) && any(coding %in% c(1, 2))) {

    attribute_levels <- unlist(prod.levels)[coding %in% 0]
    attribute_levels <- attribute_levels[!is.na(attribute_levels)]
    numeric_vector(attribute_levels)

    # check for numeric input in prod.levels
    var_names <- colnames(data[unique(attribute_levels)])
    variable_numeric_alt(data, variable = var_names, argument = prod.levels)

    # check for same length of coding as for prod.levels
    coding_equals_length_list_members(coding, prod.levels, code = c(0, 1, 2))
  }

  # warning if alternative specific coding applied
  missing_prod.levels(prod.levels)

  # prod.levels needs to be a list
  list_input(prod.levels)

  # end ------------------------------------------------------------------------

  # check for method argument --------------------------------------------------

  # test whether method is correctly specified
  match.arg(method, c("acbc", "maxdiff", "cbc"))

  # end ------------------------------------------------------------------------

  # check for arguments falsely defined if method set to maxdiff ---------------
  if (method == "maxdiff") {
    if (!missing(coding)) {
      stop(
        'Error: "coding" must not be provided if ',
        '"method" is set to "maxdiff".'
      )
    }

    if (!missing(interpolate.levels)) {
      stop(
        'Error: "interpolate.levels" must not be provided if ',
        '"method" is set to "maxdiff".'
      )
    }

    if (!missing(piece.p)) {
      stop(
        'Error: "piece.p" must not be provided if ',
        '"method" is set to "maxdiff".'
      )
    }

    if (!missing(lin.p)) {
      stop(
        'Error: "lin.p" must not be provided if ',
        '"method" is set to "maxdiff".'
      )
    }

    # specify coding
    coding <- c(rep(0, length(prod.levels)))
  }

  # end ------------------------------------------------------------------------

  # check for coding argument --------------------------------------------------

  # test whether coding is provided
  if (missing(coding)) {
    stop('Error: argument "coding" must be provided.')
  }

  # coding must be 0, 1, 2
  allowed_input(coding, c(0, 1, 2))

  # coding and prod.levels need to have the same length
  if (!missing(coding) && method != "maxdiff") {
    length_vector_list(coding, prod.levels, "coding", "prod.levels")
  }

  # end ------------------------------------------------------------------------

  # check for arguments falsely defined if method set to (A)CBC ----------------

  # check whether lin.p matches coding argument
  if ((any(coding == 1) && missing(lin.p)) || (all(coding != 1) &&
    !missing(lin.p))) {
    stop('Error: "coding" and "lin.p" do not match.')
  }

  # check whether piece.p matches coding argument
  if ((any(coding == 2) && missing(piece.p)) || (all(coding != 2) &&
    !missing(piece.p))) {
    stop('Error: "coding" and "piece.p" do not match.')
  }

  # check whether interpolate.levels is defined
  if (any(coding %in% c(1, 2)) && missing(interpolate.levels)) {
    stop('Error: "interpolate.levels" must be provided.')
  }

  # end ------------------------------------------------------------------------

  # check for list arguments ---------------------------------------------------

  # piece.p
  if (!missing(piece.p)) {
    # needs to be a list
    nested_list_input(piece.p)

    # check for max. length of 2
    piecewise_coded(piece.p)
  }

  # interpolate.levels
  if (!missing(interpolate.levels)) {

    # needs to be a list
    list_input(interpolate.levels)

    # can only have numeric input
    numeric_vector(unlist(interpolate.levels))

    # length of interpolate.levels should equal coding
    coding_equals_length_list(coding, interpolate.levels, code = c(1, 2))
  }

  # end ------------------------------------------------------------------------

  # run create_hot() function --------------------------------------------------

  # define number of alternatives in validation task
  alternatives <- length(prod.levels)

  # create new data frame
  hot_df <- data.frame(
    id = data[, id]
  )

  # preapre vector for final names
  fin_names <- c("id", paste0("option_", seq_len(alternatives)))

  # create new variables
  hot_df[paste0("option_", seq_len(alternatives))] <- 0

  # start for loop for each of the alternatives
  for (q in seq_len(alternatives)) {

    # start for loop for each of the attribute levels
    for (p in seq_along(coding)) {

      # if coding == 0, use raw utility an additive utility model
      if (coding[p] == 0 && !is.na(prod.levels[[q]][p])) {

        # store variable name
        variable <- colnames(data[unlist(prod.levels[[q]][p])])

        # get utility
        hot_df[paste0("option_", q)] <- hot_df[paste0("option_", q)] +
          data[, variable]
      }

      # if attribute level is linear-coded
      if (coding[p] == 1 && !is.na(prod.levels[[q]][p])) {

        # create helping vector to get right position of interpolate.levels
        helper_lin <- sum(coding[1:p] > 0)

        # store the linear levels
        linear_levels <- unlist(interpolate.levels[helper_lin])

        # check for extrapolation
        extrapolation_check(
          prod.levels[[q]][p],
          linear_levels[1],
          linear_levels[length(linear_levels)],
          allowed = TRUE
        )

        # center linear coded attribute
        effects_coded_lin <- linear_levels - mean(linear_levels)

        # get utility of lower bound of linear coded attribute
        lin_low <- effects_coded_lin[1] * data[, lin.p[helper_lin]]

        # get utility of upper bound of linear coded attribute
        lin_up <- effects_coded_lin[length(effects_coded_lin)] *
          data[, lin.p[helper_lin]]

        # adjust input value
        lin_value <- prod.levels[[q]][p] - mean(linear_levels)

        # finally interpolate utility for specified value
        add_util <- ((lin_low * (effects_coded_lin[length(effects_coded_lin)] -
          lin_value)) +
          (lin_up * (lin_value - effects_coded_lin[1]))) /
          diff(range(
            effects_coded_lin[1],
            effects_coded_lin[length(effects_coded_lin)]
          ))

        # add the utility
        hot_df[paste0("option_", q)] <- hot_df[paste0("option_", q)] + add_util
      }

      # if piecewise coded
      if (coding[p] == 2 && !is.na(prod.levels[[q]][p])) {

        # create helping vector to get right position of interpolate.levels
        helper_lin <- sum(coding[1:p] > 0)

        linear_levels <- unlist(interpolate.levels[helper_lin])

        # create helping vector for piece.p
        helper_piece <- sum(coding[1:p] == 2)

        # column of lower bound
        pos_lower <- piece.p[[helper_piece]][[q]][1]

        # column of upper bound
        pos_upper <- piece.p[[helper_piece]][[q]][2]

        # check for extrapolation
        extrapolation_check(
          prod.levels[[q]][p],
          linear_levels[1],
          linear_levels[length(linear_levels)],
          allowed = FALSE
        )

        if (min(linear_levels) == prod.levels[[q]][p]) {
          lower_break <- max(linear_levels[linear_levels <= prod.levels[[q]][p]])

          upper_break <- min(linear_levels[linear_levels > prod.levels[[q]][p]])

        }

        if (min(linear_levels) != prod.levels[[q]][p]) {
          # extract the breakpoint below the price to be interpolated
          lower_break <- max(linear_levels[linear_levels < prod.levels[[q]][p]])

          # extract the breakpoint equal or above the price to be interpolated
          upper_break <- min(linear_levels[linear_levels >= prod.levels[[q]][p]])
        }

        # store utilities of lower break point
        utilities_lower <- data[, pos_lower]

        # store utilities of upper break point
        utilities_upper <- data[, pos_upper]

        # finally interpolate utility for specified value
        add_util <- ((utilities_lower * (upper_break - prod.levels[[q]][p])) +
          (utilities_upper * (prod.levels[[q]][p] - lower_break))) /
          diff(range(lower_break, upper_break))

        # add the utility
        hot_df[paste0("option_", q)] <- hot_df[paste0("option_", q)] + add_util
      }
    }
  }

  # store none alternative
  if (!missing(none)) {
    hot_df["none"] <- data[, none]
    fin_names <- c(fin_names, "none")
  }

  # store choice alternative
  if (!missing(choice)) {
    hot_df["choice"] <- data[, choice]
    fin_names <- c(fin_names, "choice")
  }

  # store varskeep if specified
  if (!missing(varskeep)) {
    keep_names <- colnames(data[id])
    keep_names <- c(keep_names, colnames(data[varskeep]))

    keep_df <- data[, keep_names]
    hot_df <- merge(
      x = hot_df,
      y = keep_df,
      by.x = "id",
      by.y = colnames(data[id])
    )

    fin_names <- c(fin_names, keep_names[-1])
  }

  # rename final data frame
  colnames(hot_df) <- fin_names

  # store as data.frame
  hot_df <- as.data.frame(hot_df)

  return(hot_df)

  # end ------------------------------------------------------------------------
}
