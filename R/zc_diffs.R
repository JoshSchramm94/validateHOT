#' Function to calculate zero-centered diffs for (A)CBCs
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s).
#' @param attrib A list that specifies the attribute levels for each attribute.
#' @param coding A vector of the coding of each attribute, '0' = part-worth
#' coding, '1' = linear coding, or '2' = piecewise coding.
#' @param interpolate.levels A list of the attribute levels that should
#' be interpolated. These have to be the same as specified in model estimation
#' (e.g., if you scale or center attribute levels before estimation, insert
#' these levels). Please make sure to provide all levels. Only has to be
#' specified for the variables that are coded as '1' (linear).
#' @param res A character vector indicating whether individual zero-centered
#' diffs  (`ind`) or aggregated (`agg`) zero-centered diffs should be returned.
#' @param none An optional vector to specify `none`
#' alternative in `data`.
#'
#' @details
#' `zc_diffs()` converts raw utilities of a CBC or an ACBC to
#' zero-centered diffs (Orme, 2020, p. 78). This allows for comparison between
#' the attributes.
#'
#' `data` has to be a data.frame object with the relevant attribute levels.
#' Attribute levels have to be the raw utilities.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `attrib` specifies the attribute levels for each alternative.
#' Input for `attrib` has to be a list. Needs to specify the column names or
#' column indexes of the attribute levels.
#'
#' `coding` indicates the attribute coding. `0`to indicated part-worth coding,
#' `1` for linear coding, or `2` for piecewise coding.
#'
#' `interpolate.levels` is required for linear-coded variables.
#' If scaled or centered values were used for Hierarchical Bayes
#' estimation, these have to be specified in this case.
#' All values have to be specified. For example, if one linear-coded attribute
#' has 5 levels, all 5 levels have to be inserted.
#'
#' `res` specifies whether results should be aggregated across all participants
#' or across `group` (`res` needs to be set to `agg`) or if scores
#' should be converted for individuals only (`ind`).
#'
#' `none` specifies whether none option was included or not, if yes,
#' column name or column index of `none` needs to be specified. If no
#' `none` option was included, leave it empty.
#'
#'
#'
#' @seealso {
#' [`att_imp()`][att_imp] for attribute importance scores for (A)CBC
#' [`prob_scores()`][prob_scores] for probability scores for MaxDiff
#' [`zero_anchored()`][zero_anchored] for zero-anchored interval
#' scores for MaxDiff
#' }
#'
#' @references {
#'
#' Orme, B. K. (2020). *Getting Started with Conjoint Analysis: Strategies
#' for Product Design and Pricing Research*. 4th edition. Manhattan Beach, CA:
#' Research Publishers LLC.
#'
#' }
#'
#' @return a tibble
#'
#' @examples
#'
#' # zero-centered scores for CBC (only part-worth) - without group
#' # argument defined
#' zc_diffs(
#'   data = cbc,
#'   attrib = list(
#'     paste0("att1_lev", c(1:3)),
#'     paste0("att2_lev", c(1:2)),
#'     paste0("att3_lev", c(1:4)),
#'     paste0("att4_lev", c(1:4)),
#'     paste0("att5_lev", c(1:2)),
#'     paste0("att6_lev", c(1:4)),
#'     paste0("att7_lev", c(1:6)),
#'     paste0("att8_lev", c(1:6)),
#'     paste0("price_", c(1:6))
#'   ),
#'   coding = c(rep(0, times = 9)),
#'   none = "none",
#'   res = "agg"
#' )
#'
#' \dontrun{
#'
#' # zero-centered scores for CBC (incl. linear coded attribute) - without
#' # group argument defined
#' zc_diffs(
#'   data = cbc_linear,
#'   attrib = list(
#'     paste0("att1_lev", c(1:3)),
#'     paste0("att2_lev", c(1:2)),
#'     paste0("att3_lev", c(1:4)),
#'     paste0("att4_lev", c(1:4)),
#'     paste0("att5_lev", c(1:2)),
#'     paste0("att6_lev", c(1:4)),
#'     paste0("att7_lev", c(1:6)),
#'     paste0("att8_lev", c(1:6)),
#'     "price"
#'   ),
#'   coding = c(rep(0, times = 8), 1),
#'   interpolate.levels = list(c(seq(from = 175.99, to = 350.99, by = 35))),
#'   none = "none",
#'   res = "agg"
#' )
#'}
#'
#' @export
zc_diffs <- function(data,
                     group = NULL,
                     attrib,
                     coding,
                     interpolate.levels = NULL,
                     res = c("agg", "ind"),
                     none = NULL) {
  # check for missing arguments ------------------------------------------------
  if (missing(attrib)) {
    stop('Error: argument "attrib" must be provided.')
  }

  if (missing(coding)) {
    stop('Error: argument "coding" must be provided.')
  }

  if (missing(res)) {
    stop('Error: argument "res" must be provided.')
  }

  # end ------------------------------------------------------------------------

  # check for attrib argument --------------------------------------------------

  # attrib and coding must have the same length
  same_length(coding, attrib, "coding", "attrib")

  # end ------------------------------------------------------------------------

  # check for coding argument --------------------------------------------------

  # coding must be a numeric vector
  numeric_vector(coding)

  # coding must be 0, 1, 2
  allowed_input(coding, c(0, 1, 2))

  # only one piecewise coded attribute
  number_piecewise(coding)

  # end ------------------------------------------------------------------------

  # check for interpolate.levels argument --------------------------------------
  if (any(coding == 1)) {
    # interpolate.levels must be provided if coding includes 1
    if (missing(interpolate.levels)) {
      stop('Error: argument "interpolate.levels" must be provided.')
    }

    # interpolate.levels must be a list
    list_input(interpolate.levels)

    # interpolate levels must be list with only numeric input
    numeric_list(interpolate.levels)

    # interpolate levels must be the same length as '1' specifies
    coding_equals_length_list(coding, interpolate.levels, 1)

    # coding input must equal length interpolate levels
    same_length(
      coding[coding %in% 1],
      interpolate.levels,
      "coding",
      "interpolate.levels"
    )
  }

  # end ------------------------------------------------------------------------

  # check coding and attrib ----------------------------------------------------
  for (i in seq_along(coding)) {
    switch(EXPR = (coding[i] + 1),
      coding_zero(attrib[i], attribute = i),
      coding_one(attrib[i], attribute = i)
    )
  }

  # numeric variables
  attrib_names <- unlist(attrib)
  var_names <- colnames(data[attrib_names])
  variable_numeric_alt(data, variable = var_names, argument = attrib)

  # end ------------------------------------------------------------------------

  # check res ------------------------------------------------------------------
  allowed_input(res, c("ind", "agg"))
  # end ------------------------------------------------------------------------

  # check for group argument ---------------------------------------------------

  # check for missings in group
  missing_group(data, group = {{ group }})
  # end ------------------------------------------------------------------------

  # run att_imp() function -----------------------------------------------------

  # manipulate varibales if price is part-worth coded
  if (any(coding == 2)) {
    var_names <- data %>%
      dplyr::select(unlist(attrib[which(coding == 2)])) %>%
      colnames()

    piece_coded <- var_names <- data %>%
      dplyr::select(unlist(attrib[which(coding == 2)])) %>%
      colnames()

    if (!missing(none)) {
      var_names <- c(var_names, colnames(data[none]))
    }

    data <- data %>%
      dplyr::mutate(
        dplyr::across(
          tidyselect::all_of(var_names),
          \(x) x - rowMeans(.[, piece_coded])
        )
      )
  }

  # prepare out
  new_col_names <- paste0("att_imp_", c(seq_along(coding)))

  for (i in seq_along(coding)) {

    att_levels <- attrib[[i]]

    # if not linearly coded, calculate range of attributes
    if (coding[i] != 1) {
      data[[new_col_names[i]]] <- apply(data[att_levels], 1, summed_range)
    }

    # if linear coded attribute, treat differently
    if (coding[i] == 1) {
      list_member <- sum(coding[1:i] == 1)
      range_levels <- vapply(
        X = interpolate.levels,
        FUN = summed_range,
        FUN.VALUE = numeric(1)
      )[list_member]

      data[[new_col_names[i]]] <- vapply(
        X = data[, att_levels],
        FUN = \(x) abs(x) * range_levels,
        FUN.VALUE = numeric(1)
      )
    }
  }

  # get attribute levels
  attribute_levels <- unlist(attrib)

  # add none alternative if not missing
  if (!missing(none)) {
    attribute_levels <- c(attribute_levels, colnames(data[none]))
  }

  # calculate the zero centered diffs
  zc_diffs_data <- data %>%
    dplyr::mutate(mult_factor = (length(coding) * 100) /
      rowSums(data[new_col_names])) %>%
    dplyr::mutate(dplyr::across(
      tidyselect::all_of(attribute_levels),
      \(x) x * mult_factor
    )) %>%
    dplyr::select(-mult_factor)

  # if res == "agg" aggregate results
  if (res == "agg") {
    zc_diffs_data <- zc_diffs_data %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(dplyr::across(tidyselect::all_of(attribute_levels),
        c(mw = mean, std = stats::sd),
        .names = "{.col}___{.fn}"
      )) %>%
      tidyr::pivot_longer(
        cols = tidyselect::ends_with(
          c("___mw", "___std")
        ),
        names_to = c("Option", ".value"),
        names_sep = "___"
      )
  }

  return(zc_diffs_data)
  # end ------------------------------------------------------------------------
}
