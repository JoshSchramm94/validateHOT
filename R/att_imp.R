#' Function to calculate attributes' importance score of (A)CBCs
#'
#' @param data A data frame containing all relevant variables.
#' @param group Optional variable to to get `att_imp()` by group(s).
#' @param attrib A list that specifies the attribute levels for each attribute.
#' @param coding A vector of the coding of each attribute, `0` = part-worth
#' coding, `1` = linear coding, or `2` = piecewise coding.
#' @param interpolate.levels A list of the levels of the attribute that should
#' be interpolated. These levels must match those specified in the model
#' estimation (e.g., if you scale or center attribute levels before estimation,
#' insert the scaled or centered levels). Ensure to provide the entire list.
#' It has to be specified only for the variables that are coded as '1' (linear).
#' @param res A vector indicating whether individual results (`ind`) or
#' aggregated (`agg`) results should be returned.
#'
#' @details
#' `att_imp()` converts raw utilities of a CBC or an ACBC to relative
#' importance scores (see, Orme, 2020, p. 80, for more information).
#'
#' `data` A data frame with all attributes and the corresponding
#' levels. Attribute levels need to be the raw utilities of hierarchical Bayes
#'  estimation.
#'
#' `group` optional grouping variable(s) to display results by group(s).
#' Has to be the column name(s) of variables in `data`.
#'
#' `attrib` specifies the attribute levels for each alternative.
#' Input for `attrib` has to be a list.
#'
#' `coding` is required to indicate the attributes' coding. `0`
#' to indicate part-worth coding, `1` for linear coding, or `2` for
#' piecewise coding.
#'
#' `interpolate.levels` is required for linear coded variables.
#' If scaled or centered values were used for hierarchical Bayes
#' estimation, these need to be defined in `att_imp()`.
#' All values have to be specified. For example, if one linear coded attribute
#' has 5 levels, all 5 levels have to be provided to the list.
#'
#' `res` specifies whether results should be aggregated across all
#' participants or across `group` (`res` needs to be set to
#' `agg`) or if scores should be converted for individuals only (`ind`).
#'
#'
#' @seealso {
#' [`prob_scores()`][prob_scores] for probability scores for MaxDiff
#' [`zero_anchored()`][zero_anchored] for zero-anchored interval
#' scores for MaxDiff
#' [`zc_diffs()`][zc_diffs] for zero-centered diff scores for (A)CBC
#' }
#'
#' @return a tibble
#'
#' @references {
#'
#' Orme, B. K. (2020). *Getting Started with Conjoint Analysis: Strategies
#' for Product Design and Pricing Research*. 4th edition. Manhattan Beach,
#' CA: Research Publishers LLC.
#'
#' }
#'
#' @examples
#'
#' att_imp(
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
#'   res = "agg"
#' )
#'
#' att_imp(
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
#'   res = "agg"
#' )
#'
#' @export
att_imp <- function(data,
                    group = NULL,
                    attrib,
                    coding,
                    interpolate.levels = NULL,
                    res = c("agg", "ind")) {
  # check for missing arguments ------------------------------------------------
  if (missing(attrib)) {
    stop('Error: argument "attrib" must be provided!')
  }

  if (missing(res)) {
    stop('Error: argument "res" must be provided!')
  }

  if (missing(coding)) {
    stop('Error: argument "coding" must be provided!')
  }

  # end ------------------------------------------------------------------------

  # check for attrib argument --------------------------------------------------

  # attrib and coding must have the same length
  same_length(coding, attrib, "coding", "attrib")

  # end ------------------------------------------------------------------------

  # check for coding argument --------------------------------------------------

  # coding input should be numeric
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

  if (all(coding != 1)) {
    if (!missing(interpolate.levels)) {
      stop('Error: argument "interpolate.levels" must not be defined.')
    }
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

  # create column names for new variables
  new_col_names <- paste0("att_imp_", c(seq_along(coding)))

  for (i in seq_along(coding)) {
    # store the attribute levels
    att_levels <- attrib[[i]]

    # if not linear coded, calculate the difference between max and min
    if (coding[i] != 1) {
      data[[new_col_names[i]]] <- apply(data[att_levels], 1, summed_range)
    }

    # if linear coded variable handle it differently
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

  # calculate relative percentage
  att_imp_res <- data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(new_col_names),
        \(x) x / rowSums(.[, new_col_names]) * 100
      )
    )


  # create aggregated results, if results set to "agg"
  if (res == "agg") {
    att_imp_res <- att_imp_res %>%
      dplyr::group_by(pick({{ group }})) %>%
      dplyr::reframe(dplyr::across(tidyselect::all_of(new_col_names),
        c(mw = mean, std = sd),
        .names = "{.col}___{.fn}"
      )) %>%
      tidyr::pivot_longer(
        cols = tidyselect::ends_with(c("___mw", "___std")),
        names_to = c("alternative", ".value"),
        names_sep = "___"
      )
  }

  return(att_imp_res)
  # end ------------------------------------------------------------------------
}
