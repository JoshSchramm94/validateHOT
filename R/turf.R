#' Function to run Total Unduplicated Reach and Frequency analysis
#'
#' @description
#' T(otal) U(nduplicated) R(each) and F(requency) is a "product line extension
#' model" (Miaoulis et al., 1990, p. 29). For each possible combination,
#' `turf()` looks for the reach and frequency of this combination. Participants
#' are reached, if at least one of the alternatives in a combination has a
#' higher utility than `none`. On the contrary, frequency calculates the
#' averaged number of alternatives that have a
#' higher utility than `none`.
#'
#' @param data A data frame with all relevant variables.
#' @param opts Column names of the alternatives included in the assortment.
#' @param none Column name of none / threshold alternative.
#' @param size An integer to determine size of the assortment.
#' @param fixed An optional vector to determine alternatives / items that have
#' to be included in the assortment.
#' @param prohib An optional list with vectors to determine prohibitions, i.e.,
#' alternatives / items that are not allowed to be together in one assortment.
#' @param approach A character whether to run first choice approach ('fc') or
#' threshold approach ('thres').
#'
#' @details
#'
#' `data` a data.frame object including the alternatives that should be tested.
#'
#' `opts` to specify the different alternatives in the product assortment that
#' should be considered. Input of `opts` has to be column name of variables
#' in `data`.
#'
#' `none` to specify column name of the `none` alternative in `data`
#' that needs to be exceeded.
#'
#' `size` an integer to determine the size of the assortment.
#'
#' `fixed` has to be a vector of variables that are fixed in the
#' assortment, i.e., they have to be part of the assortment.
#'
#' `prohib` has to be a list of vectors of variables that are prohibited
#' in the assortment, i.e., alternatives that are not allowed to be together in
#' one assortment.
#'
#' `approach` character defining whether first choice `approach = 'fc'` or
#' threshold `approach = 'thres'` should be applied for running `turf()`. If
#' `approach = 'fc'`, participants are considered being reached, if their
#' alternative with the highest utility is included in the assortment and this
#' alternative's utility is larger than the threshold's utility (Chrzan & Orme,
#' 2019, p. 111).
#' On the contrary, if `approach = 'thres'`, participants are considered
#' being reached, if utility of one product is higher than the one of the
#' `none` alternative (Chrzan & Orme, 2019, p. 112).
#' If `approach = 'fc'`, `reach` equals `freq` since participants have at
#' maximum their most preferred alternative that exceeds the `none`
#' alternative.
#'
#' @references {
#'
#' Chrzan, K., & Orme, B. K. (2019). *Applied MaxDiff: A Practitioner’s
#' Guide to Best-Worst Scaling* Provo, UT: Sawtooth Software.
#'
#' Miaoulis, G., Parsons, H., & Free, V. (1990). Turf: A New Planning Approach
#' for Product Line Extensions. *Marketing Research 2* (1): 28-40.
#'
#' }
#'
#' @return data frame
#'
#' @examples
#'
#' hot <- create_hot(
#'   data = maxdiff,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(
#'     2, 3, 4, 5, 6, 7, 8, 9, 10,
#'     11, 12, 13, 14, 15, 16, 17
#'   ),
#'   method = "maxdiff",
#'   choice = "hot"
#' )
#'
#' # turf no fixed alternatives + no prohibitions
#' t1 <- turf(
#'   data = hot,
#'   opts = c(option_1:option_16),
#'   none = none,
#'   size = 3L,
#'   approach = "thres"
#' )
#'
#' head(t1)
#'
#' # turf alternative 4 and 5 fixed, no prohibitions
#' t2 <- turf(
#'   data = hot,
#'   opts = c(option_1:option_16),
#'   none = none,
#'   size = 4L,
#'   fixed = c("option_4", "option_5"),
#'   approach = "thres"
#' )
#'
#' head(t2)
#'
#' #' # turf alternative 4 and 5 fixed, 2 and 9 not allowed together
#' t3 <- turf(
#'   data = hot,
#'   opts = c(option_1:option_16),
#'   none = none,
#'   size = 4L,
#'   fixed = c("option_4", "option_5"),
#'   prohib = list(c("option_2", "option_9")),
#'   approach = "thres"
#' )
#'
#' head(t3)
#'
#' @export
turf <- function(data,
                 opts,
                 none,
                 size,
                 fixed = NULL,
                 prohib = NULL,
                 approach = c("thres", "fc")) {

  # check for missing arguments ------------------------------------------------
  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }

  if (missing(none)) {
    stop('Error: argument "none" must be provided.')
  }

  if (missing(size)) {
    stop('Error: argument "size" must be provided.')
  }

  if (missing(approach)) {
    stop('Error: argument "approach" must be provided.')
  }

  # end ------------------------------------------------------------------------

  # check for `opts` argument --------------------------------------------------

  # check for numeric input
  variable_numeric(data, variable = {{ opts }}, argument = opts)

  # check for length of `opts`
  n_opts_cols(data, opts = {{ opts }})

  # check for missings in `opts`
  nvar_missings(data, variables = {{ opts }})

  # end ------------------------------------------------------------------------

  # check for `none` argument --------------------------------------------------

  # check for missings in `opts`
  nvar_missings(data, variables = {{ none }})

  # check for length of input
  ncol_input(data, variable = {{ none }}, argument = none)

  # check if `none` is parts of `opts`
  none_in_opts(data, none = {{ none }}, opts = {{ opts }}, should = FALSE)

  # check for numeric input
  variable_numeric(data, variable = {{ none }}, argument = choice)

  # end ------------------------------------------------------------------------

  # check size argument --------------------------------------------------------

  # check for numeric size
  numeric_vector(size)

  # size can not be larger than number in opts
  max_size(data, size = size, opts = {{ opts }})

  # store size as integer
  size <- as.integer(size)

  # end ------------------------------------------------------------------------

  # check for fixed argument ---------------------------------------------------

  if (!missing(fixed)) {
    vars_in_opts(data, vars = fixed, opts = {{ opts }}, arg_name = "fixed")
  }

  # end ------------------------------------------------------------------------

  # check for prohib argument --------------------------------------------------

  if (!missing(prohib)) {
    # prohib has to be part of opts
    variables_prohib <- unlist(prohib)
    vars_in_opts(data,
      vars = variables_prohib,
      opts = {{ opts }},
      arg_name = "prohib"
    )

    # prohib needs to be a list
    list_input(prohib)
  }

  # prohib can not be part of fixed
  if (!missing(prohib) && !missing(fixed)) {
    test_result <- lapply(prohib, \(x) all(x %in% fixed)) %>%
      unlist()

    if (any(test_result)) {
      stop('Error: "prohib" and "fixed" must be different.')
    }
  }

  # end ------------------------------------------------------------------------

  # check approach argument ----------------------------------------------------

  # check valid input in approach
  allowed_input(approach, c("thres", "fc"))

  # end ------------------------------------------------------------------------

  # run turf() -----------------------------------------------------------------

  # threshold approach
  if (approach == "thres") {
    working_set <- data %>%
      dplyr::mutate(
        dplyr::across({{ opts }}, \(x) ifelse(x > {{ none }}, 1, 0))
      ) %>%
      dplyr::select({{ opts }})
  }

  # first choice rule
  if (approach == "fc") {
    var_names <- dplyr::select(data, {{ opts }}, {{ none }}) %>%
      colnames()

    working_set <- t(apply(
      data[, var_names], 1,
      \(x) ifelse(
        names(x) == var_names[which.max(x)], 1, 0
      )
    )) %>%
      as.data.frame() %>%
      dplyr::rename_all(~var_names) %>%
      dplyr::select({{ opts }})
  }

  # prepare items
  items <- data %>%
    dplyr::select({{ opts }}) %>% # select specified opts
    colnames() # store column names only

  # define new variable names
  var_names <- c(items, paste0("new_col_names_", c(seq_len(size))))
  var_names <- make.unique(var_names, sep = "...")
  var_names <- var_names[-c(seq_along(items))]

  # create all possible combinations
  combos <- as.data.frame(t(combn(items, size))) %>%
    dplyr::rename_all(~var_names)

  # delete combos that do not include fixed items
  if (!missing(fixed)) {

    # define the fixed alternatives
    fixed_alternatives <- data %>%
      dplyr::select(tidyselect::all_of(fixed)) %>%
      colnames()

    # delete combinations that do not include fixed alternatives
    combos <- combos %>%
      dplyr::filter(apply(combos, 1, \(x) all(fixed_alternatives %in% x)))
  }

  if (!missing(prohib)) {

    # define variable names of prohibitions
    prohib_vars <- c(paste0("prohib_", c(seq_len(length(prohib)))))

    # only include combinations that do not include prohibitions
    combos <- vapply(
      X = prohib,
      FUN = \(x) apply(combos, 1, \(y) all(x %in% y)),
      FUN.VALUE = logical(nrow(combos))
    ) %>%
      as.data.frame() %>%
      dplyr::rename_all(~prohib_vars) %>%
      cbind(combos, .) %>%
      as.data.frame() %>%
      dplyr::filter(!apply(.[prohib_vars], 1, \(x) any(x))) %>%
      dplyr::select(tidyselect::all_of(names(combos)))
  }

  # calculate number of purchase options
  for (i in seq_len(nrow(combos))) {
    alternatives_included <- unname(c(unlist(combos[i, ])))

    working_set[[paste0(
      "comb.",
      paste0(alternatives_included, collapse = "_")
    )]] <-
      apply(working_set[, alternatives_included], 1, sum)
  }

  # store names of combinations
  combo_names <- dplyr::select(
    working_set,
    tidyselect::all_of(
      tidyselect::starts_with("comb.")
    )
  ) %>%
    colnames()

  # calculate reach and frequency
  turf_df <- cbind(
    combo = combo_names,
    reach = unname(apply(
      working_set[combo_names], 2,
      \(x) sum(x > 0) / length(x) * 100
    )),
    freq = unname(apply(working_set[combo_names], 2, mean))
  ) %>%
    as.data.frame() %>%
    dplyr::mutate(dplyr::across(c(reach, freq), \(x) as.numeric(x)))

  # prepare output, depending on number of combinations
  if (length(combo_names) == 1) {
    turf_df <- cbind(
      turf_df,
      t(vapply(
        X = items,
        FUN = function(x) as.integer(grepl(
          paste0(x, "_"),
          paste0(turf_df$combo, "_")
        )),
        FUN.VALUE = numeric(length(turf_df$combo))
      ))
    )
  }

  if (length(combo_names) > 1) {
    turf_df <- cbind(
      turf_df,
      vapply(
        X = items,
        FUN = \(x) as.integer(grepl(
          paste0(x, "_"),
          paste0(turf_df$combo, "_")
        )),
        FUN.VALUE = numeric(length(turf_df$combo))
      )
    )
  }

  # prepare final output
  turf_df <- turf_df %>%
    as.data.frame() %>%
    dplyr::arrange(-reach, -freq) %>%
    dplyr::mutate(
      combo = paste0("combination ", dplyr::row_number())
    )

  return(turf_df)

  # end ------------------------------------------------------------------------
}
