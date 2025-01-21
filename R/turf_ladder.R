#' Function to run Total Unduplicated Reach and Frequency ladder technique
#'
#' @description
#' T(otal) U(nduplicated) R(each) and F(requency) is a "product line extension
#' model" (Miaoulis et al., 1990, p. 29). This function calculates the so-called
#' TURF ladder by starting with one alternative (or a fixed set of alternatives)
#' and then subsequently adds the next best alternative to add for increasing
#' the reach.
#'
#' @param data A data frame containing all relevant variables.
#' @param opts Column names of the alternatives included in the assortment.
#' @param none Column name of the none / threshold alternative.
#' @param fixed An optional vector to determine alternatives / items that have
#' to be included in the assortment.
#' @param approach A character whether to run the first-choice approach
#' (`'fc'`) or threshold approach (`'thres'`).
#'
#' @details
#'
#' `data` a `data.frame` object containing the alternatives that
#' should be tested.
#'
#' `opts` to specify the different alternatives in the product assortment that
#' should be considered. Input of `opts` has to be the column names of variables
#' in `data`.
#'
#' `none` to specify the column name of the `none` alternative in `data` (i.e.,
#' variable that needs to be exceeded).
#'
#' `fixed` has to be a vector of variables that are fixed in the
#' assortment, i.e., they must be part of the assortment.
#'
#' `approach` character defining whether first choice approach (`approach = 'fc'`)
#' or threshold approach (`approach = 'thres'`) should be applied for running
#' `turf_ladder()`. If `approach = 'fc'`, participants are considered to be
#' reached, if their alternative with the highest utility is included in the
#' assortment and this alternative's utility is larger than the threshold's
#' utility (Chrzan & Orme, 2019, p. 111).
#' On the contrary, if `approach = 'thres'`, participants are considered
#' to be reached, if the utility of one product is higher than the one of the
#' `none` alternative (Chrzan & Orme, 2019, p. 112).
#' If `approach = 'fc'`, `reach` equals `freq` since participants have at
#' maximum their most preferred alternative that exceeds the `none`
#' alternative.
#'
#' Returns a data frame with the size of the portfolio, the reach score, the
#' reach add (i.e., additional reach for adding this new alternative to the
#' portfolio), frequency, the frequency add (i.e., additional frequency for
#' adding this new alternative to the portfolio), and the coding of which
#' alternatives are present in this portfolio (indicated by `1`).
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
#' @seealso {
#' [`turf()`][turf]
#' }
#'
#' @examples
#'
#' # turf ladder, no fixed alternatives
#' turf_ladder(
#'   data = maxdiff,
#'   opts = c(option_01:option_16),
#'   none = none,
#'   approach = "thres"
#' )
#'
#' # turf ladder, alternative and 4 and 5 fixed
#' turf_ladder(
#'   data = maxdiff,
#'   opts = c(option_01:option_16),
#'   none = none,
#'   fixed = c("option_04", "option_05"),
#'   approach = "thres"
#' )
#'
#' @export
turf_ladder <- function(data,
                        opts,
                        none,
                        approach = c("thres", "fc"),
                        fixed = NULL) {
  # check for missing arguments ------------------------------------------------
  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }

  if (missing(none)) {
    stop('Error: argument "none" must be provided.')
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

  # check for fixed argument ---------------------------------------------------

  if (!missing(fixed)) {
    vars_in_opts(data, vars = fixed, opts = {{ opts }}, arg_name = "fixed")
  }

  # end ------------------------------------------------------------------------

  # check approach argument ----------------------------------------------------

  # check valid input in approach
  allowed_input(approach, c("thres", "fc"))

  # end ------------------------------------------------------------------------

  # run turf_ladder() ----------------------------------------------------------

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
  items <- colnames(working_set)


  if (missing(fixed)) {
    length_opts <- ncol(working_set)

    winner <- names(which.max(apply(working_set, 2, mean)))
  }


  if (!missing(fixed)) {
    length_opts <- ncol(working_set) - length(fixed)

    winner <- fixed
  }

  results_df <- data.frame(
    combo = paste0("comb.", paste0(winner, collapse = "_")),
    reach = mean(apply(working_set[winner], 1, sum) > 0) * 100,
    freq = mean(apply(working_set[winner], 1, sum))
  )

  results_df[items] <- 0
  results_df[winner] <- 1

  # working_set_bu <- working_set

  # create all possible combinations
  for (i in seq_len(length_opts)) {
    # skip first loop if fixed is not provided
    if (i == 1 && missing(fixed)) {
      next
    }

    # update size for the case where fixed variables are provided
    if (!missing(fixed)) {
      i <- i + length(fixed)
    }

    # define new variable names
    var_names <- c(items, paste0("new_col_names_", c(seq_len(i))))
    var_names <- make.unique(var_names, sep = "...")
    var_names <- var_names[-c(seq_along(items))]

    # create all possible combinations
    combos <- as.data.frame(t(combn(items, i))) %>%
      dplyr::rename_all(~var_names) %>%
      dplyr::filter(apply(., 1, \(x) all(winner %in% x)))

    # caclulate the reach and freq of each combination
    for (k in seq_len(nrow(combos))) {
      alternatives_included <- unname(c(unlist(combos[k, ])))

      working_set[[paste0(
        "comb.",
        paste0(alternatives_included, collapse = "_")
      )]] <-
        apply(working_set[alternatives_included], 1, sum)
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
      dplyr::mutate(dplyr::across(c(reach, freq), \(x) as.numeric(x))) %>%
      dplyr::arrange(-reach, -freq) %>%
      dplyr::slice(1)

    turf_df <- cbind(
      turf_df,
      t(vapply(
        X = items,
        FUN = \(x) as.integer(grepl(
          paste0(x, "_"),
          paste0(turf_df$combo, "_")
        )),
        FUN.VALUE = numeric(length(turf_df$combo))
      ))
    )

    # store the results
    results_df <- rbind(results_df, turf_df)

    # correct for i, if fixed are provided
    if (!missing(fixed)) {
      i <- i - length(fixed)
    }

    # update winner vector
    winner <- c(items[(which(results_df[i, items] == 1))])

    # update working set (i.e., reset from previous round)
    working_set <- dplyr::select(working_set, {{ opts }})
  }

  # prepare final output
  results_df <- results_df %>%
    dplyr::select(-combo) %>%
    dplyr::mutate(
      size = apply(.[items], 1, sum),
      add_reach = reach - dplyr::lag(reach),
      add_freq = freq - dplyr::lag(freq)
    ) %>%
    dplyr::relocate(add_reach, .after = reach) %>%
    dplyr::relocate(add_freq, .after = freq) %>%
    dplyr::relocate(size, .before = tidyselect::everything())

  return(results_df)
}
