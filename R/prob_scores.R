#' Function to calculate probability scores for (anchored) MaxDiff
#'
#' @param data A data frame containing all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s).
#' @param items A vector with the column names of the items.
#' @param set.size A vector that specifies the size of the choice set.
#' @param res A vector indicating whether individual results (`ind`) or
#' aggregated (`agg`) results should be returned.
#' @param anchor An optional variable to specify anchor variable.
#'
#'
#' @return a tibble
#'
#' @details
#' `prob_scores()` converts raw utilities from a MaxDiff to probability scores.
#' Probability scores for the unanchored MaxDiff are calculated according to the
#' formula provided by Chrzan & Orme (2019, p. 56):
#' \eqn{\frac{e^U}{(e^U + (a - 1)}}, where `U` is the raw utility of the
#' item and `a` is the number of items shown per choice task.
#'
#' For anchored MaxDiff the following formula is applied
#' \eqn{\frac{e^U}{(e^U + (a - 1)} * 100 / (1 / a)} (Chrzan & Orme, 2019,
#' pp. 59-60).
#'
#' `data` a `data.frame` object with the items (raw utilities).
#'
#' `group` optional grouping variable(s) to display results by group(s).
#' different groups. Has to be the column name(s) of variables in `data`.
#'
#' `items` specifies the items of the MaxDiff. Has to be column name of
#' variables in `data`.
#'
#' `set.size` specifies the size of the choice sets (i.e., MaxDiff tasks, how
#' many items were shown in one task). Input must be an integer.
#'
#' `res` specifies whether results should be aggregated across all participants
#' or across `group` (`res` needs to be set to `agg`) or if scores should be
#' converted for individuals only (`res` needs to be set to `ind`).
#'
#' `anchor` only required if anchored MaxDiff is applied.
#' Input for `anchor` has to  be column name of variables in `data`.
#'
#'
#' @seealso {
#' [`zero_anchored()`][zero_anchored] for zero-anchored interval
#' scores for MaxDiff
#' [`att_imp()`][att_imp] for attribute importance scores for (A)CBC
#' [`zc_diffs()`][zc_diffs] for zero-centered diff scores for (A)CBC
#' }
#'
#' @references {
#'
#' Chrzan, K., & Orme, B. K. (2019). *Applied MaxDiff: A Practitioner’s
#' Guide to Best-Worst Scaling* Provo, UT: Sawtooth Software.
#'
#' }
#'
#' @examples
#'
#' # probability scores for unanchored maxdiff - without group argument defined
#' prob_scores(
#'   data = maxdiff,
#'   items = c(option_01:option_16),
#'   set.size = 4,
#'   res = "agg"
#' )
#'
#' # probability scores for anchored maxdiff - without group argument defined
#' prob_scores(
#'   data = maxdiff,
#'   items = c(option_01:none),
#'   set.size = 4,
#'   anchor = none,
#'   res = "agg"
#' )
#'
#' @export
prob_scores <- function(data,
                        group = NULL,
                        items,
                        set.size,
                        res = c("agg", "ind"),
                        anchor = NULL) {
  # check for missing arguments ------------------------------------------------
  if (missing(items)) {
    stop('Error: argument "items" must be provided.')
  }

  if (missing(set.size)) {
    stop('Error: argument "set.size" must be provided.')
  }

  if (missing(res)) {
    stop('Error: argument "res" must be provided.')
  }

  # end ------------------------------------------------------------------------

  # check for items argument ---------------------------------------------------

  # check for numeric input
  variable_numeric(data, variable = {{ items }}, argument = items)

  # check for length of items
  n_opts_cols(data, opts = {{ items }})

  # check for missings in items
  nvar_missings(data, variables = {{ items }})

  # end ------------------------------------------------------------------------

  # check for group argument ---------------------------------------------------

  # check for missings in group
  missing_group(data, group = {{ group }})

  # end ------------------------------------------------------------------------

  # check for res argument -----------------------------------------------------

  # res can only be set to "agg" or "ind"
  allowed_input(res, c("agg", "ind"))

  # end ------------------------------------------------------------------------

  # check for set.size argument ------------------------------------------------

  # set.size has to be numeric
  numeric_vector(set.size)

  # end ------------------------------------------------------------------------

  # check for anchor argument --------------------------------------------------

  if (!missing(anchor)) {
    # check for length of anchor
    ncol_input(data, variable = {{ anchor }}, argument = anchor)

    # anchor should be part of items
    none_in_opts(data, none = {{ anchor }}, opts = {{ items }}, should = TRUE)
  }

  # end ------------------------------------------------------------------------

  # run prob_scores() ----------------------------------------------------------

  if (missing(anchor)) {
    prob_scores_data <- ch_probability(data,
      items = {{ items }},
      set.size = set.size
    )
  } else {
    prob_scores_data <- ch_probability(data,
      items = {{ items }},
      set.size = set.size, anchor = anchor
    )
  }

  if (res == "agg") {
    prob_scores_data <- prob_scores_data %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(dplyr::across({{ items }},
        c(mw = mean, std = sd),
        .names = "{.col}___{.fn}"
      )) %>%
      tidyr::pivot_longer(
        cols = tidyselect::ends_with(c("___mw", "___std")),
        names_to = c("alternative", ".value"),
        names_sep = "___"
      )
  }

  return(prob_scores_data)

  # end ------------------------------------------------------------------------
}
