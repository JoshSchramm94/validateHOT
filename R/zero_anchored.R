#' Function to calculate zero-anchored interval scores for (anchored) MaxDiff
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s).
#' @param items Vector that specifies the items.
#' @param res A vector indicating whether individual results (`ind`) or
#' aggregated (`agg`) results should be returned.
#' @param anchor An optional variable to specify anchor variable.
#'
#'
#' @details
#' `zero_anchored()` converts raw utilities of a MaxDiff to zero-anchored
#' interval scores that have a range of 100.
#'
#' For anchored MaxDiff the anchor is set to 0.
#'
#' `data` a data.frame object with the items (raw utilities).
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `items` specifies the items of the MaxDiff. Has to be column name of
#' variables in `data`.
#'
#' `res` specifies whether results should be aggregated across all participants
#' or across `group` (`res` needs to be set to `agg`) or if scores should be
#' converted for individuals only (`res` needs to be set to `ind`).
#'
#' `anchor` only needs to be specified if anchored MaxDiff is applied.
#' Input for `anchor` has to to be column name of variables in `data`.
#'
#'
#' @return a tibble
#'
#'
#' @seealso {
#' [`att_imp()`][att_imp] for attribute importance scores for (A)CBC
#' [`prob_scores()`][prob_scores] for probability scores for MaxDiff
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
#' # zero-anchored interval scores for unanchored MaxDiff - without group
#' # argument defined
#' zero_anchored(
#'   data = maxdiff,
#'   items = c(option_01:option_16),
#'   res = "agg"
#' )
#' \dontrun{
#' # zero-anchored interval scores for unanchored MaxDiff - with group defined
#' zero_anchored(
#'   data = maxdiff,
#'   group = group,
#'   items = c(option_01:option_16),
#'   res = "agg"
#' )
#' }
#'
#' # zero-anchored interval scores for anchored MaxDiff - without group defined
#' zero_anchored(
#'   data = maxdiff,
#'   items = c(option_01:none),
#'   anchor = none,
#'   res = "agg"
#' )
#' \dontrun{
#' # zero-anchored interval scores for anchored MaxDiff - with group defined
#' zero_anchored(
#'   data = maxdiff,
#'   group = group,
#'   items = c(option_01:none),
#'   anchor = none,
#'   res = "agg"
#' )
#' }
#'
#' @export
zero_anchored <- function(data,
                          group = NULL,
                          items,
                          res = c("agg", "ind"),
                          anchor = NULL) {
  # check for missing arguments ------------------------------------------------
  if (missing(items)) {
    stop('Error: argument "items" must be provided.')
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

  # check for anchor argument --------------------------------------------------

  if (!missing(anchor)) {
    # check for length of anchor
    ncol_input(data, variable = {{ anchor }}, argument = anchor)

    # anchor should be part of items
    none_in_opts(data, none = {{ anchor }}, opts = {{ items }}, should = TRUE)
  }

  # end ------------------------------------------------------------------------

  # run zero_anchored() --------------------------------------------------------

  # zero anchor utilities
  if (missing(anchor)) {
    zero_anchored_data <- zero_anchor_utilities(data, items = {{ items }})
  } else {
    zero_anchored_data <- zero_anchor_utilities(data, items = {{ items }},
                                                anchor = {{ anchor }})
  }

  # if res == "agg" aggregate results
  if (res == "agg") {
    zero_anchored_data <- zero_anchored_data %>%
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

  return(zero_anchored_data)

  # end ------------------------------------------------------------------------
}
