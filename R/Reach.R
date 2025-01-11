#' Function to calculate percentage of participants that are reached by
#' assortment
#'
#' @description
#' `reach()` of T(otal) U(nduplicated) R(each) and F(requency)
#' analysis to measure the number of  the averaged percentage of how
#' many participants you can reach (at least one of the products resemble
#' a purchase option) is reached with a specific product bundle assortment.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `reach()` by group(s).
#' @param opts Column names of the alternatives included in the assortment.
#' @param none Column name of none / threshold alternative.
#'
#' @details
#' `reach()` calculates the the percentage of consumers that are
#' reached with a particular product assortment. The current
#' logic of `reach()` is that the utility of an alternative has to
#' exceed a threshold. In the case of `reach()` this threshold is referred
#' to the `none` argument in `data`, however, could also be, e.g., a
#' current product.
#'
#'
#' `data` a data.frame object including the alternatives that should
#' be tested.
#'
#' `group` optional grouping variable, if results should be displayed
#' by different groups. Has to be column name of variables in `data`.
#'
#' `opts` defines product assortment that should be considered.
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `none` to specify column name of the `none` alternative (i.e.,
#' threshold variable).
#'
#'
#' @return a tibble
#'
#' @examples
#'
#' hot <- create_hot(
#'   data = maxdiff,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(2, 9, 10, 14, 15, 16, 17),
#'   method = "maxdiff",
#'   varskeep = "group",
#'   choice = "hot"
#' )
#'
#' # reach - without group argument defined
#' reach(
#'   data = hot,
#'   opts = c(option_1, option_2, option_6),
#'   none = none
#' )
#'
#' # reach - with group argument defined
#' reach(
#'   data = hot,
#'   opts = c(option_1, option_2, option_6),
#'   none = none,
#'   group = group
#' )
#'
#' @export
reach <- function(data, group, none, opts) {
  # check for missing arguments ------------------------------------------------
  if (missing(none)) {
    stop('Error: argument "none" must be provided.')
  }

  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }
  # end ------------------------------------------------------------------------

  # check for opts argument ----------------------------------------------------

  # check for numeric input
  variable_numeric(data, variable = {{ opts }}, argument = opts)

  # check for missings in opts
  nvar_missings(data, variables = {{ opts }})

  # end ------------------------------------------------------------------------

  # check for `none` argument --------------------------------------------------

  # check if `none` is parts of `opts`
  none_in_opts(data, none = {{ none }}, opts = {{ opts }}, should = FALSE)

  # check for missings in `opts`
  nvar_missings(data, variables = {{ none }})

  # check for length of input
  ncol_input(data, variable = {{ none }}, argument = none)

  # check for numeric input
  variable_numeric(data, variable = {{ none }}, argument = none)

  # end ------------------------------------------------------------------------

  # check for group argument ---------------------------------------------------

  # check for missings in group
  missing_group(data, group = {{ group }})

  # end ------------------------------------------------------------------------

  # run reach() function -------------------------------------------------------

  # store option names
  variables <- dplyr::select(data, {{ opts }}) %>% colnames()

  reach_data <- data %>%
    # mark those with higher than none utility as purchase option
    dplyr::mutate(
      dplyr::across(
        {{ opts }},
        \(x) ifelse(x > {{ none }}, 1, 0)
      )
    ) %>%
    # count the percentage of people with purchase option
    dplyr::mutate(options = apply(.[variables], 1, sum)) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(reach = mean(options >= 1) * 100)

  return(reach_data)

  # end ------------------------------------------------------------------------
}
