#' Function to calculate averaged number of times a participant is reached by
#' assortment
#'
#' @description
#' Frequency function of T(otal) U(nduplicated) R(each) and F(requency)
#' analysis to measure the average time a consumer is reached with a particular
#' product bundle assortment.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `freqassort()` by group(s).
#' @param opts Column names of the alternatives included in the assortment.
#' @param none Column name of none / threshold alternative.
#'
#' @details
#' Frequency calculates the average times a consumer would be reached with the
#' tested product assortment. The current logic of `freqassort()` is that the
#' utility of an alternative has to exceed a threshold. For `freqassort()`
#' this threshold is referred to the `none` argument in `data`.
#' The frequency is calculated based on the 'threshold' approach, i.e.,
#' each alternative that exceeds utility of `none` alternative is
#' considered as, for example, purchase option.
#'
#' `data` a data frame including the alternatives that should
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
#' # freqassort - without group argument defined
#' freqassort(
#'   data = hot,
#'   opts = c(option_1, option_2, option_6),
#'   none = none
#' )
#'
#' # freqassort - with group argument defined
#' freqassort(
#'   data = hot,
#'   opts = c(option_1, option_2, option_6),
#'   none = none,
#'   group = group
#' )
#'
#' @export
freqassort <- function(data, group, none, opts) {

  # check for missing arguments ------------------------------------------------
  if (missing(none)) {
    stop('Error: argument "none" must be provided.')
  }

  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }
  # end ------------------------------------------------------------------------

  # check for `opts` argument --------------------------------------------------

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

  # run freq_assort() function -------------------------------------------------

  # store variable names
  variables <- dplyr::select(data, {{ opts }}) %>% colnames()

  freqassort_data <- data %>%

    # recode as purchase option if utility is larger than no-buy
    dplyr::mutate(
      dplyr::across(
        {{ opts }},
        \(x) ifelse(x > {{ none }}, 1, 0)
      )
    ) %>%

    # calculate number of purchase options
    dplyr::mutate(options = apply(.[variables], 1, sum)) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(freq = mean(options))

  return(freqassort_data)

  # end ------------------------------------------------------------------------
}
