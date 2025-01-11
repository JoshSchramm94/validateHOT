#' Function to calculate F1-Score of confusion matrix for general product demand
#'
#' @description F1-Score is one of the 5 metrics of the confusion matrix
#' and is defined as \eqn{\frac{2 * precision * recall}{precision + recall}} or
#' stated differently by Burger (2018) \eqn{\frac{2TP}{2TP + FP + FN}},
#' where TP = True Positives, FP = False Positives, and FN = False Negatives.
#'
#' @param data A data.frame object.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `f1()` by group(s).
#' @param opts Column names of the alternatives in the validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#' @param none Column name of none alternative.
#'
#' @details
#' The current logic of `f1()` is to determine whether a binary coded
#' choice is correctly predicted by the model. To use the function, the
#' validation task must include a `none` alternative.
#' One possible application is, for example, whether a buy or a no-buy choice
#' has been correctly predicted. For example, suppose you have three
#' alternatives plus a `none` alternative and want to check whether a buy
#' or no-buy was correctly predicted. This function can be useful if you, for
#' example, test whether your model significantly overestimates or
#' underestimates a purchase likelihood.
#'
#'
#' `data` a data frame including the alternatives shown in the validation task.
#' Can be created using the `create_hot()` function.
#'
#' `group` optional grouping variable(s), if results should be displayed by
#' different groups. Has to be column name(s) of variables in `data`.
#'
#' `opts` to specify the different alternatives in the
#' validation task (also includes the `none` alternative).
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation
#' task. Input of `choice` has to be column name of actual choice.
#'
#' `none` to specify column name of the `none`
#' alternative in the validation task.
#'
#' Please be aware about the following 2x2 table regarding coding of buy and
#' no-buy choice:
#'
#' \tabular{crcc}{
#'    \tab \tab  Predicted           \tab    \cr
#' Observed \tab \tab  Buy \tab No-Buy  \cr
#'  \tab Buy \tab A \tab B  \cr
#'  \tab No-Buy \tab C \tab D  \cr
#' }
#'
#'
#' @return a tibble
#'
#' @seealso {
#' [`accuracy()`][accuracy]
#' [`precision()`][precision]
#' [`recall()`][recall]
#' [`specificity()`][specificity]
#' }
#'
#'
#' @references {
#'
#' Burger, S. V. (2018). *Introduction to Machine Learning with R:
#' Rigorous Mathematical Analysis*. O'Reilly.
#'
#' }
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
#' # f1 - without group argument defined
#' f1(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   none = none
#' )
#'
#' # f1 - with group argument defined
#' f1(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   none = none,
#'   group = group
#' )
#'
#' @export
f1 <- function(data, group, opts, choice, none) {
  # check for missing arguments ------------------------------------------------
  if (missing(none)) {
    stop('Error: argument "none" must be provided.')
  }

  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }

  if (missing(choice)) {
    stop('Error: argument "choice" must be provided.')
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

  # check for `choice` argument ------------------------------------------------

  # check for numeric input
  variable_numeric(data, variable = {{ choice }}, argument = choice)

  # check for missings in `choice`
  nvar_missings(data, variables = {{ choice }})

  # check for length of input
  ncol_input(data, variable = {{ choice }}, argument = choice)

  # end ------------------------------------------------------------------------

  # check for `none` argument --------------------------------------------------

  # check if `none` is parts of `opts`
  none_in_opts(data, none = {{ none }}, opts = {{ opts }}, should = TRUE)

  # check for missings in `opts`
  nvar_missings(data, variables = {{ none }})

  # check for length of input
  ncol_input(data, variable = {{ none }}, argument = none)

  # end ------------------------------------------------------------------------

  # check for group argument ---------------------------------------------------

  # check for missings in group
  missing_group(data, group = {{ group }})

  # end ------------------------------------------------------------------------

  # run f1() function ----------------------------------------------------------

  f1_data <- data %>%
    dplyr::mutate(

      # store column index with highest utility
      predicted = max.col(dplyr::pick({{ opts }})),
      none = colnames_match(., {{ none }}, {{ opts }}),

      # store whether it is a purchase option
      actual = ifelse({{ choice }} != none, 1, 2),
      predicted = ifelse(predicted != none, 1, 2)
    ) %>%
    dplyr::group_by(pick({{ group }})) %>%
    # calculate f1 score
    dplyr::reframe(
      f1 = 100 * ((2 * (sum(actual == 1 & predicted == 1))) /
        (((2 * (sum(actual == 1 & predicted == 1))) +
          sum(actual == 2 & predicted == 1) +
          sum(actual == 1 & predicted == 2)))
      )
    )

  return(f1_data)
}
