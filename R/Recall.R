#' Function to calculate recall of confusion matrix for general product demand
#'
#' @description
#' `recall()` is one of the 5 metrics of the confusion matrix
#' and is defined as \eqn{\frac{TP}{TP + FN}}, where TP =
#' True Positives, FN = False Negatives (see, e.g., Burger, 2018).
#'
#' @param data A data frame containing all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `recall()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#' @param none Column name of the none alternative.
#'
#' @details
#' The current logic of `recall()` is to determine whether a binary-coded
#' choice is correctly predicted by the model. To use the function, the
#' validation task must include a `none` alternative.
#' A potential application could involve evaluating whether a buy or a no-buy
#' choice has been correctly predicted. For example, consider a scenario with
#' three alternatives plus a `none` alternative. You may want to verify if the
#' model correctly predicted a buy or no-buy decision (i.e., overestimates or
#' underestimates the purchase likelihood). The `recall()` function can be
#' useful for assessing the recall score of that 2x2 matrix.
#'
#' `data` a `data.frame` object including the alternatives shown in the
#' validation task. It can be created using the `create_hot()` function.
#'
#' `group` optional grouping variable(s) to display results by group(s).
#' Has to be the column name(s) of variables in `data`.
#'
#' `opts` to specify the different alternatives in the
#' validation task (also includes the `none` alternative).
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify the column of the actual choice in the validation
#' task. The input of `choice` has to be the column name of the actual choice.
#'
#' `none` to specify the column name of the `none`
#' alternative in the validation task.
#'
#' Please be aware of the following 2x2 table regarding the coding of buy and
#' no-buy choices:
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
#' [`f1()`][f1]
#' [`precision()`][precision]
#' [`specificity()`][specificity]
#' }
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
#' # recall - without group argument defined
#' recall(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   none = none
#' )
#'
#' # recall - with group argument defined
#' recall(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   none = none,
#'   group = group
#' )
#'
#' @export
recall <- function(data, group, opts, choice, none) {
  # check for missing arguments ------------------------------------------------
  if (missing(none)) {
    stop('Error: argument "none" is missing!')
  }

  if (missing(opts)) {
    stop('Error: argument "opts" is missing!')
  }

  if (missing(choice)) {
    stop('Error: argument "choice" is missing!')
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

  # run recall() function ------------------------------------------------------

  recall_data <- data %>%
    dplyr::mutate(

      # store column index with highest utility
      predicted = max.col(dplyr::pick({{ opts }})),
      none = colnames_match(., {{ none }}, {{ opts }}),

      # check whther purchase option
      actual = ifelse({{ choice }} != none, 1, 2),
      predicted = ifelse(predicted != none, 1, 2)
    ) %>%
    dplyr::group_by(pick({{ group }})) %>%
    # calculate recall score
    dplyr::reframe(
      recall = 100 * (sum(actual == 1 & predicted == 1) /
        (sum(actual == 1 & predicted == 1) +
          sum(actual == 1 & predicted == 2)))
    )

  return(recall_data)
}
