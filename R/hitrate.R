#' Function to calculate hit rate of validation task
#'
#' @description `hitrate()` measures number of times a choice was correctly
#' predicted in a validation task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `hitrate()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#'
#' @details
#' `hitrate()` measures number of times a participant's choice was correctly
#' predicted by the model.
#' Output contains the following 5 metrics:
#' \itemize{
#' \item `hr` hit rate (number of correctly predicted
#' choices) in percentages
#' \item `se` standard error in percentages
#' \item `chance` chance level of correctly predicted choices by just
#' guessing
#' \item `cor` absolute number of correctly predicted choices
#' \item `n` total number of choices
#' }
#'
#' `data` a data.frame object including the alternatives shown in the
#' validation task. Can be created using the `create_hot()` function.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `opts` is required to specify the different alternatives in the validation
#' task. Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation
#' task. Input of opts `choice` has to be column name of actual choice.
#'
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
#' # hit rate - without group argument defined
#' hitrate(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice
#' )
#'
#' # hit rate - with group argument defined
#' hitrate(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   group = group
#' )
#'
#' @export
hitrate <- function(data, group, opts, choice) {
  # check for missing arguments ------------------------------------------------
  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }

  if (missing(choice)) {
    stop('Error: argument "choice" must be provided.')
  }
  # end ------------------------------------------------------------------------

  # check for opts argument ----------------------------------------------------

  # check for numeric input
  variable_numeric(data, variable = {{ opts }}, argument = opts)

  # check for length of opts (> 1)
  n_opts_cols(data, opts = {{ opts }})

  # check for missings in opts
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

  # check for group argument ---------------------------------------------------

  # check for missings in group
  missing_group(data, group = {{ group }})

  # end ------------------------------------------------------------------------

  # run hitrate() --------------------------------------------------------------

  hitrate_data <- data %>%
    # store column index with highest utility
    dplyr::mutate(pred = max.col(dplyr::pick({{ opts }}))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      # calculate the hit rate
      hr = mean(as.integer({{ choice }} == pred)) * 100,
      # calculate se
      se = (sd(as.integer({{ choice }} == pred)) /
        sqrt(dplyr::n())) * 100,
      # calculate the chance level
      chance = 1 / length(dplyr::select(data, {{ opts }})) * 100,
      # calculate number of correct predicted
      cor = sum(as.integer({{ choice }} == pred)),
      n = dplyr::n() # n
    )

  return(hitrate_data)

  # end ------------------------------------------------------------------------
}
