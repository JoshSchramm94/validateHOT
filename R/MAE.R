#' Function to calculate mean absolute error of validation task
#'
#' @description `mae()` measures the mean absolute error of a
#' validation task, i.e., aggregated deviation between predicted and
#' stated share of alternatives in the validation task.
#'
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `mae()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#'
#' @details
#' Mean absolute error calculates the deviation between predicted and
#' stated (i.e., actual) choice share. It is an aggregated value across all
#' alternatives in the validation task.
#'
#' `data` a data.frame object including the alternatives shown in the
#' validation task. Can be created using the `create_hot()` function.
#'
#' `group` optional grouping variable(s), if results should be displayed by
#' different groups. Has to be column name(s) of variables in `data`.
#'
#' `opts` to specify the different alternatives in the validation task.
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation
#' task. Input of `choice` has to be column name of actual choice.
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
#'   choice = "hot",
#'   varskeep = "group"
#' )
#'
#' # mae - without group argument defined
#' mae(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice
#' )
#'
#' # mae - with group argument defined
#' mae(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   group = group
#' )
#'
#' @export
mae <- function(data, group, opts, choice) {
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

  # check for length of opts (>1)
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

  # run mae() ------------------------------------------------------------------

  # create factor labels
  factor_labels <- define_fctr_labels(data, {{ opts }}, "option_")

  # actual share of choice
  WS1 <- data %>%
    dplyr::mutate(
      merger = factor(
        x = {{ choice }},
        levels = c(seq_along(factor_labels)),
        labels = factor_labels
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # count choices
    dplyr::count(merger, .drop = FALSE) %>%
    # calculate percentage
    dplyr::mutate(chosen = percentage(n) * 100) %>%
    dplyr::select(-"n")

  # define number of times factor_labels needs to be repeated
  n_opts_rep <- length(WS1$merger)

  # predicted share of choice
  WS2 <- data %>%
    # run multinomial logit analysis
    mnl({{ opts }}) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # calculate mean of predicted shares
    dplyr::reframe(
      dplyr::across({{ opts }}, \(x) mean(x))
    ) %>%
    tidyr::pivot_longer(
      cols = {{ opts }},
      names_to = "alternatives",
      values_to = "share"
    ) %>%
    # prepare for merging
    dplyr::mutate(
      merger = factor(
        x = rep(factor_labels,
          length.out = n_opts_rep,
          labels = factor_labels
        )
      )
    )

  # define merge variables
  merge_variables <- c(dplyr::select(WS2, {{ group }}) %>%
    colnames(), "merger")

  mae_data <- WS2 %>%
    merge(
      x = .,
      y = WS1,
      by = merge_variables
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(mae = mean(abs(share - chosen)))

  return(mae_data)

  # end ------------------------------------------------------------------------
}
