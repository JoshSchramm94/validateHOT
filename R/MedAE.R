#' Function to calculate median absolute error
#'
#' @description
#' `medae()` calculates the median absolute error of a validation task.
#'
#' @param data A data frame containing all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to calculate `medae()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#'
#' @details
#' `medae()` calculates the deviation between predicted and
#' stated (actual) choice shares and returns the median error in the
#' validation task.
#'
#' `data` a `data.frame` object including the alternatives shown in the
#' validation task. It can be created using the `create_hot()` function.
#'
#' `group` optional grouping variable(s) to display results by group(s).
#' Has to be the column name(s) of variables in `data`.
#'
#' `opts` to specify the different alternatives in the
#' validation task (also includes the `none` alternative).
#'
#' `choice` to specify the column of the actual choice in the validation
#' task. The input of `choice` has to be the column name of the actual choice.
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
#' # medae - without group argument defined
#' medae(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice
#' )
#'
#' # medae - with group argument defined
#' medae(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   group = group
#' )
#'
#' @export
medae <- function(data, group, opts, choice) {
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

  # run medae() --------------------------------------------------------------

  # define factor labels
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
    dplyr::count(merger, .drop = FALSE) %>%
    dplyr::mutate(chosen = percentage(n) * 100) %>%
    dplyr::select(-"n")

  n_opts_rep <- length(WS1$merger)

  # predicted share of choice
  WS2 <- data %>%
    mnl({{ opts }}) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      dplyr::across({{ opts }}, \(x) mean(x))
    ) %>%
    tidyr::pivot_longer(
      cols = {{ opts }},
      names_to = "alternatives",
      values_to = "share"
    ) %>%
    dplyr::mutate(
      merger = factor(
        x = rep(factor_labels,
          length.out = n_opts_rep,
          labels = factor_labels
        )
      )
    )

  # define merging variables
  merge_variables <- c(dplyr::select(WS2, {{ group }}) %>%
    colnames(), "merger")

  medae_data <- WS2 %>%
    merge(
      x = .,
      y = WS1,
      by = merge_variables
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(medae = median(abs(share - chosen)))

  return(medae_data)

  # end ------------------------------------------------------------------------
}
