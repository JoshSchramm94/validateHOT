#' Function to calculate Kullback-Leibler divergence of validation
#' task
#'
#' @param data A data frame containing all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `kl()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#' @param epsilon A vector of noise that should be added to `0` values, per
#' default set to `1e-05`.
#' @param log_base A character string to define the logarithm base, currently
#' `log` (default) and `log2` are provided.
#'
#' @return a tibble
#'
#' @details
#' Kullback-Leibler-Divergence calculates the divergence between the actual
#' choice distribution and the predicted choice distribution (Ding et al., 2011;
#' Drost, 2018). Currently, `kl()` provides the deviation measured based on
#' \eqn{log} and \eqn{log{_2}} algorithm. \eqn{log} is set as default.
#'
#' Due to Kullback-Leibler divergence's asymmetry, the output provides both
#' `KL_O_P` which is equivalent to (Observed || Predicted) and
#' `KL_P_O` which is equivalent to (Predicted || Observed).
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
#' `epsilon` numeric input in case of `0` in the numerator or
#' denominator. `0` then will be replaced by `epsilon`. The default value
#' is `epsilon = 1e-5` but can be adopted (see also Drost, 2018).
#'
#' `log_base` has to be a character string, deciding which logarithm base
#' you want to apply to calculate Kullback-Leibler. You can choose
#' between \eqn{log} and \eqn{log{_2}}. The default is set to \eqn{log}.
#'
#' @references {
#'
#' Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang,
#' SU Chenting, and Steven P. Gaskin. (2011).
#' Unstructured Direct Elicitation of Decision Rules. *Journal of
#' Marketing Research 48*(1): 116-27.
#' \verb{https://doi.org/10.1509/jmkr.48.1.116}.
#'
#'
#' Drost, Hajk-Georg. (2018). Philentropy: Information Theory and Distance
#' Quantification with R. *Journal of Open Source Software 3*(26),
#' 765, \verb{https://joss.theoj.org/papers/10.21105/joss.00765}.
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
#'   choice = "hot",
#'   varskeep = "group"
#' )
#'
#' # kl with log base - without group argument defined
#' kl(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   log_base = "log"
#' )
#'
#' \dontrun{
#' # kl with log2 base - without group argument defined
#' kl(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   log_base = "log2"
#' )
#'
#' # kl grouped - log + specifying epsilon
#' kl(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   log_base = "log",
#'   group = group,
#'   epsilon = 1e-8
#' )
#'
#' # kl grouped - log2
#' kl(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   log_base = "log2",
#'   group = group
#' )
#' }
#'
#' @export
kl <- function(data,
               group = NULL,
               opts,
               choice,
               epsilon = NULL,
               log_base = NULL) {
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

  # check epsilon argument -----------------------------------------------------
  if (missing(epsilon)) {
    epsilon <- .00001
  }

  # epsilon needs to be numeric
  numeric_vector(epsilon)

  # end ------------------------------------------------------------------------

  # check log_base argument ----------------------------------------------------

  # specify log_base if not defined
  if (missing(log_base)) {
    log_base <- "log"
  }

  # log_base can only be set to "log" or "log2"
  allowed_input(log_base, c("log", "log2"))
  # end ------------------------------------------------------------------------

  # run kl() -------------------------------------------------------------------

  # create factor labels
  factor_labels <- define_fctr_labels(data, {{ opts }}, "option_")

  # actual share of choice
  kl_actual <- data %>%
    dplyr::mutate(
      alt = factor(
        x = {{ choice }},
        levels = c(seq_along(factor_labels)),
        labels = factor_labels
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # count choices
    dplyr::count(alt, .drop = FALSE) %>%
    # calculate percentage
    dplyr::mutate(chosen = percentage(n)) %>%
    dplyr::select(-"n")

  # predicted share of choice
  kl_predicted <- data %>%
    dplyr::mutate(pred = max.col(pick({{ opts }}))) %>%
    dplyr::mutate(
      alt = factor(
        x = pred,
        levels = c(seq_along(factor_labels)),
        labels = factor_labels
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # count choices
    dplyr::count(alt, .drop = FALSE) %>%
    # calculate percentage
    dplyr::mutate(pred = percentage(n)) %>%
    dplyr::select(-"n")

  merge_variables <- c("alt", dplyr::select(data, {{ group }}) %>% colnames())

  kl_data <- kl_actual %>%
    # merge actual and predicted
    merge(
      x = .,
      y = kl_predicted,
      by = merge_variables
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(
      # add epsilon if cell equals 0
      dplyr::across(
        c(chosen, pred), \(x) ifelse(x == 0, epsilon, x)
      )
    ) %>%
    dplyr::reframe(
      kl_o_p = kl_observed_predicted(chosen, pred, log_base),
      kl_p_o = kl_predicted_observed(chosen, pred, log_base)
    )

  return(kl_data)

  # end ------------------------------------------------------------------------
}
