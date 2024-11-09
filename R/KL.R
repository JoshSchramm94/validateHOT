#' Function to calculate Kullback-Leibler divergence of validation/holdout
#' task
#'
#' @description Function to measure the Kullback-Leibler Divergence of a
#' validation/holdout task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `kl()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout
#' task.
#' @param epsilon A vector of noise that should be added to 0 values, per
#' default set to 1e-05 (see Drost, 2018).
#' @param base A character string to define the logarithm base, currently two
#' are provided, namely `log` (default) and `log2`.
#'
#' @return a tibble
#' @importFrom dplyr select mutate group_by pick count reframe
#' @importFrom magrittr "%>%"
#'
#' @details
#' Kullback-Leibler-Divergence measures the divergence between the actual
#' choice distribution and the predicted choice distribution (Ding et al., 2011;
#' Drost, 2018). Currently only provides the deviation measured based on
#' \eqn{log} and \eqn{log{_2}} algorithm. \eqn{log} set as default.
#'
#' Due to Kullback-Leibler divergence's asymmetry, the output provides both
#' `KL_O_P` which is equivalent to (Observed || Predicted) and
#' `KL_P_O` which is equivalent to (Predicted || Observed).
#'
#' `data` has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the `createHOT()`
#' function.
#'
#' `group` optional grouping variable, if results should be displayed
#' by different groups. Has to be column name of variables in `data`.
#'
#' `opts` is required to specify the different alternatives in the
#' validation/holdout task (also includes the `none` alternative).
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation/holdout
#' task. Input of opts `choice` has to be column name of actual choice.
#'
#' `epsilon` has to be a numeric input in case of 0 in the numerator or
#' denominator. 0 then will be replaced by `epsilon`. Default value
#' is `epsilon = 1e-5`, however, can be adopted (see also Drost, 2018).
#'
#' `base` has to be a character string, deciding which logarithm base
#' you want to apply to calculate Kullback-Leibler. You can choose
#' between \eqn{log} and \eqn{log{_2}}. Default is set to \eqn{log}.
#'
#' @references {
#'
#' Ding, Min, John R. Hauser, Songting Dong, Daria Dzyabura, Zhilin Yang,
#' SU Chenting, and Steven P. Gaskin. (2011).
#' Unstructured Direct Elicitation of Decision Rules. \emph{Journal of
#' Marketing Research 48}(1): 116-27.
#' \verb{https://doi.org/10.1509/jmkr.48.1.116}.
#'
#'
#' Drost, Hajk-Georg. (2018). Philentropy: Information Theory and Distance
#' Quantification with R. \emph{Journal of Open Source Software 3}(26),
#' 765, \verb{https://joss.theoj.org/papers/10.21105/joss.00765}.
#'
#' }
#'
#' @examples
#'
#' library(validateHOT)
#'
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   none = 19,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20,
#'   varskeep = 21
#' )
#'
#' # kl with log base - without group argument defined
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log"
#' )
#'
#' \dontrun{
#' # kl with log2 base - without group argument defined
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log2"
#' )
#'
#' # kl grouped - log + specifying epsilon
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log",
#'   group = Group,
#'   epsilon = 1e-8
#' )
#'
#' # kl grouped - log2
#' kl(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   base = "log2",
#'   group = Group
#' )
#' }
#'
#' @export

kl <- function(data, group, opts, choice, epsilon = NULL, base = NULL) {
  # specify epsilon if not defined
  if (is.null(epsilon)) {
    epsilon <- .00001
  }

  if (!is.numeric(epsilon)) {
    stop("Error: 'epsilon' has to be numeric!")
  }

  # specify base if not defined
  if (is.null(base)) {
    base <- "log"
  }

  if ((base != "log") & (base != "log2")) {
    stop("Error: base can only be 'log' or 'log2'!")
  }

  if (length(data %>% dplyr::select({{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  if (length(data %>% dplyr::select({{ opts }})) == 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
  }

  # grouping variable
  ## check for missings
  if (anyNA(data %>% dplyr::select({{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }

  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select({{ opts }}) %>%
    colnames()

  ## check whether variable is numeric
  for (i in seq_along(alternatives)) {
    if (!is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'opts' has to be numeric!")
    }
  }

  ## check for missings
  if (anyNA(data %>% dplyr::select({{ opts }}))) {
    stop("Error: 'opts' contains NAs!")
  }

  # choice
  ## check for missing
  if (anyNA(data %>% dplyr::select({{ choice }}))) {
    stop("Error: 'choice' contains NAs!")
  }

  ## check for str
  choi <- data %>%
    dplyr::select({{ choice }}) %>%
    colnames()

  if (!is.numeric(data[[choi]])) {
    stop("Error: 'choice' has to be numeric!")
  }

  # create actual share of actual choice
  WS1 <- data %>%
    dplyr::mutate(
      # create factor for actual choice
      alt = factor(
        {{ choice }},
        levels = c(seq_along(dplyr::select(data, {{ opts }}))),
        labels = paste0(
          "Option_",
          c(seq_along(dplyr::select(data, {{ opts }})))
        )
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(alt, .drop = FALSE) %>% # count choices
    dplyr::mutate(chosen = n / sum(n)) %>% # calculate percentage
    dplyr::select(-"n") # drop variable

  # create share of predicted choice
  WS2 <- data %>%
    # store column index of highest utility
    dplyr::mutate(pred = max.col(pick({{ opts }}))) %>%
    dplyr::mutate(
      alt = factor(
        pred,
        levels = c(seq_along(dplyr::select(data, {{ opts }}))),
        labels = paste0("Option_", c(seq_along(
          dplyr::select(data, {{ opts }})
        )))
      )
    ) %>% # create factor
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(alt, .drop = FALSE) %>% # count number of predicted choice
    dplyr::mutate(pred = n / sum(n)) %>% # calculate percentage
    dplyr::select(-"n") # drop variable

  # if base set to 'log'
  if (base == "log") {
    KL_data <- WS1 %>%
      # merge both data frames
      merge(
        x = .,
        y = WS2,
        by = c(WS1 %>%
          dplyr::select({{ group }}) %>% colnames(), "alt")
      ) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::mutate(
        chosen = ifelse(chosen == 0, epsilon, chosen), # add epsilon if 0
        pred = ifelse(pred == 0, epsilon, pred) # add epsilon if 0
      ) %>%
      dplyr::reframe(
        kl_o_p = sum(chosen * log(chosen / pred)),
        kl_p_o = sum(pred * log(pred / chosen))
      )

    return(KL_data)
  }

  # if base set to 'log2'
  if (base == "log2") {
    KL_data <- WS1 %>%
      # merge both data frames
      merge(
        x = .,
        y = WS2,
        by = c(WS1 %>% dplyr::select({{ group }}) %>% colnames(), "alt")
      ) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::mutate(
        chosen = ifelse(chosen == 0, epsilon, chosen), # add epsilon if 0
        pred = ifelse(pred == 0, epsilon, pred) # add epsilon if 0
      ) %>%
      dplyr::reframe(
        kl_o_p = sum(chosen * log2(chosen / pred)),
        kl_p_o = sum(pred * log2(pred / chosen))
      )

    return(KL_data)
  }
}
