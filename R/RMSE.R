#' Function to calculate Root Mean Square Error of validation/holdout task
#'
#' @description
#' `rmse()` measures the root mean square error
#' of a validation/holdout task. Calculates the averaged
#' root mean square error of the stated and predicted share of alternatives
#' in the validation/holdout task.
#'
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `rmse()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout
#' task.
#'
#' @details
#' Root mean square error (RMSE) calculates the root mean square error when
#' comparing the share of the actual choice in the holdout task and the
#' predicted share.
#'
#'
#' `data` has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the `createHOT()`
#' function.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `opts` is required to specify the different alternatives in the
#' validation/holdout task. Input of `opts` has to be column names
#' of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation/holdout
#' task. Input of opts `choice` has to be column name of actual choice.
#'
#'
#' @return a tibble
#' @importFrom dplyr select mutate group_by pick count rowwise ungroup across
#' reframe
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect ends_with
#'
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
#' # rmse - without group argument defined
#' rmse(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice
#' )
#'
#' # rmse - with group argument defined
#' rmse(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   group = Group
#' )
#'
#' @export

rmse <- function(data, group, opts, choice) {
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
      # factorize choice
      merger = factor(
        {{ choice }},
        levels = c(seq_along(dplyr::select(data, {{ opts }}))),
        labels = c(seq_along(dplyr::select(data, {{ opts }})))
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # count choice
    dplyr::count(merger, .drop = FALSE) %>%
    # calculate percentage
    dplyr::mutate(chosen = n / sum(n) * 100) %>%
    dplyr::select(-"n") # drop variable

  # create share of predicted choice
  suppressMessages(WS2 <- data %>%
    dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>% # exponentiate
    dplyr::rowwise() %>%
    dplyr::mutate(Summe = sum(dplyr::pick({{ opts }}))) %>% # create sum
    dplyr::ungroup() %>%
    # calculate choice probability in percentage
    dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # aggreagte choice probability
    dplyr::reframe(across({{ opts }}, ~ mean(.x),
      .names = "{.col}_mean"
    )) %>%
    tidyr::pivot_longer(.,
      cols = tidyselect::ends_with("_mean"),
      names_to = "alt",
      values_to = "mean"
    ) %>%
    dplyr::mutate(
      # adjust labeling
      alt = substr(alt, 1, (nchar(alt) - nchar("_mean"))),
      # prepare merge helper variable
      merger = rep(seq_along(dplyr::select(data, {{ opts }})),
        length.out = length(alt)
      )
    ))

  rmse_data <- WS2 %>%
    merge(
      x = .,
      y = WS1,
      by = c(WS2 %>% dplyr::select({{ group }}) %>%
        colnames(), "merger")
    ) %>% # merge
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(RMSE = (abs(mean - chosen))^2) %>% # calculate RMSE
    dplyr::reframe(rmse = sqrt(mean(RMSE)))

  return(rmse_data)
}
