#' Function to calculate mean absolute error of validation/holdout task
#'
#' @description mae measures the mean absolute error of a
#' validation/holdout task, i.e., aggregated deviation between predicted and
#' stated share of alternatives in the validation/holdout task.
#'
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `mae()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout
#' task.
#'
#' @details
#' Mean absolute error (MAE) calculates the deviation between predicted and
#' stated (actual) choice share. It is an aggregated value across all
#' alternatives in the validation/holdout task.
#'
#' `data` has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the `createHOT()`
#' function.
#'
#' `group` optional grouping variable, if results should be displayed
#' by different groups. Has to be column name of variables in `data`.
#'
#' `opts` is required to specify the different alternatives in the
#' validation/holdout task.
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation/holdout
#' task. Input of opts `choice` has to be column name of actual choice.
#'
#' @return a tibble
#' @importFrom dplyr select mutate group_by pick count rowwise ungroup
#' across reframe
#' @importFrom magrittr %>%
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect ends_with
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
#' # mae - without group argument defined
#' mae(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice
#' )
#'
#' # mae - with group argument defined
#' mae(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   group = Group
#' )
#'
#' @export

mae <- function(data, group, opts, choice) {
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
      # create factor
      merger = factor(
        {{ choice }},
        levels = c(seq_along(dplyr::select(data, {{ opts }}))),
        labels = c(seq_along(dplyr::select(data, {{ opts }})))
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(merger, .drop = FALSE) %>% # count number of choices
    dplyr::mutate(chosen = n / sum(n) * 100) %>% # calculate percentage
    dplyr::select(-"n") # drop variable

  # create predicted share
  WS2 <- data %>%
    # exponentiate utilities
    dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Summe = sum(dplyr::pick({{ opts }}))) %>% # create sum
    dplyr::ungroup() %>%
    dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>% # rescale
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    # aggregate
    dplyr::reframe(across({{ opts }}, ~ mean(.x),
      .names = "{.col}_mean"
    )) %>%
    # change to longer format
    tidyr::pivot_longer(.,
      cols = tidyselect::ends_with("_mean"),
      names_to = "alt", values_to = "mean"
    ) %>%
    dplyr::mutate(
      # change labeling
      alt = substr(alt, 1, (nchar(alt) - nchar("_mean"))),
      # create merger variable
      merger = rep(seq_along(dplyr::select(data, {{ opts }})),
        length.out = length(alt)
      )
    )

  mae_data <- WS2 %>%
    # merge dfs
    merge(x = ., y = WS1, by = c(WS2 %>%
      dplyr::select({{ group }}) %>%
      colnames(), "merger")) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(MAE = abs(mean - chosen)) %>% # calculate MAE
    dplyr::reframe(mae = mean(MAE))

  return(mae_data)
}
