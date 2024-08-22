#' Function to calculate mean absolute error of validation/holdout task
#'
#' @description \code{mae} measures the mean absolute error of a
#' validation/holdout task, i.e., aggregated deviation between predicted and
#' stated share of alternatives in the validation/holdout task.
#'
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get \code{mae} by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout task.
#'
#' @details
#' Mean absolute error (MAE) calculates the deviation between predicted and
#' stated (actual) choice share. It is an aggregated value across all
#' alternatives in the validation/holdout task.
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()}
#' function.
#'
#' \code{group} optional grouping variable, if results should be displayed
#' by different groups. Has to be column name of variables in \code{data}.
#'
#' \code{opts} is required to specify the different alternatives in the
#' validation/holdout task.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice in the validation/holdout task.
#' Input of opts \code{choice} has to be column name of actual choice.
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
  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
  }

  # grouping variable
  ## check for missings
  if (base::anyNA(data %>% dplyr::select(., {{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }

  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select(., {{ opts }}) %>%
    base::colnames()

  ## check whether variable is numeric
  for (i in base::seq_along(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'opts' has to be numeric!")
    }
  }

  ## check for missings
  if (anyNA(data %>% dplyr::select(., {{ opts }}))) {
    stop("Error: 'opts' contains NAs!")
  }

  # choice
  ## check for missing
  if (base::anyNA(data %>% dplyr::select(., {{ choice }}))) {
    stop("Error: 'choice' contains NAs!")
  }

  ## check for str
  choi <- data %>%
    dplyr::select(., {{ choice }}) %>%
    base::colnames()

  if (!base::is.numeric(data[[choi]])) {
    stop("Error: 'choice' has to be numeric!")
  }

  # create actual share of actual choice
  base::suppressMessages(WS1 <- data %>%
    dplyr::mutate(
      # create factor
      merger = base::factor(
        {{ choice }},
        levels = c(base::seq_along(dplyr::select(., {{ opts }}))),
        labels = c(base::seq_along(dplyr::select(., {{ opts }})))
      )
    ) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count(merger, .drop = F) %>% # count number of choices
    dplyr::mutate(chosen = n / base::sum(n) * 100) %>% # calculate percentage
    dplyr::select(-"n")) # drop variable

  # create predicted share
  base::suppressMessages(WS2 <- data %>%
    # exponentiate utilities
    dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(Summe = base::sum(dplyr::pick({{ opts }}))) %>% # create sum
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
      alt = base::substr(alt, 1, (base::nchar(alt) - base::nchar("_mean"))),
      # create merger variable
      merger = base::rep(base::seq_along(dplyr::select(data, {{ opts }})),
        length.out = base::length(alt)
      )
    ))

  return(suppressMessages(WS2 %>%
    # merge dfs
    base::merge(x = ., y = WS1, by = c(WS2 %>%
      dplyr::select(., {{ group }}) %>%
      base::colnames(), "merger")) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::mutate(MAE = base::abs(mean - chosen)) %>% # calculate MAE
    dplyr::reframe(mae = base::mean(MAE))))
}
