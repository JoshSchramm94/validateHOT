#' Function to calculate hit rate of validation/holdout task
#'
#' @description `hitrate()` measures number of times a choice was correctly
#' predicted in a validation/holdout task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `hitrate()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout
#' task.
#'
#' @details
#' `hitrate()` measures number of times a participant's choice was correctly
#' predicted by the model.
#' Output contains the following 5 metrics:
#' \itemize{
#' \item `HR` hit rate (number of correctly predicted
#' choices) in percentages
#' \item `se` standard error in percentages
#' \item `chance` chance level of correctly predicted choices by just
#' guessing
#' \item `cor` absolute number of correctly predicted choices
#' \item `n` total number of choices
#' }
#'
#' `data` has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the `createHOT()`
#' function.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `opts` is required to specify the different alternatives in the
#' validation/holdout task.
#' Input of `opts` has to be column names of variables in `data`.
#'
#' `choice` to specify column of actual choice in the validation/holdout
#' task. Input of opts `choice` has to be column name of actual choice.
#'
#' @return a tibble
#' @importFrom dplyr select mutate pick group_by reframe n
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
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
#' # hit rate - without group argument defined
#' hitrate(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice
#' )
#'
#' # hit rate - with group argument defined
#' hitrate(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   group = Group
#' )
#'
#' @export

hitrate <- function(data, group, opts, choice) {
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


  hitrate_data <- data %>%
    # store column index with highest utility
    dplyr::mutate(pred = max.col(dplyr::pick({{ opts }}))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      # calculate the hit rate
      HR = mean(as.integer({{ choice }} == pred)) * 100,
      # calculate se
      se = (stats::sd(as.integer({{ choice }} == pred)) /
        sqrt(dplyr::n())) * 100,
      # calculate the chance level
      chance = 1 / length(dplyr::select(data, {{ opts }})) * 100,
      # calculate number of correct predicted
      cor = sum(as.integer({{ choice }} == pred)),
      n = dplyr::n() # n
    )

  return(hitrate_data)
}
