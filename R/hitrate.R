#' Function to calculate hit rate of validation/holdout task
#'
#' @description \code{hitrate} measures number of times a choice was correctly
#' predicted in a validation/holdout task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get \code{hitrate} by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout
#' task.
#'
#' @details
#' \code{hitrate} measures number of times a participant's choice was correctly
#' predicted by the model.
#' Output contains the following 5 metrics:
#' \itemize{
#' \item \code{HR} hit rate (number of correctly predicted
#' choices) in percentages
#' \item \code{se} standard error in percentages
#' \item \code{chance} chance level of correctly predicted choices by just
#' guessing
#' \item \code{cor} absolute number of correctly predicted choices
#' \item \code{n} total number of choices
#' }
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()}
#' function.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in \code{data}.
#'
#' \code{opts} is required to specify the different alternatives in the
#' validation/holdout task.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice in the validation/holdout
#' task. Input of opts \code{choice} has to be column name of actual choice.
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


  suppressMessages(return(data %>%
    # store column index with highest utility
    dplyr::mutate(pred = base::max.col(dplyr::pick({{ opts }}))) %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      # calculate the hit rate
      HR = base::mean(as.integer({{ choice }} == pred)) * 100,
      # calculate se
      se = (stats::sd(as.integer({{ choice }} == pred)) /
              base::sqrt(dplyr::n())) * 100,
      # calculate the chance level
      chance = 1 / base::length(dplyr::select(data, {{ opts }})) * 100,
      # calculate number of correct predicted
      cor = base::sum(base::as.integer({{ choice }} == pred)),
      n = dplyr::n() # n
    )))
}
