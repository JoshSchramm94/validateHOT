#' Function to calculate F1-Score of confusion matrix for general product demand
#'
#' @description F1-Score is one of the 5 metrics of the confusion matrix
#' and is defined as \eqn{\frac{2 * precision * recall}{precision + recall}} or
#' stated differently by Burger (2018) \eqn{\frac{2TP}{2TP + FP + FN}},
#' where TP = True Positives, FP = False Positives, and FN = False Negatives.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get \code{f1} by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout task.
#' @param none Column name of none alternative.
#'
#' @details
#' The current logic of \code{f1} is to determine whether a binary coded choice
#' is correctly predicted by the model. To use the function, the validation/holdout task
#' must include a \code{none} alternative.
#' One possible application is, for example, whether a buy or a no-buy choice
#' has been correctly predicted. For example, suppose you have three alternatives plus
#' a \code{none} alternative and want to check whether a buy or no-buy was
#' correctly predicted. This function can be useful if you, for example,
#' test whether your model significantly overestimates or underestimates
#' a purchase likelihood.
#'
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()}
#' function.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups Has to be column name of variables in \code{data}.
#'
#' \code{opts} is required to specify the different alternatives in the
#' validation/holdout task (also includes the \code{none} alternative).
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{choice} to specify column of actual choice in the validation/holdout task.
#' Input of \code{choice} has to be column name of actual choice.
#'
#' \code{none} is required to specify column name of the \code{none} alternative in the
#' validation/holdout task.
#'
#' Please be aware about the following 2x2 table regarding coding of buy and
#' no-buy choice:
#'
#' \tabular{crcc}{
#'    \tab \tab  Predicted           \tab    \cr
#' Observed \tab \tab  Buy \tab No-Buy  \cr
#'  \tab Buy \tab A \tab B  \cr
#'  \tab No-Buy \tab C \tab D  \cr
#' }
#'
#' @importFrom dplyr group_by reframe select pick
#' @importFrom magrittr "%>%"
#'
#' @return a tibble
#'
#' @seealso {
#' \code{\link[=accuracy]{accuracy}}
#' \code{\link[=precision]{precision}}
#' \code{\link[=recall]{recall}}
#' \code{\link[=specificity]{specificity}}
#' }
#'
#'
#' @references {
#'
#' Burger, S. V. (2018). \emph{Introduction to Machine Learning with R:
#' Rigorous Mathematical Analysis}. O'Reilly.
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
#'   choice = 20, varskeep = 21
#' )
#'
#' # f1 - without group argument defined
#' f1(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   none = None
#' )
#'
#' # f1 - with group argument defined
#' f1(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   none = None,
#'   group = Group
#' )
#'
#' @export


f1 <- function(data, group, opts, choice, none) {
  # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    stop("Error: argument 'none' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 1) {
    stop("Error: specify at least 2 alternatives in 'opts'!")
  }

  if (!(data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
    (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' has to be part of 'opts'!")
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

  # test length of none
  if (!base::missing(none)) {
    anc <- data %>%
      dplyr::select(., {{ none }}) %>%
      base::colnames(.)
    if (length(anc) > 1) {
      base::stop("Error: 'none' can only be one variable!")
    }
  }

  return(data %>%
    dplyr::mutate(
      # store column index with highest utility
      pred = base::max.col(dplyr::pick({{ opts }})),
      buy = base::ifelse({{ choice }} != base::match(
        data %>% dplyr::select(., {{ none }}) %>% colnames(),
        data %>% dplyr::select(., {{ opts }}) %>% colnames()
      ), 1, 2), # dichotomies actual choice (1 = prod, 2 = none)
      pred = base::ifelse(pred != base::match(
        data %>% dplyr::select(., {{ none }}) %>% colnames(),
        data %>% dplyr::select(., {{ opts }}) %>% colnames()
      ), 1, 2)
    ) %>% # dichotomies pred choice (1 = prod, 2 = none)
    dplyr::group_by(pick({{ group }})) %>%
    dplyr::reframe(
      f1 = 100 * ((2 * (base::sum(buy == 1 & pred == 1))) /
        (((2 * (base::sum(buy == 1 & pred == 1))) +
          base::sum(buy == 2 & pred == 1) +
          base::sum(buy == 1 & pred == 2)))
      )
    ))
}
