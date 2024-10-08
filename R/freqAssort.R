#' Function to calculate averaged number of times a participant is reached by assortment
#'
#' @description
#' Frequency function of T(otal) U(nduplicated) R(each) and F(requency)
#' analysis to measure the average time a consumer
#' is reached with a particular product bundle assortment.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get \code{freqassort} by group(s).
#' @param opts Column names of the alternatives included in the assortment.
#' @param none Column name of none / threshold alternative.
#'
#' @details
#' Frequency calculates the average times a consumer would be reached with the
#' tested product assortment. The current logic of \code{freqassort}
#' is that the utility of an alternative has to exceed a threshold. In the case
#' of \code{freqassort} this threshold is referred to the \code{none} argument
#' in \code{data}.
#' The frequency is calculated based on the 'threshold' approach, i.e.,
#' each alternative that exceeds utility of \code{none} alternative is
#' considered as, for example, purchase option.
#'
#' \code{data} has to be a data frame including the alternatives that should
#' be tested.
#'
#' \code{group} optional grouping variable, if results should be displayed
#' by different groups. Has to be column name of variables in \code{data}.
#'
#' \code{opts} defines product assortment that should be considered.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{none} to specify column name of the \code{none} alternative (i.e.,
#' threshold variable).
#'
#'
#' @importFrom dplyr select mutate across rowwise c_across pick reframe
#' group_by ungroup
#' @importFrom magrittr "%>%"
#'
#' @return a tibble
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
#' # freqassort - without group argument defined
#' freqassort(
#'   data = HOT,
#'   opts = c(Option_1, Option_2, Option_6),
#'   none = None
#' )
#'
#' # freqassort - with group argument defined
#' freqassort(
#'   data = HOT,
#'   opts = c(Option_1, Option_2, Option_6),
#'   none = None,
#'   group = Group
#' )
#'
#' @export
freqassort <- function(data, group, none, opts) {
  # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    stop("Error: argument 'none' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    stop("Error: argument 'opts' is missing!")
  }

  # grouping variable
  ## store names of grouping variables
  groups <- data %>%
    dplyr::select(., {{ group }}) %>%
    base::colnames()

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

  # None
  ## check for missing
  if (base::anyNA(data %>% dplyr::select(., {{ none }}))) {
    stop("Error: 'none' contains NAs!")
  }

  ## check for str
  Noo <- data %>%
    dplyr::select(., {{ none }}) %>%
    base::colnames()

  if (!base::is.numeric(data[[Noo]])) {
    stop("Error: 'none' has to be numeric!")
  }

  ## check none can not be part of opts
  if ((data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
    (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' can not be part of 'opts'!")
  }

  return(data %>%
    dplyr::select(., {{ opts }}, {{ none }}, {{ group }}) %>%
    dplyr::mutate(
      thres = {{ none }}, # store threshold utility
      # recode opts depending whether it is higher (1) or lower (0)
      # than the threshold
      dplyr::across({{ opts }}, ~ base::ifelse(.x > thres, 1, 0))
    ) %>%
    dplyr::rowwise() %>%
    # # sum the number of options rowwise
    dplyr::mutate(freq = base::sum(dplyr::c_across({{ opts }}))) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(freq = base::mean(freq)))
}
