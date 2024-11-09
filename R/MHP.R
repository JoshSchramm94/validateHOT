#' Function to calculate Mean Hit Probability (MHP) of validation/holdout task
#'
#' @description
#' `mhp` measures the mean hit probability of a validation/holdout
#' task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `mhp` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param choice Column name of the actual choice in the validation/holdout
#' task.
#'
#' @details
#' Mean hit probability (MHP) measures the averaged hit probability of
#' participants actual choices in the validation/holdout task.
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
#' task.
#' Input of opts `choice` has to be column name of actual choice.
#'
#' Output will display both mean hit probability and its corresponding standard
#' error (both in percentage).
#'
#' @return a tibble
#' @importFrom dplyr select relocate mutate rowwise pick across ungroup
#' group_by reframe
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
#'
#' # mhp - without group argument defined
#' mhp(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice
#' )
#'
#' # mhp - with group argument defined
#' mhp(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   choice = choice,
#'   group = Group
#' )
#'
#' @export
mhp <- function(data, group, opts, choice) {
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

  rm(choi)

  #############################################################################
  # change data structure
  data <- data %>%
    # reorder columns
    dplyr::relocate(c({{ opts }}, {{ choice }}, {{ group }})) %>%
    # exponentiate
    dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
    dplyr::rowwise() %>%
    # calculate sum
    dplyr::mutate(Summe = sum(dplyr::pick({{ opts }}))) %>%
    dplyr::ungroup() %>%
    # choice probability in percentage
    dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>%
    # create mhp variable
    dplyr::mutate(mhp = 0)


  # store actual choice
  choi <- data %>%
    dplyr::select({{ choice }}) %>%
    unlist() %>%
    unname()

  # assign correct hit probability
  for (j in seq_len(nrow(data))) {
    data$mhp[j] <- unlist(data[j, choi[j]])
  }

  # calculate MHP
  mhp_data <- data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      MHP = mean(mhp),
      se = (stats::sd(mhp) / sqrt(dplyr::n()))
    )

  return(mhp_data)
}
