#' Function to calculate the mean hit probability of a validation task
#'
#' @description
#' `mhp()` calculates the mean hit probability of a validation
#' task.
#'
#' @param data A data frame containing all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to calculate `mhp()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task.
#' @param choice Column name of the actual choice in the validation
#' task.
#'
#' @details
#' Mean hit probability calculates the averaged hit probability of
#' participants actual choices in the validation task.
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
#' Output will display both mean hit probability and its corresponding standard
#' error (both in percentage).
#'
#' @return a tibble
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
#' # mhp - without group argument defined
#' mhp(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice
#' )
#'
#' # mhp - with group argument defined
#' mhp(
#'   data = hot,
#'   opts = c(option_1:none),
#'   choice = choice,
#'   group = group
#' )
#'
#' @export
mhp <- function(data, group, opts, choice) {
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

  # check for length of opts
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

  # run mhp() --------------------------------------------------------------

  # store names of alternatives
  opts_names <- dplyr::select(data, {{ opts }}) %>% colnames()

  mhp_data <- data %>%
    mnl(variables = {{ opts }}) %>%
    # get actual choice
    dplyr::mutate(ch_share = opts_names[{{ choice }}]) %>%
    dplyr::rowwise() %>%
    # get choice probability of actual choice
    dplyr::mutate(mhp = get(ch_share)) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::reframe(
      MHP = mean(mhp),
      se = (sd(mhp) / sqrt(dplyr::n()))
    )

  return(mhp_data)
  # end ------------------------------------------------------------------------
}
