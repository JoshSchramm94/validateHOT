#' Market Simulation of Options included in HOT
#'
#' @description
#' Function to run market simulations with options in a validation/holdout task.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get \code{"marksim"} by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task.
#' @param method Name of the market simulation method that should be conducted.
#' Either needs to be \code{method = "shareofpref"} to run share of preference
#' as method or \code{method = "firstchoice"} to run first choice rule. Default
#' set to \code{"shareofpref"}.
#'
#' @return a tibble
#' @importFrom dplyr select pick mutate across ungroup group_by summarise count
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom tibble as_tibble
#' @importFrom fastDummies dummy_cols
#'
#' @details
#' Market simulation provides the expected aggregated market shares of each
#'  alternative in the validation/holdout task as well as its standard error
#'  and the lower and upper confidence interval which is calculated according
#'  to the following formula \eqn{mean +/- 1.96 x \frac{sd}{\sqrt(n)}}
#'  (Orme, 2020, p. 94). \code{method} can either be set to \code{method = "shareofpref"}
#'  to run share of preference rule or to \code{method = "firstchoice"} to run
#'  first choice rule to simulate market shares.
#'
#' \code{data} has to be a data frame including the alternatives shown in
#' the validation/holdout task. Can be created using the \code{createHOT()}
#' function.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in \code{data}.
#'
#' \code{opts} is needed to specify the different alternatives in the simulation
#' task.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{method} can either be set to \code{method = "shareofpref"} to run share of preference
#' as method or \code{method = "firstchoice"} to run first choice rule. Default
#' set to \code{method = "shareofpref"}.
#'
#' @references {
#'
#' Orme, B. K. (2020). \emph{Getting Started with Conjoint Analysis:
#' Strategies for Product Design and Pricing Research}. 4th edition.
#' Manhattan Beach, CA: Research Publishers LLC.
#'
#' }
#'
#' @examples
#' \dontrun{
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   none = 19,
#'   prod = 7,
#'   prod.levels = list(3, 10, 11, 15, 16, 17, 18),
#'   method = "MaxDiff",
#'   choice = 20,
#'   varskeep = 21
#' )
#'
#' # marksim ungrouped share of preference
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   method = "shareofpref"
#' )
#'
#' # marksim ungrouped first choice
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   method = "firstchoice"
#' )
#'
#' # marksim grouped share of preference
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   group = Group
#' )
#'
#' # marksim grouped first choice
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   group = Group
#' )
#' }
#'
#' @export

marksim <- function(data, group, opts,
                    method = c("shareofpref", "firstchoice")) {
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
  for (i in 1:base::length(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'opts' has to be numeric!")
    }
  }

  ## check for missings
  if (anyNA(data %>% dplyr::select(., {{ opts }}))) {
    stop("Error: 'opts' contains NAs!")
  }

  # approach needs to be specified
  if (base::missing(method)) {
    method <- "shareofpref"
  }

  # method can only be 'shareofpref' or 'firstchoice'
  if ((method != "shareofpref") & (method != "firstchoice")) {
    base::stop(
      "Error: 'method' is wrong, please choose between",
      " 'shareofpref' and 'firstchoice'!"
    )
  }



  # store the number of persons in each group for creating the standard error
  WS1 <- data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count()

  if (method == "shareofpref") {
    return(data %>%
      # exponentiate all alternatives
      dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Summe = base::sum(dplyr::pick({{ opts }}))) %>% # sum up
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>% # scale
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      # calculate mean and sd
      dplyr::summarise(dplyr::across({{ opts }},
        c(mw = base::mean, std = stats::sd),
        .names = "{.col}.{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c(".mw", ".std")),
        names_to = c("Option", ".value"), names_sep = "\\."
      ) %>% # change to longer format
      base::merge(
        x = .,
        y = WS1,
        by = c(data %>% dplyr::select(., {{ group }}) %>%
          base::colnames())
      ) %>% # merge
      dplyr::mutate(
        se = std / sqrt(n), # calculate standard error
        lo.ci = mw - (1.96 * se), # lower ci
        up.ci = mw + (1.96 * se) # upper ci
      ) %>%
      # delete irrelevant variables
      dplyr::select(!tidyselect::all_of(c("std", "n"))) %>%
      tibble::as_tibble())
  }

  if (method == "firstchoice") {
    return(data %>%
      dplyr::mutate(pred = base::max.col(dplyr::across({{ opts }}))) %>%
      mutate(pred = factor(pred,
        levels = c(1:base::length(dplyr::select(., {{ opts }}))),
        labels = c(data %>% dplyr::select({{ opts }}) %>%
          base::colnames())
      ))
      %>%
      dplyr::select({{ group }}, pred) %>%
      fastDummies::dummy_cols(.,
        select_columns = "pred",
        remove_selected_columns = T,
        omit_colname_prefix = T
      )
      %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::mutate(dplyr::across(
        {{ opts }},
        ~ .x * 100
      )) %>%
      dplyr::summarise(dplyr::across({{ opts }},
        c(mw = base::mean, std = stats::sd),
        .names = "{.col}.{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c(".mw", ".std")),
        names_to = c("Option", ".value"), names_sep = "\\."
      ) %>% # change to longer format
      base::merge(
        x = .,
        y = WS1,
        by = c(data %>% dplyr::select(., {{ group }}) %>%
          base::colnames())
      ) %>% # merge
      dplyr::mutate(
        se = std / sqrt(n), # calculate standard error
        lo.ci = mw - (1.96 * se), # lower ci
        up.ci = mw + (1.96 * se) # upper ci
      ) %>%
      # delete irrelevant variables
      dplyr::select(!tidyselect::all_of(c("std", "n"))) %>%
      tibble::as_tibble())
  }
}