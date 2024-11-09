#' Function to run market simulations for validation task / market scenario
#'
#' @description
#' `marksim` runs market simulations with options in a validation/holdout
#' task or a market scenario.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `marksim()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation/holdout task or market scenario.
#' @param method Name of the market simulation method that should be conducted.
#' Set either to `method = "sop"` to run share of preference
#' as method or `method = "fc"` to run first choice rule. Default
#' is `"sop"`.
#'
#' @details
#' `marksim()` provides the expected aggregated market shares of each
#' alternative specified as well as its standard error,
#' and the lower and upper confidence interval. Latter is calculated according
#' to the following formula \eqn{mean +/- 1.96 x \frac{sd}{\sqrt(n)}}
#' (Orme, 2020, p. 94). `method` can either be set to `method = "sop"`
#' to run share of preference rule or to `method = "fc"` to run
#' first choice rule to simulate market shares.
#'
#' `data` has to be a data frame including the alternatives shown in
#' the validation/holdout task or market scenario. Scenario can be created
#' using the `createHOT()` function.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `opts` is required to specify the different alternatives in the
#' simulation task. Input of `opts` has to be column names of variables
#' in `data`.
#'
#' `method` can either be set to `method = "sop"` to run share of
#' preference as method or `method = "fc"` to run first choice rule.
#' Default set to `method = "sop"`.
#'
#' @return a tibble
#' @importFrom dplyr select pick mutate across ungroup group_by reframe count
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom tibble as_tibble
#' @importFrom fastDummies dummy_cols
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
#' # marksim share of preference - without group argument defined
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   method = "sop"
#' )
#'
#' # marksim first choice - without group argument defined
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   method = "fc"
#' )
#'
#' \dontrun{
#' # marksim share of preference - with group argument defined
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   group = Group
#' )
#'
#' # marksim first choice - with group argument defined
#' marksim(
#'   data = HOT,
#'   opts = c(Option_1:None),
#'   group = Group
#' )
#' }
#'
#' @export

marksim <- function(data, group, opts,
                    method = c("sop", "fc")) {
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

  # approach needs to be specified
  if (missing(method)) {
    method <- "sop"
  }

  # method can only be 'sop' or 'fc'
  if ((method != "sop") && (method != "fc")) {
    stop(
      "Error: 'method' is wrong, please choose between",
      " 'sop' and 'fc'!"
    )
  }



  # store the number of persons in each group for creating the standard error
  WS1 <- data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count()

  if (method == "sop") {
    marksim_data <- data %>%
      # exponentiate all alternatives
      dplyr::mutate(dplyr::across({{ opts }}, ~ exp(.x))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Summe = sum(dplyr::pick({{ opts }}))) %>% # sum up
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across({{ opts }}, ~ .x / Summe * 100)) %>% # scale
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      # calculate mean and sd
      dplyr::reframe(dplyr::across({{ opts }},
        c(mw = mean, std = stats::sd),
        .names = "{.col}....{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c("....mw", "....std")),
        names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
      ) %>% # change to longer format
      merge(
        x = .,
        y = WS1,
        by = c(data %>% dplyr::select({{ group }}) %>%
          colnames())
      ) %>% # merge
      dplyr::mutate(
        se = std / sqrt(n), # calculate standard error
        lo.ci = mw - (1.96 * se), # lower ci
        up.ci = mw + (1.96 * se) # upper ci
      ) %>%
      # delete irrelevant variables
      dplyr::select(!tidyselect::all_of(c("std", "n"))) %>%
      tibble::as_tibble()

    return(marksim_data)
  }

  if (method == "fc") {
    marksim_data <- data %>%
      dplyr::mutate(pred = max.col(dplyr::across({{ opts }}))) %>%
      mutate(pred = factor(pred,
        levels = c(seq_along(dplyr::select(data, {{ opts }}))),
        labels = c(data %>% dplyr::select({{ opts }}) %>%
          colnames())
      )) %>%
      dplyr::select({{ group }}, pred) %>%
      fastDummies::dummy_cols(.,
        select_columns = "pred",
        remove_selected_columns = TRUE,
        omit_colname_prefix = TRUE
      ) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::mutate(dplyr::across(
        {{ opts }},
        ~ .x * 100
      )) %>%
      dplyr::reframe(dplyr::across({{ opts }},
        c(mw = mean, std = stats::sd),
        .names = "{.col}....{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c("....mw", "....std")),
        names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
      ) %>% # change to longer format
      merge(
        x = .,
        y = WS1,
        by = c(data %>% dplyr::select({{ group }}) %>%
          colnames())
      ) %>% # merge
      dplyr::mutate(
        se = std / sqrt(n), # calculate standard error
        lo.ci = mw - (1.96 * se), # lower ci
        up.ci = mw + (1.96 * se) # upper ci
      ) %>%
      # delete irrelevant variables
      dplyr::select(!tidyselect::all_of(c("std", "n"))) %>%
      tibble::as_tibble()

    return(marksim_data)
  }
}
