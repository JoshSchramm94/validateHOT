#' Function to run market simulations for validation task / market scenario
#'
#' @description
#' `marksim()` runs market simulations with options in a validation
#' task or a market scenario.
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s)
#' to get `marksim()` by group(s).
#' @param opts Column names of the alternatives included in the
#' validation task or market scenario.
#' @param method Name of the market simulation method that should be used.
#' Set either to `method = "sop"` to run share of preference
#' as method or `method = "fc"` to run first choice rule. Default
#' is set to `"sop"`.
#' @param res A vector indicating whether individual results (`ind`) or
#' aggregated (`agg`) results should be returned.
#'
#' @details
#' `marksim()` provides the market shares of each alternative specified. For
#' aggregated results, `marksim()` also provides standard error plus the lower
#' and upper confidence interval. Latter is calculated according to the
#' following formula \eqn{mean +/- 1.96 x \frac{sd}{\sqrt(n)}} (Orme, 2020,
#' p. 94). `method` can either be set to `method = "sop"` to run share of
#' preference rule or to `method = "fc"` to run first choice rule to simulate
#' market shares.
#'
#' `data` a data.frame object including the alternatives shown in
#' the validation task or market scenario. Scenario can be created
#' using the `create_hot()` function.
#'
#' `group` optional grouping variable(s), if results should be displayed by
#' different groups. Has to be column name(s) of variables in `data`.
#'
#' `opts` to specify the different alternatives in the validation task or
#' market scenario. Input of `opts` has to be column names of variables in
#' `data`.
#'
#' `method` can either be set to `method = "sop"` to run share of
#' preference as method or `method = "fc"` to run first choice rule.
#' Default is set to `method = "sop"`.
#'
#' `res` specifies whether results should be aggregated across all participants
#' or across `group` (`res` needs to be set to `agg`) or if scores should be
#' converted for individuals only (`res` needs to be set to `ind`). Default is
#' set to `agg`.
#'
#' @return a tibble
#'
#' @references {
#'
#' Orme, B. K. (2020). *Getting Started with Conjoint Analysis:
#' Strategies for Product Design and Pricing Research*. 4th edition.
#' Manhattan Beach, CA: Research Publishers LLC.
#'
#' }
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
#' # marksim share of preference - without group argument defined
#' marksim(
#'   data = hot,
#'   opts = c(option_1:none),
#'   method = "sop",
#'   res = "agg"
#' )
#'
#' # marksim first choice - without group argument defined
#' marksim(
#'   data = hot,
#'   opts = c(option_1:none),
#'   method = "fc",
#'   res = "agg",
#' )
#'
#' \dontrun{
#' # marksim share of preference - with group argument defined
#' marksim(
#'   data = hot,
#'   opts = c(option_1:none),
#'   group = group
#' )
#'
#' # marksim first choice - with group argument defined
#' marksim(
#'   data = hot,
#'   opts = c(option_1:none),
#'   method = "fc",
#'   group = group
#' )
#' }
#'
#' @export

marksim <- function(data,
                    group,
                    opts,
                    method = c("sop", "fc"),
                    res = c("agg", "ind")) {
  # check for missing arguments ------------------------------------------------
  if (missing(opts)) {
    stop('Error: argument "opts" must be provided.')
  }
  # end ------------------------------------------------------------------------

  # check for opts argument ----------------------------------------------------

  # check for numeric input
  variable_numeric(data, variable = {{ opts }}, argument = opts)

  # check for length of opts (>1)
  n_opts_cols(data, opts = {{ opts }})

  # check for missings in opts
  nvar_missings(data, variables = {{ opts }})

  # end ------------------------------------------------------------------------

  # check for group argument ---------------------------------------------------

  # check for missings in group
  missing_group(data, group = {{ group }})

  # end ------------------------------------------------------------------------

  # check for method argument --------------------------------------------------

  # if missing, set to "sop"
  if (missing(method)) {
    method <- "sop"
  }

  # method can only be set to "sop" or "fc"
  allowed_input(method, c("sop", "fc"))

  # end ------------------------------------------------------------------------

  # check res argument ---------------------------------------------------------

  # specify log_base if not defined
  if (missing(res)) {
    res <- "agg"
  }

  # res can only be set to "agg" or "ind"
  allowed_input(res, c("ind", "agg"))
  # end ------------------------------------------------------------------------

  # run marksim() --------------------------------------------------------------

  # share of preference
  if (method == "sop") {
    marksim_data <- mnl(data, {{ opts }})
  }

  # first choice
  if (method == "fc") {
    marksim_data <- fc_share(data, {{ opts }})
  }

  if (res == "agg") {
    # get actual sample size for creating standard error
    n_sample <- sample_size(data, group = {{ group }})

    marksim_data <- marksim_data %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(dplyr::across({{ opts }},
        c(mw = mean, std = sd),
        .names = "{.col}___{.fn}"
      )) %>%
      # change to longer format
      tidyr::pivot_longer(
        cols = tidyselect::ends_with(c("___mw", "___std")),
        names_to = c("alternative", ".value"),
        names_sep = "___"
      ) %>%
      # get sample size
      merge(
        x = .,
        y = n_sample,
        by = c(dplyr::select(data, {{ group }}) %>%
          colnames())
      ) %>% # merge
      dplyr::mutate(
        se = std / sqrt(n), # calculate standard error
        lo.ci = mw - (1.96 * se), # lower ci
        up.ci = mw + (1.96 * se) # upper ci
      ) %>%
      # delete variables that are not reported
      dplyr::select(!tidyselect::all_of(c("std", "n")))
  }

  return(marksim_data)

  # end ------------------------------------------------------------------------
}
