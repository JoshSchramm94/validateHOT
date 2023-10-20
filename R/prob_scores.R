#' Probability Scores for (anchored) MaxDiff
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s).
#' @param items Vector that specifies the items.
#' @param set.size A vector that specifies size of the choice set.
#' @param anchor An optional variable to specify anchor variable.
#'
#' @return a tibble
#'
#' @details
#' \code{prob_scores} converts raw utilities of a MaxDiff to probability scores.
#' Probability scores for the unanchored MaxDiff are calculated according to the formula provided by
#' Chrzan & Orme (2019, p. 56): \eqn{\frac{e^U}{(e^U + (a - 1)}}, where \emph{U} is the
#' raw utility of the item and \emph{a} is the number of items shown per choice task.
#'
#' For anchored MaxDiff the following formula is applied \eqn{\frac{e^U}{(e^U + (a - 1)} * 100 / (1 / a)}
#' (Chrzan & Orme, 2019, pp. 59-60).
#'
#' \code{data} has to be a data frame with the attributes. Items need
#' to be the raw utilities.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in \code{data}.
#'
#' \code{items} specifies the items of the MaxDiff.
#' Input for \code{items} has to be variable names.
#'
#' \code{set.size} specifies the size of the choice sets (how many items were
#' shown in one task). Input needs to be a whole number.
#'
#' \code{anchor} only needs to be specified if anchored MaxDiff is applied. Input
#' needs to be variable name or column index of anchor variable.
#'
#'
#'
#' @seealso {
#' \code{\link[=att_imp]{att_imp}} for attribute importance scores for (A)CBC
#' }
#'
#' @references {
#'
#' Chrzan, K., & Orme, B. K. (2019). \emph{Applied MaxDiff: A Practitioner’s
#' Guide to Best-Worst Scaling} Provo, UT: Sawtooth Software.
#'
#'}
#'
#' @examples
#' \dontrun{
#'
#' # probability scores for unanchored MaxDiff - without Group defined
#' prob_scores(
#'   data = MaxDiff,
#'   items = c(Option_01:Option_16),
#'   set.size = 4
#' )
#'
#' # probability scores for unanchored MaxDiff - with Group defined
#' prob_scores(
#'   data = MaxDiff,
#'   group = Group,
#'   items = c(Option_01:Option_16),
#'   set.size = 4
#' )
#'
#'
#' # probability scores for anchored MaxDiff - without Group defined
#' prob_scores(
#'   data = MaxDiff,
#'   items = c(Option_01:Option_16),
#'   set.size = 4,
#'   anchor = "none"
#' )
#'
#' # probability scores for anchored MaxDiff - with Group defined
#' prob_scores(
#'   data = MaxDiff,
#'   group = Group,
#'   items = c(Option_01:Option_16),
#'   set.size = 4,
#'   anchor = "none"
#' )
#' }
#'
#' @export
prob_scores <- function(data, group = NULL, items, set.size, anchor = NULL) {
  if (base::length(data %>% dplyr::select(., {{ items }})) < 2) {
    base::stop("Error: specify at least 2 items in 'items'!")
  }

  if (base::anyNA(data %>% dplyr::select(., {{ group }}))) {
    base::warning("Warning: 'group' contains NAs!")
  }

  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select(., {{ items }}) %>%
    base::colnames()

  ## check whether variable is numeric
  for (i in 1:base::length(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      base::stop("Error: 'items' has to be numeric!")
    }
  }

  ## check for missings
  if (base::anyNA(data %>% dplyr::select(., {{ items }}))) {
    base::stop("Error: 'items' contains NAs!")
  }

  if (!base::is.numeric(set.size)) {
    base::stop("Error: 'set.size' has to be numeric!")
  }

  if (set.size > length(data %>% dplyr::select(., {{ items }}))) {
    base::stop("Error: 'set.size' cannot be larger than number of items!")
  }

  # check anchor
  if (!(base::is.null(anchor))) {
    if (!(base::is.numeric(data[[anchor]]))) {
      base::stop("Error: 'anchor' needs to be numeric!")
    }
  }

  if (!(base::is.null(anchor))) {
    if (anyNA(data[[anchor]])) {
      base::stop("Error: 'anchor' contains NAs!")
    }
  }


  if (base::is.null(anchor)) {
    return(data %>%
      dplyr::mutate(dplyr::across({{ items }}, ~ (base::exp(.x) / (base::exp(.x) + (set.size - 1))))) %>%
      dplyr::rowwise() %>%
      dplyr::mutate(Summe = base::sum(dplyr::pick({{ items }}))) %>% # sum up
      dplyr::ungroup() %>%
      dplyr::mutate(dplyr::across({{ items }}, ~ .x / Summe * 100)) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::summarise(dplyr::across({{ items }},
        c(mw = base::mean, std = stats::sd),
        .names = "{.col}.{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c(".mw", ".std")),
        names_to = c("Option", ".value"), names_sep = "\\."
      ))
  }

  if (!base::is.null(anchor)) {
    return(
      data %>%
        dplyr::mutate(across(c({{ items }}, anchor), ~ (base::exp(.x) / (base::exp(.x) + (set.size - 1))) * 100 / (1 / set.size))) %>%
        dplyr::group_by(dplyr::pick({{ group }})) %>%
        dplyr::summarise(dplyr::across(c({{ items }}, anchor),
          c(mw = base::mean, std = stats::sd),
          .names = "{.col}.{.fn}"
        )) %>%
        tidyr::pivot_longer(.,
          cols = tidyselect::ends_with(c(".mw", ".std")),
          names_to = c("Option", ".value"), names_sep = "\\."
        )
    )
  }
}