#' Function to calculate probability scores for (anchored) MaxDiff
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s).
#' @param items Vector that specifies the items.
#' @param set.size A vector that specifies size of the choice set.
#' @param res A vector indicating whether individual shares (`ind`) or
#' aggregated (`agg`) shares should be returned.
#' @param anchor An optional variable to specify anchor variable.
#'
#'
#' @return a tibble
#'
#' @details
#' `prob_scores()` converts raw utilities of a MaxDiff to probability scores.
#' Probability scores for the unanchored MaxDiff are calculated according to the
#' formula provided by Chrzan & Orme (2019, p. 56):
#' \eqn{\frac{e^U}{(e^U + (a - 1)}}, where \emph{U} is the raw utility of the
#' item and \emph{a} is the number of items shown per choice task.
#'
#' For anchored MaxDiff the following formula is applied
#' \eqn{\frac{e^U}{(e^U + (a - 1)} * 100 / (1 / a)} (Chrzan & Orme, 2019,
#' pp. 59-60).
#'
#' `data` has to be a data frame with the attributes. Items have
#' to be the raw utilities.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `items` specifies the items of the MaxDiff.
#' Input for `items` has to be variable names.
#'
#' `set.size` specifies the size of the choice sets (how many items were
#' shown in one task). Input needs to be a whole number.
#'
#' `res` specifies whether results should be aggregated across all
#' participants or across `group` (`res` needs to be set to
#' `agg`) or if scores should be converted for individuals only.
#'
#' `anchor` only needs to be specified if anchored MaxDiff is applied.
#' Input for `anchor` has to be variable names.
#'
#' @importFrom dplyr select across pick group_by ungroup rowwise reframe
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect ends_with
#'
#' @seealso {
#' \code{\link[=zero_anchored]{zero_anchored}} for zero-anchored interval
#' scores for MaxDiff
#' \code{\link[=att_imp]{att_imp}} for attribute importance scores for (A)CBC
#' \code{\link[=zc_diffs]{zc_diffs}} for zero-centered diff scores for (A)CBC
#' }
#'
#' @references {
#'
#' Chrzan, K., & Orme, B. K. (2019). \emph{Applied MaxDiff: A Practitioner’s
#' Guide to Best-Worst Scaling} Provo, UT: Sawtooth Software.
#'
#' }
#'
#' @examples
#'
#' library(validateHOT)
#'
#' # probability scores for unanchored MaxDiff - without group argument defined
#' prob_scores(
#'   data = MaxDiff,
#'   items = c(Option_01:Option_16),
#'   set.size = 4,
#'   res = "agg"
#' )
#'
#' # probability scores for unanchored MaxDiff - with group argument defined
#' \dontrun{
#' prob_scores(
#'   data = MaxDiff,
#'   group = Group,
#'   items = c(Option_01:Option_16),
#'   set.size = 4,
#'   res = "agg"
#' )
#' }
#'
#' # probability scores for anchored MaxDiff - without group argument defined
#' prob_scores(
#'   data = MaxDiff,
#'   items = c(Option_01:none),
#'   set.size = 4,
#'   anchor = none,
#'   res = "agg"
#' )
#' # probability scores for anchored MaxDiff - with group argument defined
#' \dontrun{
#' prob_scores(
#'   data = MaxDiff,
#'   group = Group,
#'   items = c(Option_01:none),
#'   set.size = 4,
#'   anchor = none,
#'   res = "agg"
#' )
#' }
#'
#' @export
prob_scores <- function(data, group = NULL, items, set.size,
                        res = c("agg", "ind"), anchor = NULL) {
  if (length(data %>% dplyr::select({{ items }})) < 2) {
    stop("Error: specify at least 2 items in 'items'!")
  }

  if (anyNA(data %>% dplyr::select({{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }

  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select({{ items }}) %>%
    colnames()

  ## check whether variable is numeric
  for (i in seq_along(alternatives)) {
    if (!is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'items' has to be numeric!")
    }
  }

  ## check for missings
  if (anyNA(data %>% dplyr::select({{ items }}))) {
    stop("Error: 'items' contains NAs!")
  }

  if (!is.numeric(set.size)) {
    stop("Error: 'set.size' has to be numeric!")
  }

  if (set.size > length(data %>% dplyr::select({{ items }}))) {
    stop("Error: 'set.size' cannot be larger than number of items!")
  }


  # test whether res is specified
  if (missing(res)) {
    stop("Error: 'res' is not defined!")
  }

  # test whether res is correctly specified
  if ((res != "agg") && (res != "ind")) {
    stop(
      "Error: 'res' can only be set to 'agg' or 'ind'!"
    )
  }

  # can not specify res to 'ind' and specify group
  if ((res == "ind") && !missing(group)) {
    stop("Error: Can not speficy 'group' if 'res' is set to 'ind'!")
  }

  # test length of anchor
  if (!missing(anchor)) {
    anc <- data %>%
      dplyr::select({{ anchor }}) %>%
      colnames(.)

    if (length(anc) > 1) {
      stop("Error: 'anchor' can only be one variable!")
    }
  }

  if (!missing(anchor)) {
    if (!(data %>% dplyr::select({{ anchor }}) %>% colnames()) %in%
      (data %>% dplyr::select({{ items }}) %>% colnames())) {
      stop("Error: 'anchor' has to be part of 'items'!")
    }
  }

  #######################################################
  if (missing(anchor)) {
    if (res == "agg") {
      prob_scores_data <- data %>%
        dplyr::mutate(dplyr::across(
          {{ items }},
          ~ (exp(.x) /
            (exp(.x) + (set.size - 1)))
        )) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Summe = sum(dplyr::pick({{ items }}))) %>% # sum up
        dplyr::ungroup() %>%
        dplyr::mutate(dplyr::across({{ items }}, ~ .x / Summe * 100)) %>%
        dplyr::select(-Summe) %>%
        dplyr::group_by(dplyr::pick({{ group }})) %>%
        dplyr::reframe(dplyr::across({{ items }},
          c(mw = mean, std = stats::sd),
          .names = "{.col}....{.fn}"
        )) %>%
        tidyr::pivot_longer(.,
          cols = tidyselect::ends_with(c("....mw", "....std")),
          names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
        )

      return(prob_scores_data)
    }


    if (res == "ind") {
      prob_scores_data <- data %>%
        dplyr::mutate(dplyr::across({{ items }}, ~
          (exp(.x) /
            (exp(.x) + (set.size - 1))))) %>%
        dplyr::rowwise() %>%
        dplyr::mutate(Summe = sum(dplyr::pick({{ items }}))) %>% # sum up
        dplyr::ungroup() %>%
        dplyr::mutate(dplyr::across({{ items }}, ~ .x / Summe * 100)) %>%
        dplyr::select(-Summe)

        return(prob_scores_data)
    }
  }

  if (!missing(anchor)) {
    if (res == "agg") {
      prob_scores_data <- data %>%
          dplyr::mutate(dplyr::across({{ items }}, ~
            (exp(.x) /
              (exp(.x) +
                (set.size - 1))) * 100 /
              (1 / set.size))) %>%
          dplyr::group_by(dplyr::pick({{ group }})) %>%
          dplyr::reframe(dplyr::across({{ items }},
            c(mw = mean, std = stats::sd),
            .names = "{.col}....{.fn}"
          )) %>%
          tidyr::pivot_longer(.,
            cols = tidyselect::ends_with(c("....mw", "....std")),
            names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
          )

        return(prob_scores_data)
    }

    if (res == "ind") {
      prob_scores_data <- data %>%
          dplyr::mutate(dplyr::across(
            {{ items }},
            ~ (exp(.x) /
              (exp(.x) +
                (set.size - 1))) * 100 /
              (1 / set.size)
          ))
      return(prob_scores_data)
    }
  }
}
