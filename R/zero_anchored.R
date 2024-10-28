#' Function to calculate zero-anchored interval scores for (anchored) MaxDiff
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s).
#' @param items Vector that specifies the items.
#' @param res A vector indicating whether individual shares (\code{ind}) or
#' aggregated (\code{agg}) shares should be returned.
#' @param anchor An optional variable to specify anchor variable.
#'
#'
#' @details
#' \code{zero_anchored} converts raw utilities of a MaxDiff to zero-anchored
#' interval scores that have a range of 100.
#'
#' For anchored MaxDiff the anchor is set to 0. More information can be
#' obtained here: https://sawtoothsoftware.com/help/lighthouse-studio/manual/analysis-manager-maxdiff-export-settings.html
#'
#' \code{data} has to be a data frame with the attributes. Items have
#' to be the raw utilities.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in \code{data}.
#'
#' \code{items} specifies the items of the MaxDiff.
#' Input for \code{items} has to be variable names.
#'
#' \code{res} specifies whether results should be aggregated across all
#' participants or across \code{group} (\code{res} needs to be set to
#' \code{agg}) or if scores should be converted for individuals only.
#'
#' \code{anchor} only needs to be specified if anchored MaxDiff is applied.
#' Input for \code{anchor} has to be variable name.
#'
#' @importFrom dplyr select across reframe group_by pick
#' @importFrom magrittr "%>%"
#' @importFrom tidyselect all_of ends_with
#' @importFrom tidyr pivot_longer
#' @importFrom scales rescale
#' @importFrom tibble as_tibble is_tibble
#'
#' @return a tibble
#'
#'
#' @seealso {
#' \code{\link[=att_imp]{att_imp}} for attribute importance scores for (A)CBC
#' \code{\link[=prob_scores]{prob_scores}} for probability scores for MaxDiff
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
#' # zero-anchored interval scores for unanchored MaxDiff - without group
#' # argument defined
#' zero_anchored(
#'   data = MaxDiff,
#'   items = c(Option_01:Option_16),
#'   res = "agg"
#' )
#' \dontrun{
#' # zero-anchored interval scores for unanchored MaxDiff - with group defined
#' zero_anchored(
#'   data = MaxDiff,
#'   group = Group,
#'   items = c(Option_01:Option_16),
#'   res = "agg"
#' )
#' }
#'
#' # zero-anchored interval scores for anchored MaxDiff - without group defined
#' zero_anchored(
#'   data = MaxDiff,
#'   items = c(Option_01:none),
#'   anchor = none,
#'   res = "agg"
#' )
#' \dontrun{
#' # zero-anchored interval scores for anchored MaxDiff - with group defined
#' zero_anchored(
#'   data = MaxDiff,
#'   group = Group,
#'   items = c(Option_01:none),
#'   anchor = none,
#'   res = "agg"
#' )
#' }
#'
#' @export
zero_anchored <- function(data, group = NULL, items,
                          res = c("agg", "ind"), anchor = NULL) {
  if (base::missing(items)) {
    stop("Error: 'items' is missing!")
  }

  if (isTRUE(tibble::is_tibble(data))) {
    stop("Error: 'data' has to be a data frame!")
  }

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
  for (i in base::seq_along(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      base::stop("Error: 'items' has to be numeric!")
    }
  }

  ## check for missings
  if (base::anyNA(data %>% dplyr::select(., {{ items }}))) {
    base::stop("Error: 'items' contains NAs!")
  }



  # test whether res is specified
  if (base::missing(res)) {
    base::stop("Error: 'res' is not defined!")
  }

  # test whether res is correctly specified
  if ((res != "agg") && (res != "ind")) {
    base::stop(
      "Error: 'res' can only be set to 'agg' or 'ind'!"
    )
  }

  # can not specify res to 'ind' and specify group
  if ((res == "ind") && !base::missing(group)) {
    stop("Error: Can not speficy 'group' if 'res' is set to 'ind'!")
  }

  # test length of anchor
  if (!base::missing(anchor)) {
    anc <- data %>%
      dplyr::select(., {{ anchor }}) %>%
      base::colnames(.)

    if (length(anc) > 1) {
      base::stop("Error: 'anchor' can only be one variable!")
    }
  }

  if (!base::missing(anchor)) {
    if (!(data %>% dplyr::select(., {{ anchor }}) %>% base::colnames()) %in%
      (data %>% dplyr::select(., {{ items }}) %>% base::colnames())) {
      stop("Error: 'anchor' has to be part of 'items'!")
    }
  }




  #######################################################

  var_items <- data %>%
    dplyr::select(., {{ items }}) %>%
    base::colnames(.)


  for (i in base::seq_len(base::nrow(data))) {
    vec <- base::unname(base::unlist(c(data[i, var_items])))

    # data[i, var_items] <- NA

    vec <- scales::rescale(vec, to = c(0, 100)) - base::mean(
      scales::rescale(vec, to = c(0, 100)))

    if (!(base::missing(anchor))) {
      vec <- vec - vec[match(
        (data %>% dplyr::select(., {{ anchor }}) %>% colnames()),
        var_items
      )]
    }

    data[i, var_items] <- vec
  }

  if (res == "agg") {
    if (base::missing(group)) {
      return(data %>%
        dplyr::reframe(dplyr::across(tidyselect::all_of(var_items),
                                     c(mw = base::mean, std = stats::sd),
          .names = "{.col}....{.fn}"
        )) %>%
        tidyr::pivot_longer(.,
          cols = tidyselect::ends_with(c("....mw", "....std")),
          names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
        ))
    }

    if (!(base::missing(group))) {
      return(data %>%
        dplyr::group_by(dplyr::pick({{ group }})) %>%
        dplyr::reframe(dplyr::across(tidyselect::all_of(var_items),
                                     c(mw = base::mean, std = stats::sd),
          .names = "{.col}....{.fn}"
        )) %>%
        tidyr::pivot_longer(.,
          cols = tidyselect::ends_with(c("....mw", "....std")),
          names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
        ))
    }
  }

  if (res == "ind") {
    return(data)
  }
}
