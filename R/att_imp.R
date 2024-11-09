#' Function to calculate attributes' importance score of (A)CBCs
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s) to get
#' `att_imp` by group(s).
#' @param attrib A list that specifies the attribute levels for each attribute.
#' @param coding A vector of the coding of each attribute, '0' = part-worth
#' coding, '1' = linear coding, or '2' = piecewise coding.
#' @param interpolate.levels A list of the attribute levels that should
#' be interpolated. These have to be the same as specified in model estimation
#' (e.g., if you center attribute levels before estimation, insert the centered
#' levels). Please make sure to provide the whole list. Only has to be
#' specified for the variables that are coded as `1` (linear).
#' @param res A vector indicating whether individual shares (`ind`) or
#' aggregated (`agg`) shares should be returned.
#'
#' @details
#' `att_imp` converts raw utilities of a CBC or an ACBC to relative
#' importance scores (see, Orme, 2020, p. 80, for more information).
#'
#' `data` has to be a data frame with all attributes and the corresponding
#' levels. Attribute levels need to be the raw utilities of hierarchical Bayes
#'  estimation.
#'
#' `group` optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in `data`.
#'
#' `attrib` specifies the attribute levels for each alternative.
#' Input for `attrib` has to be a list. Needs to specify the column names
#' or column indexes of the attribute levels.
#'
#' `coding` is required to indicate the attribute coding. `0`
#' to indicated part-worth coding, `1` for linear coding, or `2` for
#' piecewise coding.
#'
#' `interpolate.levels` is required for linear coded variables.
#' If scaled or centered values were used for hierarchical Bayes
#' estimation, these need to be defined in `att_imp`.
#' All values have to be specified. For example, if one linear coded attribute
#' has 5 levels, all 5 levels have to be inserted.
#'
#' `res` specifies whether results should be aggregated across all
#' participants or across `group` (`res` needs to be set to
#' `agg`) or if scores should be converted for individuals only.
#'
#'
#' @seealso {
#' \code{\link[=prob_scores]{prob_scores}} for probability scores for MaxDiff
#' \code{\link[=zero_anchored]{zero_anchored}} for zero-anchored interval
#' scores for MaxDiff
#' \code{\link[=zc_diffs]{zc_diffs}} for zero-centered diff scores for (A)CBC
#' }
#'
#' @return a tibble
#' @importFrom dplyr across group_by mutate pick reframe select vars
#' @importFrom magrittr "%>%"
#' @importFrom stats sd
#' @importFrom tidyselect all_of ends_with
#' @importFrom tidyr pivot_longer
#'
#' @references {
#'
#' Orme, B. K. (2020). \emph{Getting Started with Conjoint Analysis: Strategies
#' for Product Design and Pricing Research}. 4th edition. Manhattan Beach,
#' CA: Research Publishers LLC.
#'
#' }
#'
#' @examples
#'
#' library(validateHOT)
#'
#' att_imp(
#'   data = CBC,
#'   attrib = list(
#'     c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
#'     c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
#'     c(
#'       "Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4",
#'       "Att3_Lev5", "Att3_Lev6", "Att3_Lev7"
#'     )
#'   ),
#'   coding = c(0, 0, 0),
#'   res = "agg"
#' )
#'
#' att_imp(
#'   data = CBC_lin,
#'   attrib = list(
#'     c("Att1_Lev1", "Att1_Lev2", "Att1_Lev3", "Att1_Lev4", "Att1_Lev5"),
#'     c("Att2_Lev1", "Att2_Lev2", "Att2_Lev3", "Att2_Lev4", "Att2_Lev5"),
#'     c("Att3_Lin")
#'   ),
#'   coding = c(0, 0, 1),
#'   interpolate.levels = list(c(10, 20, 30, 40, 50, 60, 70)),
#'   res = "agg"
#' )
#'
#' @export
att_imp <- function(data, group = NULL, attrib, coding,
                    interpolate.levels = NULL, res = c("agg", "ind")) {
  # grouping variable
  ## check for missings
  if (anyNA(data %>% dplyr::select({{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }

  if (is.null(coding)) {
    stop("Error: 'coding' has to be specified!")
  }

  if (length(attrib) != length(coding)) {
    stop("Error: 'coding' and 'attrib' have to have the same length!")
  }


  # test numeric input for coding
  if (!(is.null(coding))) {
    for (i in seq_along(coding)) {
      if (!(is.numeric(coding[i]))) {
        stop("Error: 'coding' only can have numeric input!")
      }
    }
  }

  # test whether coding only includes 0, 1
  if (any(coding != 0 & coding != 1 & coding != 2)) {
    stop(
      "Error: Please only use '0' (for part-worth), '1' (for linear), or
      '2' (for piecewise)!"
    )
  }


  # test input of interpolate levels
  if (!(is.list(interpolate.levels)) &&
    !(is.null(interpolate.levels))) {
    stop("Error: Input of 'interpolate.levels' has to be a list!")
  }

  # test variables of interpolate.levels
  if (!(is.null(interpolate.levels))) {
    for (tt in seq_along(interpolate.levels)) {
      lng <- length(interpolate.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(is.numeric(interpolate.levels[[tt]][lng_lev]))) {
          stop(
            "Error: Input of 'interpolate.levels' has to be a list ",
            "with only numeric values!"
          )
        }
      }
    }
  }

  # interpolate.levels can not be larger than number of attributes
  if (!is.null(interpolate.levels) &&
    (length(interpolate.levels) > length(attrib))) {
    stop(
      "Error: List of 'interpolate.levels' can not be larger than list of
      'attrib'!"
    )
  }

  # check length of 'attrib' and coding
  # test input of list in prod.levels
  if (!(is.null(attrib))) {
    for (tt in seq_along(attrib)) {
      if (length(attrib[[tt]]) == 1 && coding[tt] == 0) {
        stop(
          "Error: If attribute is part-worth coded at least 2 attribute levels
          need to be specified!"
        )
      }

      if (length(attrib[[tt]]) > 1 && coding[tt] == 1) {
        stop(
          "Error: If attribute is linear coded only one attribute level needs
          to be specified!"
        )
      }

      if (length(attrib[[tt]]) == 1 && coding[tt] == 2) {
        stop(
          "Error: If attribute is piecewise coded at least 2 attribute levels
          need to be specified!"
        )
      }
    }
  }

  # if 1 is coded, interpolate.levels needs to be specified
  if (any(coding == 1) && is.null(interpolate.levels)) {
    stop(
      "Error: 'interpolate.levels' is missing!"
    )
  }

  # if no 1 coded interpolate.levels is not needed
  if (!any(coding == 1) && !is.null(interpolate.levels)) {
    stop(
      "Error: 'interpolate.levels' only needs to be specified for linear
      coded variables!"
    )
  }

  # number of coding larger than length of interpolate levels

  if (!is.null(interpolate.levels) &&
    (sum(coding == 1) != length(interpolate.levels))) {
    stop(
      "Error: Number of linear coded variables is not equal to length of
      'interpolate.levels'!"
    )
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

  # number of coding larger than length of interpolate levels

  if (any(coding == 2)) {
    if (sum(coding == 2) > 1) {
      stop("Error: Only one attribute can be piecewise-coded!")
    }
  }

  # can not specify res to 'ind' and specify group
  if ((res == "ind") && !missing(group)) {
    stop("Error: Can not speficy 'group' if 'res' is set to 'ind'!")
  }

  ############################################################################

  att <- length(attrib)

  new <- c()

  helper <- 1
  for (i in 1:att) {
    data[[paste0("att_imp_", i)]] <- 0

    vars <- attrib[[i]]

    new <- c(new, paste0("att_imp_", i))

    for (j in seq_len(nrow(data))) {
      if (coding[i] == 0 || coding[i] == 2) {
        data[j, paste0("att_imp_", i)] <- abs(
          diff(range(data[j, vars]))
        )
      }

      if (coding[i] == 1) {
        data[j, paste0("att_imp_", i)] <- abs(
          data[j, vars] * abs(
            diff(range(interpolate.levels[[helper]]))
          )
        )
      }
    }

    if (coding[i] == 1) {
      helper <- helper + 1
    }
  }

  if (res == "agg") {
    attribute_importance <- data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(new),
        ~ .x / rowSums(data[new])
      )) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(dplyr::across(tidyselect::all_of(new),
        c(mw = mean, std = stats::sd),
        .names = "{.col}....{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c("....mw", "....std")),
        names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
      ) %>%
      dplyr::mutate_at(dplyr::vars(mw, std), ~ .x * 100)

    return(attribute_importance)
  }

  if (res == "ind") {
    attribute_importance <- data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(new),
        ~ .x / rowSums(data[new])
      )) %>%
      dplyr::select({{ group }}, tidyselect::all_of(new)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(new), ~ .x * 100))

    return(attribute_importance)
  }
}
