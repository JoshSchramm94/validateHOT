#' Function to calculate attributes' importance score of (A)CBCs
#'
#' @param data A data frame with all relevant variables.
#' @param group Optional column name(s) to specify grouping variable(s) to get
#' \code{att_imp} by group(s).
#' @param attrib A list that specifies the attribute levels for each attribute.
#' @param coding A vector of the coding of each attribute, '0' = part-worth
#' coding, '1' = linear coding, or '2' = piecewise coding.
#' @param interpolate.levels A list of the attribute levels that should
#' be interpolated. These have to be the same as specified in model estimation
#' (e.g., if you center attribute levels before estimation, insert the centered
#' levels). Please make sure to provide the whole list. Only has to be
#' specified for the variables that are coded as '1' (linear).
#' @param res A vector indicating whether individual shares (\code{ind}) or
#' aggregated (\code{agg}) shares should be returned.
#'
#' @details
#' \code{att_imp} converts raw utilities of a CBC or an ACBC to relative
#' importance scores (see, Orme, 2020, p. 80, for more information).
#'
#' \code{data} has to be a data frame with all attributes and the corresponding
#' levels. Attribute levels need to be the raw utilities of hierarchical Bayes
#'  estimation.
#'
#' \code{group} optional grouping variable, if results should be displayed by
#' different groups. Has to be column name of variables in \code{data}.
#'
#' \code{attrib} specifies the attribute levels for each alternative.
#' Input for \code{attrib} has to be a list. Needs to specify the column names
#' or column indexes of the attribute levels.
#'
#' \code{coding} is required to indicate the attribute coding. \code{0}
#' to indicated part-worth coding, \code{1} for linear coding, or \code{2} for
#' piecewise coding.
#'
#' \code{interpolate.levels} is required for linear coded variables.
#' If scaled or centered values were used for hierarchical Bayes
#' estimation, these need to be defined in \code{att_imp}.
#' All values have to be specified. For example, if one linear coded attribute
#' has 5 levels, all 5 levels have to be inserted.
#'
#' \code{res} specifies whether results should be aggregated across all
#' participants or across \code{group} (\code{res} needs to be set to
#' \code{agg}) or if scores should be converted for individuals only.
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
#'     c("Att3_Lev1", "Att3_Lev2", "Att3_Lev3", "Att3_Lev4",
#'     "Att3_Lev5", "Att3_Lev6", "Att3_Lev7")
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
  if (base::anyNA(data %>% dplyr::select(., {{ group }}))) {
    warning("Warning: 'group' contains NAs!")
  }

  if (base::is.null(coding)) {
    base::stop("Error: 'coding' has to be specified!")
  }

  if (base::length(attrib) != base::length(coding)) {
    base::stop("Error: 'coding' and 'attrib' have to have the same length!")
  }


  # test numeric input for coding
  if (!(base::is.null(coding))) {
    for (i in base::seq_along(coding)) {
      if (!(base::is.numeric(coding[i]))) {
        base::stop("Error: 'coding' only can have numeric input!")
      }
    }
  }

  # test whether coding only includes 0, 1
  if (base::any(coding != 0 & coding != 1 & coding != 2)) {
    base::stop(
      "Error: Please only use '0' (for part-worth), '1' (for linear), or
      '2' (for piecewise)!"
    )
  }


  # test input of interpolate levels
  if (!(base::is.list(interpolate.levels)) &&
    !(base::is.null(interpolate.levels))) {
    base::stop("Error: Input of 'interpolate.levels' has to be a list!")
  }

  # test variables of interpolate.levels
  if (!(base::is.null(interpolate.levels))) {
    for (tt in base::seq_along(interpolate.levels)) {
      lng <- base::length(interpolate.levels[[tt]])

      for (lng_lev in 1:lng) {
        if (!(base::is.numeric(interpolate.levels[[tt]][lng_lev]))) {
          base::stop(
            "Error: Input of 'interpolate.levels' has to be a list ",
            "with only numeric values!"
          )
        }
      }
    }
  }

  # interpolate.levels can not be larger than number of attributes
  if (!base::is.null(interpolate.levels) &&
    (base::length(interpolate.levels) > base::length(attrib))) {
    base::stop(
      "Error: List of 'interpolate.levels' can not be larger than list of
      'attrib'!"
    )
  }

  # check length of 'attrib' and coding
  # test input of list in prod.levels
  if (!(base::is.null(attrib))) {
    for (tt in base::seq_along(attrib)) {
      if (base::length(attrib[[tt]]) == 1 && coding[tt] == 0) {
        base::stop(
          "Error: If attribute is part-worth coded at least 2 attribute levels
          need to be specified!"
        )
      }

      if (base::length(attrib[[tt]]) > 1 && coding[tt] == 1) {
        base::stop(
          "Error: If attribute is linear coded only one attribute level needs
          to be specified!"
        )
      }

      if (base::length(attrib[[tt]]) == 1 && coding[tt] == 2) {
        base::stop(
          "Error: If attribute is piecewise coded at least 2 attribute levels
          need to be specified!"
        )
      }
    }
  }

  # if 1 is coded, interpolate.levels needs to be specified
  if (base::any(coding == 1) && base::is.null(interpolate.levels)) {
    base::stop(
      "Error: 'interpolate.levels' is missing!"
    )
  }

  # if no 1 coded interpolate.levels is not needed
  if (!base::any(coding == 1) && !base::is.null(interpolate.levels)) {
    base::stop(
      "Error: 'interpolate.levels' only needs to be specified for linear
      coded variables!"
    )
  }

  # number of coding larger than length of interpolate levels

  if (!base::is.null(interpolate.levels) &&
      (base::sum(coding == 1) != base::length(interpolate.levels))) {
    base::stop(
      "Error: Number of linear coded variables is not equal to length of
      'interpolate.levels'!"
    )
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

  # number of coding larger than length of interpolate levels

  if (base::any(coding == 2)) {
    if (base::sum(coding == 2) > 1) {
      base::stop("Error: Only one attribute can be piecewise-coded!")
    }
  }

  # can not specify res to 'ind' and specify group
  if ((res == "ind") && !base::missing(group)) {
    stop("Error: Can not speficy 'group' if 'res' is set to 'ind'!")
  }

  ############################################################################

  att <- base::length(attrib)

  new <- c()

  helper <- 1
  for (i in 1:att) {
    data[[base::paste0("att_imp_", i)]] <- 0

    vars <- attrib[[i]]

    new <- c(new, base::paste0("att_imp_", i))

    for (j in base::seq_len(base::nrow(data))) {
      if (coding[i] == 0 || coding[i] == 2) {
        data[j, base::paste0("att_imp_", i)] <- base::abs(
          base::diff(base::range(data[j, vars])))
      }

      if (coding[i] == 1) {
        data[j, base::paste0("att_imp_", i)] <- base::abs(
          data[j, vars] * base::abs(
            base::diff(base::range(interpolate.levels[[helper]]))))
      }
    }

    if (coding[i] == 1) {
      helper <- helper + 1
    }
  }

  if (res == "agg") {
    return(data %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(new),
                                  ~ .x / base::rowSums(data[new]))) %>%
      dplyr::group_by(dplyr::pick({{ group }})) %>%
      dplyr::reframe(dplyr::across(tidyselect::all_of(new),
                                   c(mw = base::mean, std = stats::sd),
        .names = "{.col}....{.fn}"
      )) %>%
      tidyr::pivot_longer(.,
        cols = tidyselect::ends_with(c("....mw", "....std")),
        names_to = c("Option", ".value"), names_sep = "\\.\\.\\.\\."
      ) %>%
      dplyr::mutate_at(dplyr::vars(mw, std), ~ .x * 100))
  }

  if (res == "ind") {
    return(data %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(new),
                                  ~ .x / base::rowSums(data[new]))) %>%
      dplyr::select({{ group }}, tidyselect::all_of(new)) %>%
      dplyr::mutate(dplyr::across(tidyselect::all_of(new), ~ .x * 100)))
  }
}
