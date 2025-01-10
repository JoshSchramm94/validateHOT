#' MaxDiff data set with raw logit utilities
#'
#' A data set with raw scores export for a MaxDiff (Best-Worst Scaling case 1).
#'
#'
#' \describe{
#'   \item{id}{(integer) Unique identifier}
#'   \item{option_01}{(numeric) Part-worth utility of maxdiff object 1}
#'   \item{option_02}{(numeric) Part-worth utility of maxdiff object 2}
#'   \item{option_03}{(numeric) Part-worth utility of maxdiff object 3}
#'   \item{option_04}{(numeric) Part-worth utility of maxdiff object 4}
#'   \item{option_05}{(numeric) Part-worth utility of maxdiff object 5}
#'   \item{option_06}{(numeric) Part-worth utility of maxdiff object 6}
#'   \item{option_07}{(numeric) Part-worth utility of maxdiff object 7}
#'   \item{option_08}{(numeric) Part-worth utility of maxdiff object 8}
#'   \item{option_09}{(numeric) Part-worth utility of maxdiff object 9}
#'   \item{option_10}{(numeric) Part-worth utility of maxdiff object 10}
#'   \item{option_11}{(numeric) Part-worth utility of maxdiff object 11}
#'   \item{option_12}{(numeric) Part-worth utility of maxdiff object 12}
#'   \item{option_13}{(numeric) Part-worth utility of maxdiff object 13}
#'   \item{option_14}{(numeric) Part-worth utility of maxdiff object 14}
#'   \item{option_15}{(numeric) Part-worth utility of maxdiff object 15}
#'   \item{option_16}{(numeric) Part-worth utility of maxdiff object 16}
#'   \item{none}{(numeric) Part-worth utility of outside good (no-buy option)}
#'   \item{hot}{(integer) Actual choice in the validation task}
#'   \item{group}{(integer) Grouping variable}
#' }
#'
#' @docType data
#' @keywords datasets
#' @name maxdiff
#' @usage data(maxdiff)
#' @source \url{https://osf.io/m5x3a/}
#' @format A data frame with 118 rows and 20 variables
#'
#' @examples
#'
#' data(maxdiff)
#' # create total utility of validation tasks
#'
#' hot_mxd <- create_hot(
#'   data = maxdiff,
#'   id = "id",
#'   none = "none",
#'   prod.levels = list(2, 9, 10, 14, 15, 16, 17),
#'   method = "maxdiff",
#'   varskeep = "group",
#'   choice = "hot"
#' )
#'
#'
#' # measure probability scores of unanchored MaxDiff
#'
#' prob_scores(
#'   data = maxdiff,
#'   items = c(option_01:option_16),
#'   set.size = 4,
#'   res = "agg"
#' )
#'
#' # measure probability scores of anchored MaxDiff
#'
#' prob_scores(
#'   data = maxdiff,
#'   items = c(option_01:none),
#'   set.size = 4,
#'   anchor = none,
#'   res = "agg"
#' )
#'
#'
#' # convert raw utilities into zero-anchored diffs
#'
#' # unanchored MaxDiff
#' zero_anchored(
#'   data = maxdiff,
#'   items = c(option_01:option_16),
#'   res = "agg"
#' )
#'
#' # anchored MaxDiff
#' zero_anchored(
#'   data = maxdiff,
#'   group = group,
#'   items = c(option_01:option_16),
#'   res = "agg"
#' )
"maxdiff"
