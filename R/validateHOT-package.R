#' @description
#' validateHOT is a package to validate validation tasks in preference
#' measurement techniques, run market simulations, and convert raw utilities
#' into scores that are easier to interpret.
#'
#' @name validateHOT
"_PACKAGE"

## usethis namespace: start
#' @importFrom dplyr across arrange c_across count group_by mutate n pick
#' reframe relocate rename rename_all rowwise select slice ungroup vars
#' @importFrom magrittr "%>%"
#' @importFrom tidyr pivot_longer
#' @importFrom tidyselect all_of ends_with everything starts_with
#' @importFrom stats approx median sd
#' @importFrom tibble remove_rownames
#' @importFrom fastDummies dummy_cols
#' @importFrom utils combn
#' @importFrom cli cli_abort cli_warn
#' @importFrom rlang caller_arg caller_env "%||%"
## usethis namespace: end
NULL

utils::globalVariables(
  c(
    ".",
    "actual",
    "add_reach",
    "add_freq",
    "alt",
    "ch_share",
    "choice",
    "chosen",
    "combo",
    "fc",
    "freq",
    "merger",
    "mult_factor",
    "mw",
    "pred",
    "predicted",
    "se",
    "share",
    "size",
    "std"
  )
)
