#' Function to run Total Unduplicated Reach and Frequency analysis
#'
#' @description
#' T(otal) U(nduplicated) R(each) and F(requency) is
#' a "product line extension model" (Miaoulis et al., 1990, p. 29). For each
#' possible combinations, it looks for the reach and frequency of this
#' combination. Participants are reached, if at least one of the alternatives
#' in a combination has a higher utility than \code{none}. On the contrary,
#' frequency calculates the averaged number of alternatives that have a
#' higher utility than \code{none}.
#'
#' @param data A data frame with all relevant variables.
#' @param opts Column names of the alternatives included in the assortment.
#' @param none Column name of none / threshold alternative.
#' @param size A numeric vector to determine size of the assortment.
#' @param fixed An optional vector to determine alternatives/item that have to
#' be included in the assortment.
#' @param prohib An optional list with vectors to determine prohibitions, i.e.,
#' alternatives/items that are not allowed to be together in one assortment.
#' @param approach A character whether to run First Choice approach ('fc') or
#' Threshold approach ('thres').
#'
#' @details
#'
#' \code{data} has to be a data frame including the alternatives that should be
#' tested.
#'
#' \code{opts} is required to specify the different alternatives in the
#' product assortment that should be considered.
#' Input of \code{opts} has to be column names of variables in \code{data}.
#'
#' \code{none} to specify column name of the \code{none} alternative in the
#' validation/holdout task.
#'
#' \code{size} has to be a whole number determining the size of the assortment.
#'
#' \code{fixed} has to be a vector of variables that are fixed in the
#' assortment, i.e., they have to be part of the assortment.
#'
#' \code{prohib} has to be a list of vectors of variables that are prohibited
#' in the assortment, i.e., alternatives that are not allowed to be together in
#' one assortment.
#'
#' \code{approach} character defining whether first
#' choice \code{approach = 'fc'} or threshold \code{approach = 'thres'} should
#' be applied for running \code{turf()}. If \code{approach = 'fc'},
#' participants are considered being reached, if their alternative with the
#' highest utility is included in the assortment and this alternative's utility
#' is larger than the threshold's utility (Chrzan & Orme, 2019, p. 111).
#' On the contrary, if \code{approach = 'thres'}, participants are considered
#' being reached, if utility of one product is higher than the one of
#' the \code{none} alternative (Chrzan & Orme, 2019, p. 112).
#' If \code{approach = 'fc'}, \code{reach} equals \code{freq} since
#' participants have at maximum their most preferred alternative that
#' exceeds the \code{none} alternative.
#'
#' @references {
#'
#' Chrzan, K., & Orme, B. K. (2019). \emph{Applied MaxDiff: A Practitioner’s
#' Guide to Best-Worst Scaling} Provo, UT: Sawtooth Software.
#'
#' Miaoulis, G., Parsons, H., & Free, V. (1990). Turf: A New Planning Approach
#' for Product Line Extensions. \emph{Marketing Research 2} (1): 28-40.
#'
#' }
#'
#' @importFrom dplyr across arrange filter mutate rename rename_all
#' relocate select
#' @importFrom magrittr "%>%"
#' @importFrom utils combn
#' @importFrom tidyselect all_of everything starts_with
#' @importFrom tibble remove_rownames
#'
#' @return data frame
#'
#' @examples
#'
#' library(validateHOT)
#'
#' HOT <- createHOT(
#'   data = MaxDiff,
#'   id = 1,
#'   none = 19,
#'   prod.levels = list(
#'     3, 4, 5, 6, 7, 8, 9, 10, 11,
#'     12, 13, 14, 15, 16, 17, 18
#'   ),
#'   method = "MaxDiff",
#'   choice = 20
#' )
#'
#' # turf no fixed alternatives + no prohibitions
#' t1 <- turf(
#'   data = HOT,
#'   opts = c(Option_1:Option_16),
#'   none = None,
#'   size = 3L,
#'   approach = "thres"
#' )
#'
#' head(t1)
#'
#' # turf alternative 4 and 5 fixed, no prohibitions
#' t2 <- turf(
#'   data = HOT,
#'   opts = c(Option_1:Option_16),
#'   none = None,
#'   size = 4L,
#'   fixed = c("Option_4", "Option_5"),
#'   approach = "thres"
#' )
#'
#' head(t2)
#'
#' #' # turf alternative 4 and 5 fixed, 2 and 9 not allowed together
#' t3 <- turf(
#'   data = HOT,
#'   opts = c(Option_1:Option_16),
#'   none = None,
#'   size = 4L,
#'   fixed = c("Option_4", "Option_5"),
#'   prohib = list(c("Option_2", "Option_9")),
#'   approach = "thres"
#' )
#'
#' head(t3)
#'
#' @export
turf <- function(data, opts, none, size, fixed = NULL, prohib = NULL,
                 approach = c("thres", "fc")) {
  # check for wrong / missing input
  if (base::length(data %>% dplyr::select(., {{ none }})) == 0) {
    base::stop("Error: argument 'none' is missing!")
  }

  if (base::length(data %>% dplyr::select(., {{ opts }})) == 0) {
    base::stop("Error: argument 'opts' is missing!")
  }

  # size can not be larger than or equal to opts
  if (size > base::length(data %>% dplyr::select(., {{ opts }}))) {
    base::stop("Error: 'size' can not be larger than size of 'opts'!")
  }

  # approach needs to be specified
  if (base::missing(approach)) {
    base::stop("Error: 'approach' is missing!")
  }

  # approach can only be 'thres' or 'fc'
  if ((approach != "thres") & (approach != "fc")) {
    base::stop(
      "Error: 'approach' is wrong, please choose between",
      " 'fc' and 'thres'!"
    )
  }

  # size has to be numeric
  ## imports whole number function from base package
  is.wholenumber <-
    function(x, tol = .Machine$double.eps^0.5) abs(x - round(x)) < tol
  if (!is.wholenumber(size)) {
    base::stop("Error: 'size' must be a whole number!")
  }
  rm(is.wholenumber)

  # fixed has to be part of opts
  if (!base::is.null(fixed)) {
    fix_ones <- data %>%
      dplyr::select(tidyselect::all_of(fixed)) %>%
      base::colnames()

    if (!base::all(fix_ones %in% (data %>% dplyr::select(., {{ opts }}) %>%
      base::colnames()))) {
      base::stop("Error: 'fixed' has to be part of 'opts'!")
    }
  }


  # fixed can not be larger than size
  if (!base::is.null(fixed)) {
    if (length(data %>% dplyr::select(tidyselect::all_of(fixed))) > size) {
      stop("Error: 'fixed' can not be larger than 'size'!")
    }
  }

  # prohib has to be part of opts
  if (!base::is.null(prohib)) {
    for (i in base::seq_along(prohib)) {
      if (!base::all(prohib[[i]] %in% (data %>%
                                       dplyr::select(., {{ opts }}) %>%
        base::colnames()))) {
        base::stop("Error: 'prohib' has to be part of 'opts'!")
      }
    }
  }

  # fixed can not be larger than size
  if (!base::is.null(prohib)) {
    for (i in base::seq_along(prohib)) {
      if (length(prohib[[i]]) > size) {
        stop("Error: 'prohib' can not be larger than 'size'!")
      }
    }
  }

  if (!base::is.null(prohib)) {
    if (!base::is.list(prohib)) {
      base::stop("Error: 'prohib' has to be a list!")
    }
  }

  # error if prohib and fixed are exactly the same
  if (!base::is.null(prohib) & !base::is.null(fixed)) {
    for (i in base::seq_along(prohib)) {
      if (base::all(prohib[[i]] %in% fixed)) {
        base::stop("Error: 'prohib' and 'fixed' have to be different!")
      }
    }
  }

  # alternatives
  ## store names of alternatives
  alternatives <- data %>%
    dplyr::select(., {{ opts }}) %>%
    base::colnames()

  ## check whether variable is numeric
  for (i in base::seq_along(alternatives)) {
    if (!base::is.numeric(data[[alternatives[i]]])) {
      stop("Error: 'opts' has to be numeric!")
    }
  }

  ## check for missings
  if (anyNA(data %>% dplyr::select(., {{ opts }}))) {
    stop("Error: 'opts' contains NAs!")
  }

  # None
  ## check for missing
  if (base::anyNA(data %>% dplyr::select(., {{ none }}))) {
    stop("Error: 'none' contains NAs!")
  }

  ## check for str
  Noo <- data %>%
    dplyr::select(., {{ none }}) %>%
    base::colnames()

  if (!base::is.numeric(data[[Noo]])) {
    stop("Error: 'none' has to be numeric!")
  }

  ## check none can not be part of opts
  if ((data %>% dplyr::select(., {{ none }}) %>% base::colnames()) %in%
    (data %>% dplyr::select(., {{ opts }}) %>% base::colnames())) {
    stop("Error: 'none' can not be part of 'opts'!")
  }

  if (base::length(Noo) > 1) {
    base::stop("Error: 'none' can only be one variable!")
  }


  # prepare data frame and threshold approach
  if (approach == "thres") { # threshold approach
    df <- data %>%
      dplyr::select(., {{ opts }}, {{ none }}) %>% # select relevant variables
      dplyr::rename("thres" = ncol(.)) %>% # rename variable
      dplyr::mutate(
        # if value of alternatives above threshold --> reached
        dplyr::across({{ opts }}, ~ base::ifelse(.x > thres, 1, 0))
      ) %>%
      dplyr::select(-thres) # delete threshold
  }


  if (approach == "fc") { # first choice rule
    df <- data %>%
      # select relevant variables
      dplyr::select(., {{ opts }}, {{ none }}) %>%
      # rename variable
      dplyr::rename("thres" = ncol(.)) %>%
      # store column index with highest value
      dplyr::mutate(maximum = base::max.col(.))

    for (row in base::seq_len(base::nrow(df))) { # loop for each row
      # loop for each column (except for last two --> thres and maximum)
      for (col in 1:(base::ncol(df) - 2)) {
        # only run if larger than threshold and if row maximum
        if (df[row, col] > df[row, "thres"] & col == df[row, "maximum"]) {
          df[row, col] <- 1 # assign buy
        } else {
          df[row, col] <- 0 # assign no buy
        }
      }
    }

    df <- df %>%
      dplyr::select(-c(maximum, thres)) # delete irrelevant rows
  }

  # prepare items
  items <- data %>%
    dplyr::select(., {{ opts }}) %>% # select specified opts
    base::colnames() # store column names only

  # define new variable names
  var_names <- c(items, paste0("new_col_names_", c(1:size)))
  var_names <- base::make.unique(var_names, sep = "...")
  var_names <- var_names[-c(base::seq_along(items))]

  # create combos
  # create all possible combinations
  combos <- base::as.data.frame(base::t(utils::combn(items, size))) %>%
    dplyr::rename_all(., ~var_names) # rename variables

  # only run if there are fixed values and delete ones that do not contain
  # fixed values
  if (!base::is.null(fixed)) {
    # create all possible combinations

    fixies <- data %>%
      dplyr::select(tidyselect::all_of(fixed)) %>% # store the fixed values
      base::colnames() # store the column names

    combos <- combos %>%
      # check for each combo whether the fixed ones are included
      dplyr::mutate(must = base::apply(., 1, function(x) {
        base::as.integer(base::all(fixies %in% x))
      })) %>%
      # only choose those that have the fixed options
      dplyr::filter(must == 1) %>%
      dplyr::select(-must) # delete must variable
  }

  if (!base::is.null(prohib)) {
    for (i in base::seq_along(prohib)) {
      prohibitions <- data %>%
        dplyr::select(tidyselect::all_of(prohib[[i]])) %>%
        base::colnames()

      combos <- combos %>%
        # check for each combo whether the fixed ones are included
        dplyr::mutate(not = base::apply(., 1, function(x) {
          base::as.integer(base::all(prohibitions %in% x))
        })) %>%
        # only choose those that have the fixed options
        dplyr::filter(not == 0) %>%
        dplyr::select(-not) # delete must variable
    }
  }

  # count reach and frequency for each
  for (i in base::seq_len(base::nrow(combos))) {
    # store the combos as vector
    combs <- base::unname(c(base::unlist(combos[i, ])))

    # create combination and store the rowsum for that
    # combination for each participant
    df[[base::paste0("comb.", base::paste0(combs, collapse = "_"))]] <-
      base::rowSums(df[, combs])
  }


  # next create the total data frame
  total <- base::merge(
    x = (df %>%
      # only select the variables that start with comb. (see approach before)
      dplyr::select(tidyselect::all_of(tidyselect::starts_with("comb."))) %>%
      # create the reach score
      dplyr::reframe(dplyr::across(
        base::seq_len(base::ncol(.)),
        ~ (base::sum(. > 0) / base::nrow(df)
          * 100)
      )) %>%
      base::t() %>% # transpose
      base::as.data.frame() %>% # store as data frame
      # rename variable into 'reach'
      dplyr::rename_all(., ~"reach") %>%
      # store rownames in new variable
      dplyr::mutate(combo = base::rownames(.)) %>%
      tibble::remove_rownames() %>% # remove rownames
      # relocate combo variable at the beginning
      dplyr::relocate(combo, .before = tidyselect::everything())),
    y = (df %>%
      # only select the variables that start with comb. (see approach before)
      dplyr::select(tidyselect::all_of(tidyselect::starts_with("comb."))) %>%
      # create frequency score
      dplyr::reframe(dplyr::across(base::seq_len(base::ncol(.)),
                                   ~ base::mean(.x))) %>%
      base::t() %>% # transpose
      base::as.data.frame() %>% # store as data frame
      dplyr::rename_all(., ~"freq") %>% # rename variable into 'freq'
      # store rownames in new variable
      dplyr::mutate(combo = base::rownames(.)) %>%
      tibble::remove_rownames() %>% # remove rownames
      # relocate combo variable at the beginning
      dplyr::relocate(combo, .before = tidyselect::everything())),
    by = "combo" # merge both by 'combo'
  )

  # prepare a new data frame that mimics output of turfR (variables are
  # coded as 1 and 0 whether or not item is present in that assortment or not)
  # create a new data frame
  new_df <- base::data.frame(base::matrix(
    nrow = base::nrow(combos),
    ncol = base::length(items)
  )) %>%
    dplyr::rename_all(., ~items) %>% # rename columns
    # replace nas by 0s
    dplyr::mutate_all(., ~ base::ifelse(base::is.na(.x), 0, NA)) %>%
    base::cbind(combos, .) # bind with combos

  # prepare the binary coding
  for (i in base::seq_len(base::nrow(new_df))) { # loop for each row
    for (j in 1:size) { # repeat for each assortment
      new_df[i, new_df[i, j]] <- 1 # store 1
    }
  }

  # create 'combo' variable for merging purposes
  new_df <- new_df %>%
    dplyr::mutate(combo = 0)

  # store the names in the variable for merging purposes
  for (i in base::seq_len(base::nrow(new_df))) { # loop for merging purposes
    new_df[i, "combo"] <- base::paste0(
      "comb.",
      base::paste0(new_df[i, c(1:size)],
        collapse = "_"
      )
    )
  }

  # prepare final step

  return(new_df %>%
    # delete the first variables (variables indicating name of item)
    dplyr::select(-tidyselect::all_of(base::colnames(new_df)[1:size])) %>%
    base::merge(x = total, y = ., by = "combo") %>% # merge with total
    arrange(-reach, -freq) %>% # sort descending for reach and frequency
    mutate(combo = paste0("Combo ", dplyr::row_number()))) # rename combo
}
