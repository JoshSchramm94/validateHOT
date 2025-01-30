# tests ------------------------------------------------------------------------
variable_numeric <- function(data,
                             variable,
                             argument,
                             arg = rlang::caller_arg(argument),
                             call = rlang::caller_env()) {
  alts <- dplyr::select(data, {{ variable }}) %>%
    colnames()

  opts_correct <- vapply(
    X = data[, alts],
    FUN = is.numeric,
    FUN.VALUE = TRUE
  )

  if (any(!opts_correct)) {
    if (length(alts) > 1) {
      wrong <- names(which(!opts_correct)[1])
    } else {
      wrong <- alts
    }

    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.cls numeric} input.",
        "{.var {wrong}} is of class {.cls {class(data[[wrong]])}}."
      ),
      call = call
    )
  }
}


variable_numeric_alt <- function(data,
                                 variable,
                                 argument,
                                 arg = rlang::caller_arg(argument),
                                 call = rlang::caller_env()) {
  alts <- dplyr::select(data, all_of(variable)) %>%
    colnames()

  opts_correct <- vapply(
    X = data[, alts],
    FUN = is.numeric,
    FUN.VALUE = TRUE
  )

  if (any(!opts_correct)) {
    if (length(alts) > 1) {
      wrong <- names(which(!opts_correct)[1])
    } else {
      wrong <- alts
    }

    cli::cli_abort(
      c(
        "{.arg {arg}} must only contain {.cls numeric} variables.",
        "{.var {wrong}} is of class {.cls {class(data[[wrong]])}}."
      ),
      call = call
    )
  }
}


n_opts_cols <- function(data,
                        opts,
                        call = rlang::caller_env()) {
  n_opts <- dplyr::select(data, {{ opts }})
  n_opts <- ncol(n_opts)

  if (n_opts <= 1) {
    cli::cli_abort(
      c(
        "{.arg opts} must include at least 2 alternatives.",
        "{.arg opts} only includes {.num {n_opts}} alternative."
      ),
      call = call
    )
  }
}

nvar_missings <- function(data,
                          variables,
                          call = rlang::caller_env()) {
  alts <- dplyr::select(data, {{ variables }}) %>%
    colnames()

  vars_correct <- vapply(
    X = data[, alts],
    FUN = anyNA,
    FUN.VALUE = TRUE
  )

  if (any(vars_correct)) {
    if (length(alts) > 1) {
      wrong <- names(which(vars_correct)[1])
    } else {
      wrong <- alts
    }


    cli::cli_abort(
      c(
        "{.var {wrong}} can not have {.cls NA} values."
      ),
      call = call
    )
  }
}

missing_prod.levels <- function(prod.levels,
                                call = rlang::caller_env()) {
  prod.levels_input <- unlist(prod.levels)

  if (anyNA(prod.levels_input)) {
    cli::cli_warn(
      c(
        "{.arg prod.levels} contains {.cls NA} values."
      )
    )
  }
}

missing_group <- function(data,
                          group,
                          call = rlang::caller_env()) {
  group <- dplyr::select(data, {{ group }})

  if (anyNA(group)) {
    cli::cli_warn(
      c(
        "{.arg group} contains {.cls NA} values."
      )
    )
  }
}

none_in_opts <- function(data,
                         none,
                         opts,
                         should,
                         call = rlang::caller_env()) {
  none <- dplyr::select(data, {{ none }}) %>% colnames()

  options <- dplyr::select(data, {{ opts }}) %>% colnames()

  if (isTRUE(should)) {
    if (!none %in% options) {
      cli::cli_abort(
        c(
          "{.arg none} must be part of {.arg opts}."
        ),
        call = call
      )
    }
  }

  if (isFALSE(should)) {
    if (none %in% options) {
      cli::cli_abort(
        c(
          "{.arg none} can not be part of {.arg opts}."
        ),
        call = call
      )
    }
  }
}

ncol_input <- function(
    data,
    variable,
    argument,
    arg = rlang::caller_arg(argument),
    call = rlang::caller_env()) {
  var <- dplyr::select(data, {{ variable }}) %>% colnames()

  if (length(var) > 1) {
    cli::cli_abort(
      c(
        "{.arg {arg}} can only be {.num 1} variable.",
        "{.num {ncol(data[var])}} variabes are provided."
      ),
      call = call
    )
  }
}

numeric_vector <- function(
    argument,
    arg = rlang::caller_arg(argument),
    call = rlang::caller_env()) {
  input <- all(
    vapply(
      X = argument,
      FUN = is.numeric,
      FUN.VALUE = TRUE
    )
  )

  if (!input) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be {.cls numeric}.",
        "{.arg {arg}} is class {.cls {class(argument)}}."
      ),
      call = call
    )
  }
}

allowed_input <- function(
    input,
    allowed,
    arg = rlang::caller_arg(input),
    call = rlang::caller_env()) {
  correct_input <- all(input %in% allowed)

  if (!correct_input) {
    cli::cli_abort(
      c(
        "{.arg {arg}} can only have values {.val {allowed}}."
      ),
      call = call
    )
  }
}

numeric_list <- function(
    argument,
    arg = rlang::caller_arg(argument),
    call = rlang::caller_env()) {
  input_correct <- vapply(
    X = argument,
    FUN = is.numeric,
    FUN.VALUE = TRUE
  )

  if (any(!input_correct)) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be a list with only {.cls numeric} input."
      ),
      call = call
    )
  }
}

same_length <- function(
    var1,
    var2,
    argument1,
    argument2,
    call = rlang::caller_env()) {
  length_var1 <- length(var1)
  length_var2 <- length(var2)

  if (length_var1 != length_var2) {
    cli::cli_abort(
      c(
        "{.arg {argument1}} must match {.arg {argument2}}."
      ),
      call = call
    )
  }
}


list_input <- function(input,
                       arg = rlang::caller_arg(input),
                       call = rlang::caller_env()) {
  input_correct <- is.list(input)

  if (!input_correct) {
    cli::cli_abort(
      c(
        "{.arg {arg}} must be class {.cls list}",
        "{.arg {arg}} of class {.cls {class(input)}}."
      ),
      call = call
    )
  }
}


nested_list_input <- function(input,
                              arg = rlang::caller_arg(input),
                              call = rlang::caller_env()) {
  input_correct <- unlist(lapply(input, is.list))

  if (!any(input_correct)) {
    wrong <- which(!input_correct)[1]
    cli::cli_abort(
      c(
        "{.arg {arg}} must be class {.cls list}",
        "the {.num wrong} input for {.arg {arg}} is of class {.cls {class(input[[wrong]])}}."
      ),
      call = call
    )
  }
}

coding_equals_length_list <- function(
    coding,
    list,
    code,
    arg = rlang::caller_arg(list),
    call = rlang::caller_env()) {
  length_coding <- sum(coding %in% code)
  length_list <- length(list)

  if (any(length_coding != length_list)) {
    cli::cli_abort(
      c(
        "{.arg coding} must match input of {.arg {arg}}."
      ),
      call = call
    )
  }
}

coding_equals_length_list_members <- function(
    coding,
    list,
    code,
    arg = rlang::caller_arg(list),
    call = rlang::caller_env()) {
  length_coding <- sum(coding %in% code)
  length_list <- lengths(list)

  if (any(length_coding != length_list)) {
    cli::cli_abort(
      c(
        "{.arg coding} must match input of {.arg {arg}}."
      ),
      call = call
    )
  }
}

coding_zero <- function(list,
                        attribute,
                        call = rlang::caller_env()) {
  length_input <- lengths(list)

  if (length_input < 2) {
    cli::cli_abort(
      c(
        "If {.arg coding} is {.num 0}, {.arg attrib} must have a minimum of {.num 2} members.",
        "{.arg attrib} at position {.num {attribute}} only has length of {.num {length_input}}."
      ),
      call = call
    )
  }
}

coding_one <- function(list,
                       attribute,
                       call = rlang::caller_env()) {
  length_input <- lengths(list)

  if (length_input != 1) {
    cli::cli_abort(
      c(
        "If {.arg {coding}} is {.num 1}, {.arg attrib} must have {.num 1} member.",
        "{.arg attrib} at position {.num {attribute}} has length of {.num {length_input}}."
      ),
      call = call
    )
  }
}

coding_two <- function(list,
                       attribute,
                       call = rlang::caller_env()) {
  length_input <- lengths(list)

  if (length_input != 2) {
    cli::cli_abort(
      c(
        "If {.arg {coding}} is {.num 2}, {.arg attrib} must have {.num 2} members.",
        "{.arg attrib} at position {.num {attribute}} has length of {.num {length_input}}."
      ),
      call = call
    )
  }
}



number_piecewise <- function(coding,
                             call = rlang::caller_env()) {
  piecewise <- sum(coding == 2)

  if (piecewise > 1) {
    cli::cli_abort(
      c(
        "Only {.num 1} piecewise coded variable is allowed.",
        "{.num {piecewise}} piecewise coded attributes are provided to {.arg coding}."
      ),
      call = call
    )
  }
}

piecewise_coded <- function(piece.p,
                            call = rlang::caller_env()) {
  lengths_list <- lengths(unlist(piece.p, recursive = FALSE))

  if (!all(lengths_list == 2)) {
    cli::cli_abort(
      c(
        "Each list member in {.arg piece.p} can only have length {.num 2}."
      ),
      call = call
    )
  }
}

max_size <- function(data,
                     size,
                     opts,
                     call = rlang::caller_env()) {
  opts_ncol <- dplyr::select(data, {{ opts }}) %>%
    ncol()

  if (size > opts_ncol) {
    cli::cli_abort(
      c(
        "{.arg size} can not be larger than number specified in opts.",
        "{.arg size} is set to {.num {size}} while {.arg opts} is set to {.num {opts_ncol}}."
      ),
      call = call
    )
  }
}


vars_in_opts <- function(data,
                         vars,
                         opts,
                         arg_name,
                         call = rlang::caller_env()) {
  var_names <- dplyr::select(data, tidyselect::all_of(vars)) %>%
    colnames()

  opts_names <- dplyr::select(data, {{ opts }}) %>%
    colnames()

  if (!(all(var_names %in% opts_names))) {
    cli::cli_abort(
      c(
        "Variables specified in {.arg {arg_name}} have to be part of {.arg opts}."
      ),
      call = call
    )
  }
}

length_vector_list <- function(vector,
                               list,
                               arg1,
                               arg2,
                               call = rlang::caller_env()) {
  length_vector <- length(vector)
  length_list <- lengths(list)

  if (any(length_list != length_vector)) {
    cli::cli_abort(
      c(
        "{.arg {arg1}} must have the same length as {.arg {arg2}}."
      ),
      call = call
    )
  }
}

extrapolation_check <- function(value,
                                minimum,
                                maximum,
                                allowed,
                                call = rlang::caller_env()) {
  smaller <- value < minimum
  bigger <- value > maximum

  if (isFALSE(allowed) && any(c(smaller, bigger))) {
    cli::cli_abort(
      c(
        "Extrapolation, is not allowed for piecewise-coded variables.",
        "Your current value is {.num {value}} while {.arg interpolate.levels} ranges from {.num {minimum}} to {.num {maximum}}."
      )
    )
  }

  if (isTRUE(allowed) && any(c(smaller, bigger))) {
    cli::cli_warn(
      c(
        "Extrapolation, your input does not lie in the range of {.arg interpolate.levels}.",
        "Your current value is {.num {value}} while {.arg interpolate.levels} ranges from {.num {minimum}} to {.num {maximum}}."
      )
    )
  }
}

# end --------------------------------------------------------------------------

# helper functions -------------------------------------------------------------
colnames_match <- function(data, var1, var2) {
  none_name <- dplyr::select(data, {{ var1 }}) %>% colnames()

  choice_name <- dplyr::select(data, {{ var2 }}) %>% colnames()

  match(none_name, choice_name)
}

summed_range <- function(x) {
  max(x) - min(x)
}

percentage <- function(x) {
  x / sum(x)
}

kl_observed_predicted <- function(chosen, pred, log_base) {
  if (log_base == "log") {
    return(sum(chosen * log(chosen / pred)))
  }

  if (log_base == "log2") {
    return(sum(chosen * log2(chosen / pred)))
  }
}

kl_predicted_observed <- function(chosen, pred, log_base) {
  if (log_base == "log") {
    return(sum(pred * log(pred / chosen)))
  }

  if (log_base == "log2") {
    return(sum(pred * log2(pred / chosen)))
  }
}

mnl <- function(data, variables) {
  var_names <- dplyr::select(data, {{ variables }}) %>%
    colnames()

  data %>%
    dplyr::mutate(
      dplyr::across(
        tidyselect::all_of(var_names),
        function(x) exp(x) / rowSums(exp(.[var_names])) * 100
      )
    )
}

fc_share <- function(data, variables) {
  var_names <- dplyr::select(data, {{ variables }}) %>%
    colnames()

  data %>%
    dplyr::mutate(fc = apply(data[, var_names], 1, max)) %>%
    dplyr::mutate(
      dplyr::across({{ variables }}, \(x) ifelse(x == fc, 100, 0))
    ) %>%
    dplyr::select(-fc)
}

create_shares <- function(data, method, variables) {
  switch(method,
         "sop" = mnl(data, {{ variables }}),
         "fc" = fc_share(data, {{ variables }})
         )
}

sample_size <- function(data, group = NULL) {
  data %>%
    dplyr::group_by(dplyr::pick({{ group }})) %>%
    dplyr::count()
}

define_fctr_labels <- function(data, opts, prefix) {
  fctr_labels <- dplyr::select(data, {{ opts }}) %>% ncol()

  paste0(prefix, c(1:fctr_labels))
}

ch_probability <- function(data, items, set.size, anchor = NULL) {
  var_names <- dplyr::select(data, {{ items }}) %>%
    colnames()


  if (missing(anchor)) {
    data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_names),
        function(x) (exp(x) / (exp(x) + (set.size - 1)))
      )) %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_names),
        function(x) x / rowSums(.[var_names]) * 100
      ))
  } else {
    data %>%
      dplyr::mutate(dplyr::across(
        tidyselect::all_of(var_names),
        function(x) {
          (exp(x) / (exp(x) + (set.size - 1))) * 100 / (1 / set.size)
        }
      ))
  }
}

zero_anchor_utilities <- function(data, items, anchor = NULL) {
  var_names <- dplyr::select(data, {{ items }}) %>%
    colnames()

  data[, var_names] <- t(apply(data[, var_names], 1, \(x)
  scales::rescale(x, to = c(0, 100))))

  if (missing(anchor)) {
    data %>%
      dplyr::mutate(
        dplyr::across(
          {{ items }},
          \(x) x - rowMeans(.[var_names])
        )
      )
  } else {
    data %>%
      dplyr::mutate(
        dplyr::across(
          {{ items }},
          \(x) x - {{ anchor }}
        )
      )
  }
}

# end --------------------------------------------------------------------------
