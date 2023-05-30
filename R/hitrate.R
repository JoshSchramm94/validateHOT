#' Hit Rate
#'
#' @description \code{hitrate} measures number of times a choice was correctly predicted in a validation task.
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the validation task
#' @param choice column index of the actual choice
#'
#' @details
#' Output contains the chance level (\eqn{1 / # of alternatives in validation task}), the number of correct predictions and the percentage of number of correct predictions.
#'
#'
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff")
#' hitrate(data = HOT, id = 1, opts = c(2:9), choice = 10)
#'
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff", varskeep = 21)
#' hitrate(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#'
#' @return a data frame
#' @importFrom dplyr group_by summarise mutate
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @export

hitrate <- function(data, id, Group = NULL, opts, choice) {

  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])){
    base::stop("Error: Choice must be numeric!")
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])){
    base::warning("Warning: Grouping variable contains NAs.")
  }

  WS <- data[, c(id, Group, choice, opts)]

  pred <- NULL

  if (base::is.null(Group)) {
    Options <- c()

    for (k in 1:base::length(opts)) {
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", "choice", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 3:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 3):(base::length(opts) + base::length(opts) + 2)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 3):(base::length(opts) + base::length(opts) + 2)])) * 100
    }


    HOT <- WS[, c("id", "choice", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)) {
      for (k in 3:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 3:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 2
        }
      }
    }


    HR <- base::as.data.frame(rbind((1 / length(opts) * 100),
                                    base::sum(base::as.integer(HOT$choice == HOT$pred)),
                                    (base::sum(base::as.integer(HOT$choice == HOT$pred)) / base::nrow(HOT) * 100)))

    base::row.names(HR) <- c("chance", "no.", "%")
    base::colnames(HR) <- "hitrate"

    return(HR)

  }

  if (!(base::is.null(Group))) {

    pred <- NULL

    Options <- c()

    for (k in 1:base::length(opts)) {
      name <- base::paste0("Option_", k)
      Options <- c(Options, name)
    }

    newNames <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Opt_", k)
      newNames <- c(newNames, name)
    }

    Perc <- c()
    for (k in 1:base::length(opts)) {
      name <- base::paste0("Perc_", k)
      Perc <- c(Perc, name)
    }

    base::colnames(WS) <- c("id", "Group", "choice", Options)

    for (i in 1:base::length(newNames)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- newNames[i]
    }

    for (i in 4:(base::ncol(WS) - base::length(opts))) {
      WS[, (base::length(opts) + i)] <- base::exp(WS[i])
    }

    for (i in 1:base::length(Perc)) {
      WS[, base::ncol(WS) + 1] <- 0
      base::colnames(WS)[base::ncol(WS)] <- Perc[i]
    }

    for (i in (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)) {
      WS[, (base::length(opts) + i)] <- (WS[i] / base::rowSums(WS[, (base::length(opts) + 4):(base::length(opts) + base::length(opts) + 3)])) * 100
    }


    HOT <- WS[, c("id", "choice", "Group", Perc)]

    HOT$pred <- 0

    for (i in 1:base::nrow(HOT)) {
      for (k in 4:(base::ncol(HOT) - 1)) {
        if (HOT[i, k] == base::max(HOT[i, 4:(base::ncol(HOT) - 1)])) {
          HOT$pred[i] <- k - 3
        }
      }
    }



    HR <- base::rbind(HOT %>%
             dplyr::summarise(Group = "All",
               no. = base::sum(base::as.integer(choice == pred)),
                              perc. = base::mean(base::as.integer(choice == pred) * 100)) %>%
               dplyr::mutate(chance = (1 / base::length(opts) * 100)) %>%
               base::as.data.frame(),
             HOT %>%
      dplyr::group_by(Group) %>%
      dplyr::summarise(no. = base::sum(base::as.integer(choice == pred)),
                       perc. = base::mean(base::as.integer(choice == pred) * 100)) %>%
      dplyr::mutate(chance = (1 / base::length(opts) * 100)) %>%
      base::as.data.frame())

    # fixing grouping variable

    lab <- c()

    if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_num <- base::sort(base::unique(WS$Group))

        lab <- c(lab, lab_num[i])

      }
    }

    if (base::is.character(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_char <- base::sort(base::unique(WS$Group))

        lab <- c(lab, lab_char[i])

      }
    }


    if (base::is.factor(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_fac <- base::sort(base::unique(WS$Group))

        lab <- c(lab, base::levels(lab_fac)[i])

      }
    }

    if (labelled::is.labelled(WS$Group)){
      lab <- "All"
      for (i in 1:base::length(base::unique(WS$Group))){

        lab_lab <- base::sort(base::unique(WS$Group))

        lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])

      }
    }


    HR$Group <- lab

    return(HR)

  }
}