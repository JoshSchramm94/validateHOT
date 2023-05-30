#' Kullback-Leibler Divergence
#'
#' @description Function to measure the Kullback-Leibler Divergence of a validation task
#' @param data a data frame
#' @param id column index of the \code{id} variable
#' @param Group optional grouping variable to get hit rate by group
#' @param opts column indexes of the options included in the holdout task
#' @param choice column index of the actual choice
#' @param epsilon noise that should be added to 0 values, per default set to 1e-05
#'
#' @return a data frame
#' @importFrom dplyr group_by summarise
#' @importFrom magrittr "%>%"
#' @importFrom labelled is.labelled val_labels
#'
#' @details
#' Additional details...
#'
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff")
#' kl(data = HOT, id = 1, opts = c(2:9), choice = 10)
#'
#'
#' @examples
#' library(ValiDatHOT)
#' data(MaxDiff)
#' createHOT(data = MaxDiff, None = 19, id = 1,
#'           prod = 7, x = list(3, 10, 11, 15, 16, 17, 18),
#'           choice = 20, method = "MaxDiff", varskeep = 21)
#' kl(data = HOT, id = 1, opts = c(2:9), choice = 11, Group = 10)
#'
#' @export

kl <- function(data, id, Group = NULL, opts, choice, epsilon=NULL) {

  if (base::is.null(epsilon)){
    epsilon = .00001
  }

  if (!base::is.integer(data[[choice]]) & !base::is.numeric(data[[choice]])){
    base::stop("Error: Choice must be numeric!")
  }

  if (!base::is.null(Group) & base::anyNA(data[Group])){
    base::warning("Warning: Grouping variable contains NAs.")
  }

  WS <- data[, c(id, Group, choice, opts)]

  Count <- NULL

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

    Helper <- base::as.data.frame(base::matrix(nrow = base::length(3:(base::ncol(HOT) - 1)), ncol = 1))

    base::colnames(Helper) <- "Options"

    Helper$Options <- c(1:base::length(3:(base::ncol(HOT) - 1)))

    Actual <- HOT %>%
      dplyr::group_by(choice) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Share = Count / base::sum(Count)) %>%
      base::as.data.frame()

    Predicted <- HOT %>%
      dplyr::group_by(pred) %>%
      dplyr::summarise(Count = dplyr::n()) %>%
      dplyr::ungroup() %>%
      dplyr::mutate(Predicted = Count / base::sum(Count)) %>%
      base::as.data.frame()


    KL <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

    KL <- base::merge(x = KL, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

    KL[base::is.na(KL)] <- epsilon

    i <- 1:base::length(Helper$Options)

    base::sum(KL[i, 2] * base::log2(KL[i, 2] / KL[i, 3]))

    Res <- base::as.data.frame(base::cbind(base::sum(KL[i, 2] * base::log2(KL[i, 2] / KL[i, 3])), base::sum(KL[i, 3] * base::log2(KL[i, 3] / KL[i, 2]))))

    colnames(Res) <- c("KL_O_P", "KL_P_O")

    return(Res)
  }

  if (!(base::is.null(Group))) {
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

    KL <- base::data.frame(Group = base::character(base::length(base::unique(HOT$Group)) + 1), KL_O_P = base::numeric(base::length(base::unique(HOT$Group)) + 1), KL_P_O = base::numeric(base::length(base::unique(HOT$Group)) + 1))

    for (p in 1:base::length(base::unique(HOT$Group))) {
      if (p == 1) {
        Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(HOT) - 1)), ncol = 1))

        base::colnames(Helper) <- "Options"

        Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))

        Actual <- HOT %>%
          dplyr::group_by(choice) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Share = Count / sum(Count)) %>%
          base::as.data.frame()

        Predicted <- HOT %>%
          dplyr::group_by(pred) %>%
          dplyr::summarise(Count = dplyr::n()) %>%
          dplyr::ungroup() %>%
          dplyr::mutate(Predicted = Count / base::sum(Count)) %>%
          base::as.data.frame()

        DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

        DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

        i <- 1:base::length(Helper$Options)

        KL[p, p] <- "All"

        KL[p, (p + 1)] <- base::sum(DataFrame[i, 2] * base::log2(DataFrame[i, 2] / DataFrame[i, 3]))
        KL[p, (p + 2)] <- base::sum(DataFrame[i, 3] * base::log2(DataFrame[i, 3] / DataFrame[i, 2]))

        base::rm(Helper, Actual, Predicted, DataFrame)
      }

      if (base::is.numeric(WS$Group) & !labelled::is.labelled(WS$Group)){
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))){

          lab_num <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_num[i])

        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.character(WS$Group)){
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))){

          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])

        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.character(WS$Group)){
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))){

          lab_char <- base::sort(base::unique(WS$Group))

          lab <- c(lab, lab_char[i])

        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (base::is.factor(WS$Group)){
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))){

          lab_fac <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::levels(lab_fac)[i])

        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      if (labelled::is.labelled(WS$Group)){
        lab <- "All"
        for (i in 1:base::length(base::unique(WS$Group))){

          lab_lab <- base::sort(base::unique(WS$Group))

          lab <- c(lab, base::names(labelled::val_labels(lab_lab))[i])

        }

        Group <- base::subset(HOT, Group == base::sort(base::unique(WS$Group))[p])
      }

      Helper <- base::as.data.frame(base::matrix(nrow = base::length(4:(base::ncol(Group) - 1)), ncol = 1))

      base::colnames(Helper) <- "Options"

      Helper$Options <- c(1:base::length(4:(base::ncol(HOT) - 1)))

      Actual <- Group %>%
        dplyr::group_by(choice) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Share = Count / base::sum(Count)) %>%
        base::as.data.frame()

      Predicted <- Group %>%
        dplyr::group_by(pred) %>%
        dplyr::summarise(Count = dplyr::n()) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(Predicted = Count / base::sum(Count)) %>%
        base::as.data.frame()

      DataFrame <- base::merge(x = Helper, y = Actual[, c("choice", "Share")], by.x = "Options", by.y = "choice", all.x = T)

      DataFrame <- base::merge(x = DataFrame, y = Predicted[, c("pred", "Predicted")], by.x = "Options", by.y = "pred", all.x = T)

      DataFrame[base::is.na(DataFrame)] <- epsilon

      i <- 1:base::length(Helper$Options)

      KL[(p + 1), 1] <- lab[(p + 1)]

      KL[(p + 1), 2] <- base::sum(DataFrame[i, 2] * base::log2(DataFrame[i, 2] / DataFrame[i, 3]))
      KL[(p + 1), 3] <- base::sum(DataFrame[i, 3] * base::log2(DataFrame[i, 3] / DataFrame[i, 2]))

      base::rm(Helper, Actual, Predicted, DataFrame)

      if (p == base::max(base::length(base::unique(HOT$Group)))) {
        return(KL)
      }
    }
  }
}