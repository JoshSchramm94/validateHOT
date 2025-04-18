% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmse.R
\name{rmse}
\alias{rmse}
\title{Function to calculate Root Mean Square Error of validation task}
\usage{
rmse(data, group, opts, choice)
}
\arguments{
\item{data}{A data frame containing all relevant variables.}

\item{group}{Optional column name(s) to specify grouping variable(s)
to calculate \code{rmse()} by group(s).}

\item{opts}{Column names of the alternatives included in the
validation task.}

\item{choice}{Column name of the actual choice in the validation
task.}
}
\value{
a tibble
}
\description{
\code{rmse()} calculates the root mean square error of a validation task.
Calculates the averaged root mean square error of the stated and predicted
share of alternatives in the validation task.
}
\details{
Root mean square error (RMSE) calculates the root mean square error when
comparing the share of the actual choice in the holdout task and the
predicted share.

\code{data} a \code{data.frame} object including the alternatives shown in the
validation task. It can be created using the \code{create_hot()} function.

\code{group} optional grouping variable(s) to display results by group(s).
Has to be the column name(s) of variables in \code{data}.

\code{opts} to specify the different alternatives in the
validation task (also includes the \code{none} alternative).

\code{choice} to specify the column of the actual choice in the validation
task. The input of \code{choice} has to be the column name of the actual choice.
}
\examples{

hot <- create_hot(
  data = maxdiff,
  id = "id",
  none = "none",
  prod.levels = list(2, 9, 10, 14, 15, 16, 17),
  method = "maxdiff",
  choice = "hot",
  varskeep = "group"
)

# rmse - without group argument defined
rmse(
  data = hot,
  opts = c(option_1:none),
  choice = choice
)

# rmse - with group argument defined
rmse(
  data = hot,
  opts = c(option_1:none),
  choice = choice,
  group = group
)

}
