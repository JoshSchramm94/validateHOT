% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/medae.R
\name{medae}
\alias{medae}
\title{Function to calculate median absolute error}
\usage{
medae(data, group, opts, choice)
}
\arguments{
\item{data}{A data frame with all relevant variables.}

\item{group}{Optional column name(s) to specify grouping variable(s)
to get \code{medae()} by group(s).}

\item{opts}{Column names of the alternatives included in the
validation task.}

\item{choice}{Column name of the actual choice in the validation
task.}
}
\value{
a tibble
}
\description{
\code{medae()} measures the median absolute error (MedAE) of a
validation task.
}
\details{
\code{medae()} calculates the deviation between predicted and
stated (actual) choice share and returns the median error in the
validation task, which is less likely to be influenced by outliers.

\code{data} a data.frame object including the alternatives shown in the
validation task. Can be created using the \code{create_hot()} function.

\code{group} optional grouping variable(s), if results should be displayed by
different groups. Has to be column name(s) of variables in \code{data}.

\code{opts} to specify the different alternatives in the validation task.
Input of \code{opts} has to be column names of variables in \code{data}.

\code{choice} to specify column of actual choice in the validation
task. Input of \code{choice} has to be column name of actual choice.
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

# medae - without group argument defined
medae(
  data = hot,
  opts = c(option_1:none),
  choice = choice
)

# medae - with group argument defined
medae(
  data = hot,
  opts = c(option_1:none),
  choice = choice,
  group = group
)

}
