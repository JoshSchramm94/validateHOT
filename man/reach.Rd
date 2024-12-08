% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Reach.R
\name{reach}
\alias{reach}
\title{Function to calculate percentage of participants that are reached by
assortment}
\usage{
reach(data, group, none, opts)
}
\arguments{
\item{data}{A data frame with all relevant variables.}

\item{group}{Optional column name(s) to specify grouping variable(s)
to get `reach()` by group(s).}

\item{none}{Column name of none / threshold alternative.}

\item{opts}{Column names of the alternatives included in the assortment.}
}
\value{
a tibble
}
\description{
`reach()` of T(otal) U(nduplicated) R(each) and F(requency)
analysis to measure the number of  the averaged percentage of how
many participants you can reach (at least one of the products resemble
a purchase option) is reached with a specific product bundle assortment.
}
\details{
`reach()` calculates the the percentage of consumers that are
reached with a particular product assortment. The current
logic of `reach()` is that the utility of an alternative has to
exceed a threshold. In the case of `reach()` this threshold is referred
to the `none` argument in `data`, however, could also be, e.g., a
current product.


`data` has to be a data frame including the alternatives that
should be tested.

`group` optional grouping variable, if results should be displayed
by different groups. Has to be column name of variables in `data`.

`opts` specifies the different alternatives in the
product assortment that should be considered.
Input of `opts` has to be column names of variables in `data`.

`none` to specify column name of the `none` alternative (i.e.,
threshold variable).
}
\examples{

library(validateHOT)

HOT <- createHOT(
  data = MaxDiff,
  id = 1,
  none = 19,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  method = "MaxDiff",
  choice = 20, varskeep = 21
)

# reach - without group argument defined
reach(
  data = HOT,
  opts = c(Option_1, Option_2, Option_6),
  none = None
)

# reach - with group argument defined
reach(
  data = HOT,
  opts = c(Option_1, Option_2, Option_6),
  none = None,
  group = Group
)

}
