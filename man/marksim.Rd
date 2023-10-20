% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/MarkSim.R
\name{marksim}
\alias{marksim}
\title{Market Simulation of Options included in HOT}
\usage{
marksim(data, group, opts, method = c("shareofpref", "firstchoice"))
}
\arguments{
\item{data}{A data frame with all relevant variables.}

\item{group}{Optional column name(s) to specify grouping variable(s)
to get \code{"marksim"} by group(s).}

\item{opts}{Column names of the alternatives included in the
validation/holdout task.}

\item{method}{Name of the market simulation method that should be conducted.
Either needs to be \code{method = "shareofpref"} to run share of preference
as method or \code{method = "firstchoice"} to run first choice rule. Default
set to \code{"shareofpref"}.}
}
\value{
a tibble
}
\description{
Function to run market simulations with options in a validation/holdout task.
}
\details{
Market simulation provides the expected aggregated market shares of each
 alternative in the validation/holdout task as well as its standard error
 and the lower and upper confidence interval which is calculated according
 to the following formula \eqn{mean +/- 1.96 x \frac{sd}{\sqrt(n)}}
 (Orme, 2020, p. 94). \code{method} can either be set to \code{method = "shareofpref"}
 to run share of preference rule or to \code{method = "firstchoice"} to run
 first choice rule to simulate market shares.

\code{data} has to be a data frame including the alternatives shown in
the validation/holdout task. Can be created using the \code{createHOT()}
function.

\code{group} optional grouping variable, if results should be displayed by
different groups. Has to be column name of variables in \code{data}.

\code{opts} is needed to specify the different alternatives in the simulation
task.
Input of \code{opts} has to be column names of variables in \code{data}.

\code{method} can either be set to \code{method = "shareofpref"} to run share of preference
as method or \code{method = "firstchoice"} to run first choice rule. Default
set to \code{method = "shareofpref"}.
}
\examples{
\dontrun{
HOT <- createHOT(
  data = MaxDiff,
  id = 1,
  none = 19,
  prod = 7,
  prod.levels = list(3, 10, 11, 15, 16, 17, 18),
  method = "MaxDiff",
  choice = 20,
  varskeep = 21
)

# marksim ungrouped share of preference
marksim(
  data = HOT,
  opts = c(Option_1:None),
  method = "shareofpref"
)

# marksim ungrouped first choice
marksim(
  data = HOT,
  opts = c(Option_1:None),
  method = "firstchoice"
)

# marksim grouped share of preference
marksim(
  data = HOT,
  opts = c(Option_1:None),
  group = Group
)

# marksim grouped first choice
marksim(
  data = HOT,
  opts = c(Option_1:None),
  group = Group
)
}

}
\references{
{

Orme, B. K. (2020). \emph{Getting Started with Conjoint Analysis:
Strategies for Product Design and Pricing Research}. 4th edition.
Manhattan Beach, CA: Research Publishers LLC.

}
}