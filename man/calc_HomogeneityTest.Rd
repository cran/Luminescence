\name{calc_HomogeneityTest}
\alias{calc_HomogeneityTest}
\title{Apply a simple homogeneity test after Galbraith (2003)}
\description{A simple homogeneity test for De estimates}
\usage{calc_HomogeneityTest(input.data, log = TRUE, sample.id = "unknown sample")}
\arguments{
  \item{input.data}{\code{\link{data.frame}} (\bold{required}): two column data frame with De
values and corresponding De errors}
  \item{log}{\code{\link{logical}} (with default): peform the homogeniety test with
(un-)logged data}
  \item{sample.id}{\code{\link{character}} (with default): sample id}
}
\details{For details see Galbraith (2003).}
\value{Returns a terminal output. In addition a list is returned containing the
following element: \cr\cr
\code{results} data frame with statistical parameters.}
\references{Galbraith, R.F., 2003. A simple homogeneity test for estimates of dose
obtained using OSL. Ancient TL, 21, pp. 75-77.}
\author{Christoph Burow, University of Cologne (Germany) \cr
R Luminescence Package Team}



\seealso{\code{\link{pchisq}}}
\examples{
## load example data
data(ExampleData.DeValues, envir = environment())

## apply the common dose model
calc_HomogeneityTest(ExampleData.DeValues)
}
