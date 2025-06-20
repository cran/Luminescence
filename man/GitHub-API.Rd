% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/github.R
\name{GitHub-API}
\alias{GitHub-API}
\alias{github_commits}
\alias{github_branches}
\alias{github_issues}
\title{GitHub API}
\usage{
github_commits(user = "r-lum", repo = "luminescence", branch = "master", n = 5)

github_branches(user = "r-lum", repo = "luminescence")

github_issues(user = "r-lum", repo = "luminescence", verbose = TRUE)
}
\arguments{
\item{user}{\link{character} (\emph{with default}):
GitHub user name (defaults to \code{'r-lum'}).}

\item{repo}{\link{character} (\emph{with default}):
name of a GitHub repository (defaults to \code{'luminescence'}).}

\item{branch}{\link{character} (\emph{with default}):
branch of a GitHub repository (defaults to \code{'master'}).}

\item{n}{\link{integer} (\emph{with default}):
number of commits returned (defaults to 5).}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}
}
\value{
\code{github_commits}: \link{data.frame} with columns:

\tabular{ll}{
\verb{[ ,1]} \tab SHA \cr
\verb{[ ,2]} \tab AUTHOR \cr
\verb{[ ,3]} \tab DATE \cr
\verb{[ ,4]} \tab MESSAGE \cr
}

\code{github_branches}: \link{data.frame} with columns:

\tabular{ll}{
\verb{[ ,1]} \tab BRANCH \cr
\verb{[ ,2]} \tab SHA \cr
\verb{[ ,3]} \tab INSTALL \cr
}

\code{github_commits}: Nested \link{list} with \code{n} elements.
Each commit element is a list with elements:

\tabular{ll}{
\verb{[[1]]} \tab NUMBER \cr
\verb{[[2]]} \tab TITLE \cr
\verb{[[3]]} \tab BODY \cr
\verb{[[4]]} \tab CREATED \cr
\verb{[[5]]} \tab UPDATED \cr
\verb{[[6]]} \tab CREATOR \cr
\verb{[[7]]} \tab URL \cr
\verb{[[8]]} \tab STATUS \cr
}
}
\description{
R Interface to the GitHub API v3.
}
\details{
These functions can be used to query a specific repository hosted on GitHub. \cr

\code{github_commits} lists the most recent \code{n} commits of a specific branch of a repository.

\code{github_branches} can be used to list all current branches of a
repository and returns the corresponding SHA hash as well as an installation
command to install the branch in R via the 'devtools' package.

\code{github_issues} lists all open issues for a repository in valid YAML.
}
\section{Function version}{
 0.1.0
}

\examples{

\dontrun{
github_branches(user = "r-lum", repo = "luminescence")
github_issues(user = "r-lum", repo = "luminescence")
github_commits(user = "r-lum", repo = "luminescence", branch = "master", n = 10)
}

} 

\section{How to cite}{
Burow, C., 2025. GitHub-API(): GitHub API. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
GitHub Developer API v3. \url{https://docs.github.com/v3/}, last accessed: 10/01/2017.
}
\author{
Christoph Burow, University of Cologne (Germany)
, RLum Developer Team}
