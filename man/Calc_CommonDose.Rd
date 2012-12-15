\name{Calc_CommonDose}
\alias{Calc_CommonDose}
\title{
Apply the (un-)logged common age model after Galbraith et al. (1999) to a given De distribution
}
\description{
Function to calculate the common dose of a De distribution.
}
\usage{
Calc_CommonDose(input.data,
                sigmab = 0,
                log = TRUE,
                sample.id = "unknown sample")
}
\arguments{
  \item{input.data}{\link{data.frame} (\bold{required}): two column data frame with De values and corresponding De errors
}
  \item{sigmab}{\link{numeric} (with default): spread in De values given as a fraction (e.g. 0.2). This value represents the expected overdispersion in the data should the sample be well-bleached (Cunningham & Walling 2012, p. 100).
}
  \item{log}{\link{logical} (with default): fit the (un-)logged common age model to De data
}
  \item{sample.id}{\link{character} (with default): sample id
}
}
\details{

 \bold{(Un-)logged model}

When \code{log = TRUE} this function calculates the weighted mean of logarithmic De values. Each of the estimates is weighted by the inverse square of its relative standard error. The weighted mean is then transformed back to the dose scale (Galbraith & Roberts 2012, p. 14). 

The log transformation is not applicable if the De estimates are close to zero or negative. In this case the un-logged model can be applied instead (\code{log = FALSE}). The weighted mean is then calculated using the un-logged estimates of De and their absolute standard error (Galbraith & Roberts 2012, p. 14).

}
\value{
Returns a terminal output. In addition a list is returned containing the following element:

\item{results}{data frame with statistical parameters.}

}
\references{
Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470.

Galbraith, R.F., Roberts, R.G., Laslett, G.M., Yoshida, H. & Olley, J.M., 1999. Optical dating of single grains of quartz from Jinmium rock shelter, northern Australia. Part I: experimental design and statistical models. Archaeometry, 41, pp. 339-364. 

Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent dose and error calculation and display in OSL dating: An overview and some recommendations. Quaternary Geochronology, 11, pp. 1-27.

\bold{Further reading}

Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain equivalent dose (De) distributions: Implications for OSL dating of sediment mixtures. Quaternary Geochronology, 4, pp. 204-230.

Bailey, R.M. & Arnold, L.J., 2006. Statistical modelling of single grain quartz De distributions and an assessment of procedures for estimating burial dose. Quaternary Science Reviews, 25, pp. 2475-2502.

Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial archives using robust OSL chronologies. Quaternary Geochronology, 12, pp. 98-106.

Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the reproducibility and accuracy of optical dating of fluvial deposits. Quaternary Geochronology, 1, pp. 109-120. 

Rodnight, H., 2008. How many equivalent dose values are needed to obtain a reproducible distribution?. Ancient TL, 26, pp. 3-10.
}
\author{
Original S script: Rex Galbraith, University College London (UK), 2010

Revised R script: Christoph Burow, University of Cologne (Germany), 2012
}
\seealso{
  \link{Calc_CentralDose},
  \link{Calc_FiniteMixture},
  \link{Calc_FuchsLang2001},
  \link{Calc_MinDose3},
  \link{Calc_MinDose4}
}
\examples{
## load example data
data(ExampleData.DeValues)

## apply the common dose model
Calc_CommonDose(ExampleData.DeValues)
}
