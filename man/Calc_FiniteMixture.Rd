\name{calc_FiniteMixture}
\alias{calc_FiniteMixture}
\title{Apply the finite mixture model (FMM) after Galbraith (2005) to a given De distribution}
\description{This function fits a k-component mixture to a De distribution with 
differing known standard errors. Parameters (doses and mixing proportions)
are estimated by maximum likelihood assuming that the log dose estimates 
are from a mixture of normal distributions.}
\usage{calc_FiniteMixture(input.data, sigmab, n.components, sample.id = "unknown sample", 
    n.iterations = 200, grain.probability = FALSE, output.file = FALSE, 
    output.filename = "default")}
\arguments{
  \item{input.data}{\code{\link{data.frame}} (\bold{required}): two column data frame with De
values and corresponding De errors}
  \item{sigmab}{\code{\link{numeric}}  (\bold{required}): spread in De values given as a
fraction (e.g. 0.2). This value represents the expected overdispersion in
the data should the sample be well-bleached (Cunningham & Walling 2012, 
p. 100).}
  \item{n.components}{\code{\link{numeric}}  (\bold{required}): number of components to be 
fitted}
  \item{sample.id}{\code{\link{character}} (with default): sample id}
  \item{n.iterations}{\code{\link{numeric}}  (with default): number of iterations for maximum 
likelihood estimates}
  \item{grain.probability}{\code{\link{logical}} (with default): prints the estimated probabilities
of which component each grain is in}
  \item{output.file}{\code{\link{logical}} (with default): save results to file. See 
\code{output.filename}.}
  \item{output.filename}{\code{\link{character}} (with default): desired filename, else results 
are saved to default.res}
}
\details{This model uses the maximum likelihood and Bayesian Information Criterion 
(BIC) approaches. \cr\cr
Indications of overfitting are: \cr\cr
- increasing BIC \cr
- repeated dose estimates \cr
- covariance matrix not positive definite \cr
- convergence problems}
\value{Returns a terminal output and a file containing statistical results if 
wanted. In addition a list is returned containing the following elements:
\cr\cr
\code{mle.matrix} covariance matrix of maximum likelihood estimates.\cr
\code{grain.probability} matrix with estimated probabilities of which 
component each grain is in.\cr
\code{meta} data frame containing model parameters (sample.id, sigmab, 
n.components, llik, bic).\cr
\code{components} data frame containing fitted components.\cr
\code{single.comp} data frame containing log likelihood and BIC for a 
single component.}
\references{Galbraith, R.F. & Green, P.F., 1990. Estimating the component ages in a 
finite mixture. Nuclear Tracks and Radiation Measurements, 17, pp. 197-206.
\cr\cr
Galbraith, R.F. & Laslett, G.M., 1993. Statistical models for mixed fission
track ages. Nuclear Tracks Radiation Measurements, 4, pp. 459-470.\cr\cr
Galbraith, R.F. & Roberts, R.G., 2012. Statistical aspects of equivalent 
dose and error calculation and display in OSL dating: An overview and some
recommendations. Quaternary Geochronology, 11, pp. 1-27.\cr\cr
Roberts, R.G., Galbraith, R.F., Yoshida, H., Laslett, G.M. & Olley, J.M., 
2000. Distinguishing dose populations in sediment mixtures: a test of 
single-grain optical dating procedures using mixtures of laboratory-dosed
quartz. Radiation Measurements, 32, pp. 459-465.\cr\cr
Galbraith, R.F., 2005. Statistics for Fission Track Analysis, Chapman & 
Hall/CRC, Boca Raton.\cr\cr
\bold{Further reading}\cr\cr
Arnold, L.J. & Roberts, R.G., 2009. Stochastic modelling of multi-grain 
equivalent dose (De) distributions: Implications for OSL dating of sediment
mixtures. Quaternary Geochronology, 4, pp. 204-230.\cr\cr
Cunningham, A.C. & Wallinga, J., 2012. Realizing the potential of fluvial
archives using robust OSL chronologies. Quaternary Geochronology, 12, 
pp. 98-106.\cr\cr
Rodnight, H., Duller, G.A.T., Wintle, A.G. & Tooth, S., 2006. Assessing the
reproducibility and accuracy of optical dating of fluvial deposits. 
Quaternary Geochronology, 1, pp. 109-120.\cr\cr
Rodnight, H. 2008. How many equivalent dose values are needed to obtain a 
reproducible distribution?. Ancient TL, 26, pp. 3-10.}
\author{Christoph Burow, University of Cologne (Germany) \cr
Based on a rewritten S script of Rex Galbraith, 2006. \cr\cr
R Luminescence Package Team}



\seealso{\code{\link{calc_CentralDose}},
\code{\link{calc_CommonDose}}, \code{\link{calc_FuchsLang2001}},
\code{\link{calc_MinDose3}}, \code{\link{calc_MinDose4}}    }
\examples{
## load example data
data(ExampleData.DeValues, envir = environment())

## apply the finite mixture model
calc_FiniteMixture(ExampleData.DeValues,
                   sigmab = 0.08, n.components = 2,
                   grain.probability = TRUE, output.file = FALSE)
}
