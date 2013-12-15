\name{calc_AliquotSize}
\alias{calc_AliquotSize}
\title{Estimate the amount of grains on an aliquot}
\description{Estimate the number of grains on an aliquot. Alternatively, the packing
density of an aliquot is computed.}
\usage{calc_AliquotSize(grain.size, sample.diameter, packing.density = 0.65, 
    grains.counted)}
\arguments{
  \item{grain.size}{\code{\link{numeric}} (\bold{required}): mean grain size (microns) or a 
range of grain sizes from which the mean grain size is computed 
(e.g. \code{c(100,200)}).}
  \item{sample.diameter}{\code{\link{numeric}} (\bold{required}): diameter (mm) of the targeted
area on the sample carrier.}
  \item{packing.density}{\code{\link{numeric}} (with default) empirical value for mean packing 
density. \cr 
If \code{packing.density = "inf"} a hexagonal structure on an
infinite plane with a packing density of \eqn{0.906\ldots} is assumed.}
  \item{grains.counted}{\code{\link{numeric}} (optional) grains counted on a sample carrier. If
a non-zero positive integer is provided this function will calculate the 
packing density of the aliquot.
If more than one value is provided the mean packing density and its 
standard deviation is calculated. Note that this overrides
\code{packing.density}.}
}
\details{This function can be used to either estimate the number of grains
on an aliquot or to compute the packing density depending on
the arguments provided. \cr
The following function is used to estimate the number of grains \code{n}:
\cr
\deqn{n = (\pi*x^2)/(\pi*y^2)*d}
where \code{x} is the radius of the aliquot size (microns), \code{y}
is the mean radius of the mineral grains (mm) and \code{d} is the packing
density (value between 0 and 1). \cr\cr
\bold{Packing density} \cr\cr
The default value for \code{packing.density} is 0.65, which is the mean of
empirical values determined by Heer et al. 2012 and unpublished data from
the cologne luminescence laboratory. If \code{packing.density = "inf"}
a maximum density of \eqn{\pi/\sqrt12 = 0.9068\ldots} is used. However,
note that this value is not appropriate as the standard preparation
procedure of aliquots resembles a PECC ("Packing Equal Circles in a 
Circle") problem. }
\value{Returns terminal output. In addition a list is returned containing 
the following element:

\item{results}{data frame with calculation results.}}
\references{Duller, G.A.T., 2008. Single-grain optical dating of Quaternary sediments: 
why aliquot size matters in luminescence dating. Boreas 37, pp. 589-612. 
\cr\cr
Heer, A.J., Adamiec, G., Moska, P., 2012. How many grains are there on a 
single aliquot?. Ancient TL, 30, pp. 9-16. \cr\cr
\bold{Further reading} \cr\cr
Chang, H.-C., Wang, L.-C., 2010. A simple proof of Thue's Theorem on 
Circle Packing. \url{http://arxiv.org/pdf/1009.4322v1.pdf}, 2013-09-13.
\cr\cr
Graham, R.L., Lubachevsky, B.D., Nurmela, K.J., Oestergard, P.R.J., 1998. 
Dense packings of congruent circles in a circle. Discrete Mathematics, 181, 
pp. 139-154. \cr\cr
Huang, W., Ye, T., 2011. Global optimization method for finding dense 
packings of equal circles in a circle. European Journal of Operational 
Research, 210, pp. 474-481. }
\author{Christoph Burow, University of Cologne (Germany) \cr
R Luminescence Package Team}




\examples{
## Estimate the amount of grains on a small aliquot
calc_AliquotSize(grain.size = 125, sample.diameter = 1) 

## Calculate the mean packing density of large aliquots
calc_AliquotSize(grain.size = 125, sample.diameter = 8, 
                 grains.counted = c(2525,2312,2880))

}
