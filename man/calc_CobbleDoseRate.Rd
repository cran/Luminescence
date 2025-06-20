% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calc_CobbleDoseRate.R
\name{calc_CobbleDoseRate}
\alias{calc_CobbleDoseRate}
\title{Calculate dose rate of slices in a spherical cobble}
\usage{
calc_CobbleDoseRate(input, conversion = "Guerinetal2011")
}
\arguments{
\item{input}{\link{data.frame} (\strong{required}): A table containing all relevant information
for each individual layer. For the table layout see details.}

\item{conversion}{\link{character} (\emph{with default}): dose rate conversion factors
to use, see \link{BaseDataSet.ConversionFactors} for the accepted values.}
}
\value{
The function returns an \linkS4class{RLum.Results} object for which the first element
is a \link{matrix} (\code{DataIndividual}) that gives the dose rate results for each slice
for each decay chain individually, for both, the cobble dose rate and the sediment
dose rate. The second element is also a \link{matrix} (\code{DataComponent}) that gives
the total beta and gamma-dose rates for the cobble and the adjacent sediment
for each slice of the cobble.
}
\description{
Calculates the dose rate profile through the cobble based on Riedesel and Autzen (2020).

Corrects the beta dose rate in the cobble for the grain size following results
of Guérin et al. (2012). Sediment beta and gamma dose rates are corrected
for the water content of the sediment using the correction factors of Aitken (1985).
Water content in the cobble is assumed to be 0.
}
\details{
\strong{The input table layout}

\tabular{lll}{
COLUMN \tab DATA TYPE \tab DESCRIPTION\cr
\code{Distance} \tab \code{numeric} \tab distance from the surface of the cobble to the top of each rock slice in mm. The distance for each slice will be listed in this column\cr
\code{DistanceError} \tab \code{numeric} \tab Error on the distance in mm\cr
\code{Thickness} \tab \code{numeric} \tab Thickness of each slice in mm\cr
\code{ThicknessError} \tab \code{numeric} \tab uncertainty of the thickness in mm.\cr
\code{Mineral} \tab \code{character} \tab \code{'FS'} for feldspar, \code{'Q'} for quartz, depending which mineral in the cobble is used for dating\cr
\code{Cobble_K} \tab \code{numeric} \tab K nuclide content in \% of the bulk cobble\cr
\code{Cobble_K_SE} \tab \code{numeric} \tab error on K nuclide content in \% of the bulk cobble\cr
\code{Cobble_Th} \tab \code{numeric} \tab Th nuclide content in ppm of the bulk cobble\cr
\code{Cobble_Th_SE} \tab \code{numeric} \tab error on Th nuclide content in ppm of the bulk cobble\cr
\code{Cobble_U} \tab \code{numeric} \tab U nuclide content in ppm of the bulk cobble\cr
\code{Cobble_U_SE} \tab \code{numeric} \tab error on U nuclide content in ppm of the bulk cobble\cr
\code{GrainSize} \tab \code{numeric} \tab average grain size in µm of the grains used for dating\cr
\code{Density} \tab \code{numeric} \tab Density of the cobble. Default is 2.7 g cm^-3\cr
\code{CobbleDiameter} \tab \code{numeric} \tab Diameter of the cobble in cm.\cr
\code{Sed_K} \tab \code{numeric} \tab K nuclide content in \% of the sediment matrix\cr
\code{Sed_K_SE} \tab \code{numeric} \tab error on K nuclide content in \% of the sediment matrix\cr
\code{Sed_Th} \tab \code{numeric} \tab Th nuclide content in ppm of the sediment matrix\cr
\code{Sed_Th_SE} \tab \code{numeric} \tab error on Th nuclide content in ppm of the sediment matrix\cr
\code{Sed_U} \tab \code{numeric} \tab U nuclide content in ppm of the sediment matrix\cr
\code{Sed_U_SE} \tab \code{numeric} \tab error on U nuclide content in ppm of the sediment matrix\cr
\code{GrainSize_Sed} \tab \code{numeric} \tab average grain size of the sediment matrix\cr
\code{Density_Sed} \tab \code{numeric} \tab average density of the sediment matrix\cr
\code{WaterContent} \tab \code{numeric} \tab mean water content of the sediment matrix in \%\cr
\code{WaterContent_SE} \tab \code{numeric} \tab relative error on water content
}

\strong{Water content}
The water content provided by the user should be calculated according to:

\deqn{(Wet\_weight - Dry\_weight) / Dry\_weight * 100}
}
\section{Function version}{
 0.1.0
}

\examples{
## load example data
data("ExampleData.CobbleData", envir = environment())

## run function
calc_CobbleDoseRate(ExampleData.CobbleData)

} 

\section{How to cite}{
Riedesel, S., Autzen, M., 2025. calc_CobbleDoseRate(): Calculate dose rate of slices in a spherical cobble. Function version 0.1.0. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
Riedesel, S., Autzen, M., 2020. Beta and gamma dose rate attenuation in rocks and sediment.
Radiation Measurements 133, 106295.
}
\seealso{
\link{convert_Concentration2DoseRate}
}
\author{
Svenja Riedesel, Aberystwyth University (United Kingdom) \cr
Martin Autzen, DTU NUTECH Center for Nuclear Technologies (Denmark)
, RLum Developer Team}
\keyword{datagen}
