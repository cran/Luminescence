\name{readXSYG2R}
\alias{readXSYG2R}
\title{Import XSYG files to R}
\description{Import XSYG files produced by a Freiberg Instrument's lexsyg reader into R}
\usage{readXSYG2R(file, import = TRUE, txtProgressBar = TRUE)}
\arguments{
  \item{file}{\link{character} (\bold{required}): path and file name of the XSYG file}
  \item{import}{\link{logical} (with default): if set to \code{FALSE} only the XSYG file structure
is shown}
  \item{txtProgressBar}{\link{logical} (with default): enables \code{TRUE} or disables \code{FALSE}}
}
\details{\bold{How the import functions works?}\cr\cr
The function uses the \code{\link{xml}} package to parse the file structure.
Each sequence is subsequently translated into an \code{\linkS4class{RLum.Analysis}}
object.\cr\cr

\bold{General structure XSYG format}\cr\cr
\code{<?xml?}\cr
\code{ <Sample>}\cr
\code{  <Sequence>}\cr
\code{   <Record>}\cr
\code{    <Curve name="first curve" />}\cr
\code{     <Curve name="curve with data">}\cr
\code{      x0 , y0 ; x1 , y1 ; x2 , y2 ; x3 , y3}\cr
\code{     </Curve>}\cr
\code{   </Record>}\cr
\code{  </Sequence>}\cr
\code{ </Sample>}\cr\cr
So far each XSYG file can only contain one \code{<Sample></Sample>}, but 
multiple sequences. \cr\cr
Each record may comprises several curves.}
\value{\bold{Using the option \code{import = FALSE}}\cr\cr
A list consisting of two elements is shown 
\item{Sample}{\link{data.frame} with information on file.}
\item{Sequences}{\link{data.frame} with information on the sequences 
stored in the XSYG file}\cr\cr
\bold{Using the option \code{import = TRUE} (default)} \cr\cr
A list is provided, the list elements contain:
\item{Sequence.Header}{\link{data.frame} with information on the sequence.}
\item{Sequence.Object}{\code{\linkS4class{RLum.Analysis}} containing the curves}}
\references{Grehl, S., Kreutzer, S., Hoehne, M., 2013. Documentation of the XSYG file format.
Unpublished Technical Note. Freiberg, Germany \cr\cr
\bold{Further reading} \cr\cr
XML: \url{http://en.wikipedia.org/wiki/XML}}
\author{Sebastian Kreutzer, JLU Giessen (Germany)
R Luminescence Package Team}
\note{This function is a beta version as the XSYG file format is not yet fully
specified. 
Thus further file operations (merge, export, write) should be done using the functions
provided with the package \code{\link{xml}}}


\seealso{\code{\link{xml}}, \code{\linkS4class{RLum.Analysis}}, 
\code{\linkS4class{RLum.Data.Curve}}}
\examples{

##Import XSYG file
##uncomment for usage
#temp <- readXSYG2R("input_file.xsyg")

##Additional examples for pure XML import using the package XML
##uncomment for usage

  ##import entire file
  #temp <- xmlRoot(xmlTreeParse("input_file.xsyg"))
  
  ##search for specific subnodes with curves containing 'OSL'
  #getNodeSet(temp, "//Sample/Sequence/Record[@recordType = 'OSL']/Curve")
  
}

\keyword{IO}
