\name{merge_Risoe.BINfileData}
\alias{merge_Risoe.BINfileData}
\title{Merge Risoe.BINfileData objects or Risoe BIN-files}
\description{Function allows merging Risoe BIN files or Risoe.BINfileData objects.}
\usage{merge_Risoe.BINfileData(input.objects, output.file, keep.position.number = FALSE)}
\arguments{
  \item{input.objects}{\code{\link{character}} or \code{\linkS4class{Risoe.BINfileData}} (\bold{required}):
Character vector with path and files names 
(e.g. \code{input.objects = c("path/file1.bin", "path/file2.bin")} 
or \code{\linkS4class{Risoe.BINfileData}} objects 
(e.g. \code{input.objects = c(object1, object2)})}
  \item{output.file}{\code{\link{character}} (optional): File output path and name. \cr
If no value is given, a \code{\linkS4class{Risoe.BINfileData}} \cr
is returned instead of a file.}
  \item{keep.position.number}{\code{\link{logical}} (with default): Allows keeping the original 
position numbers of the input objects. Otherwise the positions numbers a recalculated.}
}
\details{The function allows mering different measurements to one file or one object.\cr
The record IDs are recalculated for the new object. Other values 
are kept for each object. The number of input objects is not limited. }
\value{Returns a \code{file} or a \code{\linkS4class{Risoe.BINfileData}} object.}
\references{Duller, G., 2007. Analyst. }
\author{Sebastian Kreutzer, JLU Giessen (Germany), 
R Luminescence Package Team}
\note{The validity of the output objects is not further checked.  }


\seealso{\code{\linkS4class{Risoe.BINfileData}},
\code{\link{readBIN2R}}, \code{\link{writeR2BIN}}  }
\examples{

##merge two objects
data(ExampleData.BINfileData, envir = environment())

object1 <- CWOSL.SAR.Data
object2 <- CWOSL.SAR.Data

object.new <- merge_Risoe.BINfileData(c(object1, object2))


}

\keyword{IO}
\keyword{manip}
