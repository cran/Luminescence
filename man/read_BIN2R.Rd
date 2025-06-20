% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/read_BIN2R.R
\name{read_BIN2R}
\alias{read_BIN2R}
\title{Import Risø BIN/BINX-files into R}
\usage{
read_BIN2R(
  file,
  show.raw.values = FALSE,
  position = NULL,
  n.records = NULL,
  zero_data.rm = TRUE,
  duplicated.rm = FALSE,
  fastForward = FALSE,
  show.record.number = FALSE,
  txtProgressBar = TRUE,
  forced.VersionNumber = NULL,
  ignore.RECTYPE = FALSE,
  pattern = NULL,
  verbose = TRUE,
  ...
)
}
\arguments{
\item{file}{\link{character} or \link{list} (\strong{required}): path and file name of the
BIN/BINX file (URLs are supported). If input is a \code{list} it should comprise
only \code{character}s representing each valid path and BIN/BINX-file names.
Alternatively, the input character can be just a directory (path), in which
case the function tries to detect and import all BIN/BINX files found in
the directory.}

\item{show.raw.values}{\link{logical} (\emph{with default}):
shows raw values from BIN-file for \code{LTYPE}, \code{DTYPE} and \code{LIGHTSOURCE} without
translation in characters. Can be provided as \code{list} if \code{file} is a \code{list}.}

\item{position}{\link{numeric} (\emph{optional}):
imports only the selected position. Note: the import performance will not
benefit by any selection made here.
Can be provided as \code{list} if \code{file} is a \code{list}.}

\item{n.records}{\link{numeric} (\emph{optional}): limits the number of imported records
to the provided record id (e.g., \code{n.records = 1:10} imports the first ten records,
while \code{n.records = 3} imports only record number 3. Can be used in combination with
\code{show.record.number} for debugging purposes, e.g. corrupt BIN-files.
Can be provided as \code{list} if \code{file} is a \code{list}.}

\item{zero_data.rm}{\link{logical} (\emph{with default}):
remove erroneous data with no count values. As such data are usually not
needed for the subsequent data analysis they will be removed by default.
Can be provided as \code{list} if \code{file} is a \code{list}.}

\item{duplicated.rm}{\link{logical} (\emph{with default}):
remove duplicated entries if \code{TRUE}. This may happen due to an erroneous
produced BIN/BINX-file. This option compares only predecessor and successor.
Can be provided as \code{list} if \code{file} is a \code{list}.}

\item{fastForward}{\link{logical} (\emph{with default}):
if \code{TRUE} for a more efficient data processing only a list of \code{RLum.Analysis}
objects is returned instead of a \linkS4class{Risoe.BINfileData} object.
Can be provided as \code{list} if \code{file} is a \code{list}.}

\item{show.record.number}{\link{logical} (\emph{with default}):
shows record number of the imported record, for debugging usage only.
Can be provided as \code{list} if \code{file} is a \code{list}.
Ignored if \code{verbose = FALSE}.}

\item{txtProgressBar}{\link{logical} (\emph{with default}):
enable/disable the progress bar. Ignored if \code{verbose = FALSE}.}

\item{forced.VersionNumber}{\link{integer} (\emph{optional}):
allows to cheat the version number check in the function by own values for
cases where the BIN-file version is not supported.
Can be provided as \code{list} if \code{file} is a \code{list}.

\strong{Note:} The usage is at own risk, only supported BIN-file versions have been tested.}

\item{ignore.RECTYPE}{\link{logical} or \link{numeric} (\emph{with default}):
this argument allows to ignore values in the byte 'RECTYPE' (BIN-file version 08),
in case there are not documented or faulty set. In this case the corrupted records are skipped.
If the setting is \link{numeric} (e.g., \code{ignore.RECTYPE = 128}), records of those type are ignored
for import.}

\item{pattern}{\link{character} (\emph{optional}):
argument that is used if only a path is provided. The argument will than be
passed to the function \link{list.files} used internally to construct a \code{list}
of wanted files}

\item{verbose}{\link{logical} (\emph{with default}):
enable/disable output to the terminal.}

\item{...}{further arguments that will be passed to the function
\link{Risoe.BINfileData2RLum.Analysis}. Please note that any matching argument
automatically sets \code{fastForward = TRUE}}
}
\value{
Returns an S4 \linkS4class{Risoe.BINfileData} object containing two
slots:

\item{METADATA}{A \link{data.frame} containing all variables stored in the BIN-file.}
\item{DATA}{A \link{list} containing a numeric \link{vector} of the measured data.
The ID corresponds to the record ID in METADATA.}

If \code{fastForward = TRUE} a list of \linkS4class{RLum.Analysis} object is returned. The
internal coercing is done using the function \link{Risoe.BINfileData2RLum.Analysis}
}
\description{
Import a \verb{*.bin} or a \verb{*.binx} file produced by a Risø DA15 and DA20 TL/OSL
reader into R.
}
\details{
The binary data file is parsed byte by byte following the data structure
published in the Appendices of the Analyst manual p. 42.

For the general BIN/BINX-file structure, the reader is referred to the
Risø website: \url{https://www.fysik.dtu.dk}
}
\note{
The function works for BIN/BINX-format versions 03, 04, 05, 06, 07 and 08. The
version number depends on the used Sequence Editor.
}
\section{Function version}{
 0.18
}

\examples{

file <- system.file("extdata/BINfile_V8.binx", package = "Luminescence")
temp <- read_BIN2R(file)
temp

} 

\section{How to cite}{
Kreutzer, S., Fuchs, M.C., Colombo, M., 2025. read_BIN2R(): Import Risø BIN/BINX-files into R. Function version 0.18. In: Kreutzer, S., Burow, C., Dietze, M., Fuchs, M.C., Schmidt, C., Fischer, M., Friedrich, J., Mercier, N., Philippe, A., Riedesel, S., Autzen, M., Mittelstrass, D., Gray, H.J., Galharret, J., Colombo, M., Steinbuch, L., Boer, A.d., 2025. Luminescence: Comprehensive Luminescence Dating Data Analysis. R package version 1.1.0. https://r-lum.github.io/Luminescence/
}

\references{
DTU Nutech, 2016. The Sequence Editor, Users Manual, February, 2016.
\url{https://www.fysik.dtu.dk}
}
\seealso{
\link{write_R2BIN}, \linkS4class{Risoe.BINfileData},
\link[base:readBin]{base::readBin}, \link{merge_Risoe.BINfileData}, \linkS4class{RLum.Analysis}
\link[utils:txtProgressBar]{utils::txtProgressBar}, \link{list.files}
}
\author{
Sebastian Kreutzer, Institute of Geography, Heidelberg University (Germany)\cr
Margret C. Fuchs, HZDR Freiberg, (Germany) \cr
Marco Colombo, Institute of Geography, Heidelberg University (Germany)\cr
based on information provided by Torben Lapp and Karsten Bracht Nielsen (Risø DTU, Denmark)
, RLum Developer Team}
\keyword{IO}
