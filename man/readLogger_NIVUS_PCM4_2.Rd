% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logger_NIVUS_PCM4.R
\name{readLogger_NIVUS_PCM4_2}
\alias{readLogger_NIVUS_PCM4_2}
\title{Read Logger File from NIVUS PCM4}
\usage{
readLogger_NIVUS_PCM4_2(filepath, headerRow = 9, sep = "\\t",
  maxCols = 50, removeEmptyColumns = FALSE)
}
\arguments{
\item{filepath}{full path to logger file}

\item{headerRow}{number of row containing the header row of the table}

\item{sep}{column separator}

\item{maxCols}{maximum number of columns}

\item{removeEmptyColumns}{if \code{TRUE} empty columns are removed}
}
\description{
Read Logger File from NIVUS PCM4
}
\examples{
# set paths to example files (contained in this package)
filepaths <- grep("NIVUS_PCM4", exampleLoggerFiles(), value = TRUE)[-1]
  
# read the files
x1 <- readLogger_NIVUS_PCM4_2(filepaths[1])
x2 <- readLogger_NIVUS_PCM4_2(filepaths[2])
x3 <- readLogger_NIVUS_PCM4_2(filepaths[3])
  
# compare structures
str(x1)
str(x2)
str(x3)
  
# get metadata
(metadata <- kwb.utils::getAttribute(x1, "metadata"))
  
# show time adjusts
metadata$timeAdjust

}
\references{
\url{http://www.nivus.de/ximages/1397007_p4ba02en.pdf}
}