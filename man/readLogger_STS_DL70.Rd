% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logger_others.R
\name{readLogger_STS_DL70}
\alias{readLogger_STS_DL70}
\title{Read Logger File from STS DL70}
\usage{
readLogger_STS_DL70(
  filepath,
  sep = "\\t",
  dec = ".",
  dateformat = .defaultTimeFormat("v5"),
  timeformat = .defaultTimeFormat("v1")
)
}
\arguments{
\item{filepath}{full path to logger file}

\item{sep}{column separator}

\item{dec}{decimal character}

\item{dateformat}{date format string}

\item{timeformat}{time format string}
}
\value{
data frame with attribute "metadata"
}
\description{
Read Logger File from STS DL70
}
\examples{
\dontrun{
# set path to example file (contained in this package)
file <- extdataFile("STS/example_STS_DLN70_H.txt")

# read the file
x <- readLogger_STS_DL70(file)
  
# examine the structure of the result
str(x)  
}

}
\references{
\url{http://www.stssensoren.de/app/download/5648435717/Manual_DL70-PC-Software_de_DDB013B.pdf?t=1372317244}
}
