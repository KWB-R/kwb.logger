% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/logger_LT_EDGE_M100.R
\name{read_logger_LT_EDGE_M100_xle}
\alias{read_logger_LT_EDGE_M100_xle}
\title{Read Logger File (.xle) from LT EDGE M100}
\usage{
read_logger_LT_EDGE_M100_xle(
  file,
  timeFormat = .defaultTimeFormat("v4"),
  country = NA
)
}
\arguments{
\item{file}{full path to logger file}

\item{timeFormat}{time format string}

\item{country}{one of "en" (English format: decimal sign = ".") or "de"
(German format: decimal sign = ","). If country is NA (default) it is
guessed from the file content}
}
\description{
Read an input file of water level logger "LT EDGE M100" from .xle file
}
