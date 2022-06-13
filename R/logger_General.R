# For a template of the following Roxygen documentation, see:
# writeLines(readLines("../kwb.utils/R/createFunctionExtdataFile.R"))

#' Path to File in Installed Package
#' 
#' @param \dots parts of the file path to be passed to \code{\link{system.file}}
#' @param must_exist if \code{TRUE} (the default) and the specified file does 
#'   not exist, the program stops with an error message
#' @return path to file in the package installation folder in the R library
#'   or "" if the path does not exist
#' @return path to the specified file
#' @importFrom kwb.utils createFunctionExtdataFile
#' @export
#' @examples 
#' # List the files provided in the "extdata" folder of kwb.logger
#' dir(extdataFile())
extdataFile <- kwb.utils::createFunctionExtdataFile("kwb.logger")
