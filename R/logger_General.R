# For a template of the following Roxygen documentation, see:
# writeLines(readLines("../kwb.utils/R/createFunctionExtdataFile.R"))

#' Path to File in Installed Package
#' 
#' @inheritParams kwb.utils::extdataFile
#' @importFrom kwb.utils createFunctionExtdataFile
#' @export
#' @examples 
#' # List the files provided in the "extdata" folder of kwb.logger
#' dir(extdataFile())
extdataFile <- kwb.utils::createFunctionExtdataFile("kwb.logger")
