# exampleLoggerFiles -----------------------------------------------------------

#' Example Logger Files
#' 
#' @return full path(s) to example file(s)
#' 
#' @export
#' 
exampleLoggerFiles <- function() 
{
  dir(
    path = system.file("extdata", package = "kwb.logger"), recursive = TRUE, 
    full.names = TRUE
  )
}
