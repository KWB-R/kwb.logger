# assignArgumentDefaults -------------------------------------------------------

assignArgumentDefaults <- function(FUN)
{
  # Get formal argument list
  arguments <- as.list(args(FUN))
  
  # Remove last entry representing the body of the function
  arguments <- arguments[- length(arguments)]
  
  # Remove arguments without defaults
  arguments <- arguments[! sapply(arguments, is.symbol)]
  
  # Evaluate the expressions
  arguments <- lapply(arguments, eval, envir = arguments)
  
  # Assign all arguments with defaults in the global environment
  kwb.utils::assignAll(arguments, envir = .GlobalEnv)
}
