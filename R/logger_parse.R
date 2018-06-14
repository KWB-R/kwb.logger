# .nonEolParser ----------------------------------------------------------------

.nonEolParser <- function()
{
  qmrparser::charInSetParser(function(x) ! qmrparser::isNewline(x))
}

# .eolParser -------------------------------------------------------------------

.eolParser <- function() 
{
  #qmrparser::charInSetParser(function(x) qmrparser::isNewline(x))
  
  qmrparser::alternation(
    qmrparser::keyword("\n"),
    qmrparser::keyword("\r\n")
  )
}

# .nonEmptyLine ----------------------------------------------------------------

.nonEmptyLine <- function()
{
  qmrparser::concatenation(
    qmrparser::repetition1N(.nonEolParser()),
    .eolParser(),
    action = function(x) .collapse(sapply(x[[1]]$value, "[[", 2))
  )
}

# .collapse --------------------------------------------------------------------

.collapse <- function(x, collapse = "")
{
  paste(x, collapse = collapse)
}

# .trim ------------------------------------------------------------------------

.trim <- function(x) 
{
  gsub("^\\s+", "", gsub("\\s+$", "", x))
}

# .dbg -------------------------------------------------------------------------

.dbg <- function(...) 
{
  if (getOption("kwb.debug")) {
    
    cat(...)
  }
}
