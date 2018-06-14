# readLogger_FLEXIM_F601 -------------------------------------------------------

#' Read Logger File from FLEXIM F601
#' 
#' @param filename full path to logger file
#' @param sep column separator
#' @param dec decimal character
#' @param timeformat time format string
#' @param headerPattern pattern matching the header line
#' @param date_yyyymmdd datestring, by default taken from the filename
#' @param dbg if \code{TRUE}, debug messages are shown
#' 
#' @references \url{http://www.flexim.com/files/tsfluxus_f601v1-5-1de_leu.pdf}
#' 
#' @export
#' 
#' @examples 
#' # Set path to example file (contained in this package)
#' filename <- grep("FLEXIM_F601", exampleLoggerFiles(), value = TRUE)[2]
#'   
#' # Let's have a look on the file structure
#' kwb.utils::catLines(readLines(filename))
#'   
#' # Now read the file
#' x <- readLogger_FLEXIM_F601(filename)
#'   
#' # Show the first lines
#' head(x)
#'   
#' # Get the meta data and show its structure
#' str(kwb.utils::getAttribute(x, "metadata"))
#'
readLogger_FLEXIM_F601 <- function(
  filename, sep = "\t", dec = ",", 
  #timeformat = .defaultTimeFormat("v2"),
  timeformat = "%d.%m.%Y %H:%M:%S",
  headerPattern = "Uhrzeit\tDruck", 
  date_yyyymmdd = substr(basename(filename), 1, 8), 
  dbg = TRUE
) 
{
  #.defaultTimeFormat <- kwb.logger:::.defaultTimeFormat
  #.parser_FLEXIM_F601 <- kwb.logger:::.parser_FLEXIM_F601
  #kwb.utils:::assignArgumentDefaults(readLogger_FLEXIM_F601)
  locale_old <- Sys.setlocale("LC_ALL", "C")
  options_old <- options(kwb.debug = dbg)
  
  on.exit({
    reset_locale(locale_old)
    options(options_old)
  })

  stream_parser <- qmrparser::streamParserFromFileName(filename)
  
  parseresult <- .parser_FLEXIM_F601()(stream = stream_parser)
  
  if (parseresult$status != "ok") {
    
    stop("Error parsing", filename)
  }
  
  header_text <- kwb.utils::selectElements(
    elements = "header", kwb.utils::selectElements(
      elements = "data", kwb.utils::selectElements(
        parseresult, "node"
      )))
  
  header <- utils::read.table(
    textConnection(header_text), 
    sep = sep, header = TRUE, 
    stringsAsFactors = FALSE
  )
  
  body_text <- kwb.utils::selectElements(
    elements = "body", kwb.utils::selectElements(
      elements = "data", kwb.utils::selectElements(
        parseresult, "node"
      )))
  
  mydata <- utils::read.table(
    textConnection(body_text), sep = sep, dec = dec, 
    header = FALSE, stringsAsFactors = FALSE
  )
  
  names(mydata) <- names(header)
  
  mydata$DATE_TIME <- kwb.datetime::reformatTimestamp(
    kwb.utils::selectColumns(mydata, "DATE_TIME"), timeformat
  )
  
  result <- kwb.utils::hsDelEmptyCols(mydata)

  metadata <- kwb.utils::selectElements(
    elements = "metadata", kwb.utils::selectElements(
      elements = "node", parseresult
    ))
  
  structure(result, metadata = metadata)
}

# reset_locale -----------------------------------------------------------------
reset_locale <- function(locale_string)
{
  assignments <- strsplit(locale_string, ";")[[1]]
  
  for (assignment in assignments) {
    
    category_locale <- strsplit(assignment, "=")[[1]]
    
    result <- try(
      Sys.setlocale(category_locale[1], category_locale[2]),
      silent = TRUE
    )
    
    if (inherits(result, "try-error")) {
      
      kwb.utils::printIf(TRUE, category_locale, "Could not set locale")
    }
  }
}

# .parser_FLEXIM_F601 ----------------------------------------------------------

.parser_FLEXIM_F601 <- function()
{
  qmrparser::concatenation(
    qmrparser::whitespace(),
    .starttag(),
    .eolParser(),
    .metablock(),
    qmrparser::whitespace(),
    .datablock(),
    qmrparser::whitespace(),
    .endtag(),
    action = function(x) list(metadata = x[[4]], data = x[[6]])
  )
}

# .starttag --------------------------------------------------------------------

.starttag <- function()
{
  qmrparser::keyword(
    "::::::::::::::::::::::::::::::::::::", 
    error = function(x, y) cat("*** No or wrong start line (\"::: ...\")")
  )
}

# .metablock -------------------------------------------------------------------

.metablock <- function()
{
  qmrparser::repetition1N(
    .metasection(),
    action = function(x) {    
      stats::setNames(object = sapply(x, "[[", 2), nm = sapply(x, "[[", 1))
    }
  )
}

# .metasection -----------------------------------------------------------------

.metasection <- function()
{
  qmrparser::concatenation(
    .metasectionHeader(),
    .metasectionBody(),
    action = function(x) list(
      header = x[[1]]$section, 
      body = if (length(x) > 1) x[[2]] else list()
    )
  )  
}

# .metasectionHeader -----------------------------------------------------------

.metasectionHeader <- function()
{
  qmrparser::concatenation(
    .metasectionKeyword(),
    qmrparser::option(.metasectionAppendix()),
    .eolParser(),
    action = function(x) list(section = x[[1]])
  )  
}

# .metasectionKeyword ----------------------------------------------------------

.metasectionKeyword <- function()
{
  qmrparser::concatenation(
    qmrparser::charParser("\\"),
    qmrparser::symbolic(),
    action = function(x) {
      x <- x[[2]]$value
      .dbg("Section keyword:", x, "\n")
      x
    }
  ) 
}

# .metasectionAppendix ---------------------------------------------------------

.metasectionAppendix <- function()
{
  qmrparser::concatenation(
    qmrparser::charParser("="), 
    qmrparser::repetition1N(.nonEolParser()),
    action = function(x) {
      .dbg(
        "  appendix:",
        paste(sapply(x[[2]]$value, "[[", "value"), collapse = ""), "\n"
      )
    }          
  )
}

# .metasectionBody -------------------------------------------------------------

.metasectionBody <- function()
{
  qmrparser::repetition0N(
    .assignment(),
    action = function(x) if (is.list(x$value)) {
      stats::setNames(
        object = as.list(sapply(x$value, "[[", 2)), 
        nm = kwb.utils::hsSubstSpecChars(sapply(x$value, "[[", 1))
      )
    }
  )
}

# .assignment ------------------------------------------------------------------

.assignment <- function()
{
  qmrparser::concatenation(
    .assignmentKey(),
    qmrparser::charParser(":"),
    qmrparser::option(.assignmentValue()),
    .eolParser(),
    action = function(x) {
      key <- .trim(x[[1]])
      value <- x[[3]]$value
      value <- ifelse(is.list(value), value$value, value)
      value <- .trim(value)
      .dbg(sprintf("assignment: \"%s\" = \"%s\"\n", key, value))
      list(key = key, value = value)
    },
    error = function(x, y) .dbg("End of assignments.\n")
  )
}

# .assignmentKey ---------------------------------------------------------------

.assignmentKey <- function()
{
  qmrparser::concatenation(
    .nonEolParser(), #01
    .nonEolParser(), #02
    .nonEolParser(), #03
    .nonEolParser(), #04
    .nonEolParser(), #05
    .nonEolParser(), #06
    .nonEolParser(), #07
    .nonEolParser(), #08
    .nonEolParser(), #09
    .nonEolParser(), #10
    .nonEolParser(), #11
    .nonEolParser(), #12
    .nonEolParser(), #13
    .nonEolParser(), #14
    .nonEolParser(), #15
    .nonEolParser(), #16
    .nonEolParser(), #17  
    action = function(x) .collapse(sapply(x, "[[", 2))
  )
}

# .assignmentValue -------------------------------------------------------------

.assignmentValue <- function()
{
  qmrparser::repetition1N(
    .nonEolParser(), 
    action = function(x) .collapse(sapply(x, "[[", 2))
  )
}

# .datablock -------------------------------------------------------------------

.datablock <- function() 
{  
  qmrparser::concatenation(
    .datablockBeginTag(), 
    .datablockHeader(),
    .datablockBody(),
    .datablockEndTag(),
    action = function(x) list(header = x[[2]], body = x[[3]])
  )
}

# .datablockBeginTag -----------------------------------------------------------

.datablockBeginTag <- function() 
{  
  qmrparser::concatenation(
    qmrparser::keyword("\\DATA"),
    .eolParser(), 
    action = function(x) .dbg("Begin of data block\n")
  )
}

# .datablockHeader -------------------------------------------------------------

.datablockHeader <- function() 
{  
  qmrparser::concatenation(
    .datablockHeaderLine1(),
    .datablockHeaderLine2(),
    action = function(x) c(x[[1]], x[[2]])
  )
}

# .datablockHeaderLine1 --------------------------------------------------------

.datablockHeaderLine1 <- function() 
{  
  qmrparser::concatenation(
    qmrparser::keyword("\\*"),
    qmrparser::repetition1N(.nonEolParser()),
    .eolParser(),
    action = function(x) {
      .dbg("datablock header line 1\n")
      .collapse(sapply(x[[2]]$value, "[[", 2))
    }
  )
}

# .datablockHeaderLine2 --------------------------------------------------------

.datablockHeaderLine2 <- function() 
{  
  qmrparser::concatenation(
    qmrparser::keyword("\\#"),
    qmrparser::repetition1N(.nonEolParser()),
    .eolParser(),
    action = function(x) {
      .dbg("datablock header line 2\n")
      .collapse(sapply(x[[2]]$value, "[[", 2))
    }
  )
}

# .datablockBody ---------------------------------------------------------------

.datablockBody <- function() 
{  
  qmrparser::repetition1N(
    .datablockLine(), 
    action = function(x) sapply(x, "[[", 1)
  )
}

# .datablockLine ---------------------------------------------------------------

.datablockLine <- function() 
{  
  qmrparser::concatenation(
    qmrparser::charParser("A"),
    qmrparser::repetition0N(.nonEolParser()),
    .eolParser(),
    action = function(x) {
      line <- .collapse(sapply(x[[2]]$value$value, "[[", 2))
      #.dbg(sprintf("data block line: \"%s\"\n", line))
      line
    }  
  )
}

# .datablockEndTag -------------------------------------------------------------

.datablockEndTag <- function() 
{  
  qmrparser::concatenation(
    qmrparser::keyword("\\END"),
    .eolParser(),
    action = function(x) .dbg("End of data block\n")
  ) 
}

# .endtag ----------------------------------------------------------------------

.endtag <- function() 
{  
  qmrparser::repetition1N(
    .nonEmptyLine(), 
    action = function(x) .collapse(sapply(x, "[[", 1), "\n")
  )
}
