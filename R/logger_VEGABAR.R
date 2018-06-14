# readLogger_VEGA_BAR54 --------------------------------------------------------

#' Read Logger File from VEGA BAR54
#' 
#' @param filename full path to inoput file
#' @param date_yyyymmdd day of measurements as text in format "yyyymmdd", e.g.
#'   "20140423" for April 23 of 2014
#' @param sep column separator
#' @param dec decimal character
#' @param timeformat time format string
#' @param headerPattern pattern matching the table header row
#'
#' @references \url{http://www.vega.com/downloads/PI/EN/37528-EN.PDF}
#' 
#' @seealso \code{\link{readLogger_VEGA_BAR54_raw}}
#' 
#' @export
#' 
#' @examples 
#' # set path to example file (contained in this package)
#' (filepath <- grep("VEGA_BAR54[.]", exampleLoggerFiles(), value = TRUE))
#'   
#' # read the file
#' x <- readLogger_VEGA_BAR54(filepath)
#'   
#' # examine the structure of the result
#' str(x)  
#'
readLogger_VEGA_BAR54 <- function(
  filename, date_yyyymmdd = "", sep = "\t", dec = ",", 
  timeformat = .defaultTimeFormat("v2"), headerPattern = "Uhrzeit\tDruck"
) 
{
  columnDescription <- list(
    myDateTime = kwb.utils::columnDescriptor("Uhrzeit"),
    pressure = kwb.utils::columnDescriptor("Druck")
  )
  
  dat <- kwb.utils::readCsvInputFile(
    filename, sep = sep, dec = dec, headerPattern = headerPattern,
    columnDescription = columnDescription, stringsAsFactors = FALSE
  )
  
  if (date_yyyymmdd == "") {
    
    date_yyyymmdd <- "19000101"
    
    warning(
      "No date (argument date_yyyymmdd) given. Date is set to 1900-01-01!"
    )
  }
  
  dat$myDateTime <- kwb.datetime::reformatTimestamp(
    paste(date_yyyymmdd, dat$myDateTime), old.format = "%Y%m%d %H:%M:%S"
  )
  
  dat  
}

# readLogger_VEGA_BAR54_raw ----------------------------------------------------

#' Read Logger File from VEGA BAR54 raw
#' 
#' @param filepath full path to logger file
#' @param trimMetadata if TRUE, metadata of class "character" are trimmed (by
#'   using \code{\link[kwb.utils]{hsTrim}})
#' 
#' @seealso \code{\link{readLogger_VEGA_BAR54}}  
#' 
#' @export
#' 
#' @examples 
#' # set paths to example files (contained in this package)
#' (filepaths <- grep("VEGA_BAR54_raw", exampleLoggerFiles(), value = TRUE))
#'   
#' # read the files
#' x1 <- readLogger_VEGA_BAR54_raw(filepaths[1])
#' x2 <- readLogger_VEGA_BAR54_raw(filepaths[2])
#'     
#' # get meta data
#' kwb.utils::getAttribute(x1, "metadata")
#' kwb.utils::getAttribute(x2, "metadata")
#' 
readLogger_VEGA_BAR54_raw <- function(filepath, trimMetadata = TRUE)
{
  txt <- paste(readLines(filepath), collapse = "\n")
  
  (pos <- gregexpr("\\[([^]]+)\\][^[]*", txt))
  
  sections <- regmatches(txt, pos)[[1]]
  
  result <- list()
  
  for (section in sections) {
    
    sec <- .splitSection(section, trim = trimMetadata)
    
    result[[sec$name]] <- sec$data
  }

  basics <- kwb.utils::selectElements(result, "BASICS")  
  
  starttime.utc <- .getStartTimeFromBasicsSection(basics)
  timestep.s <- .getTimeStepFromBasicsSection(basics)
  hasTempColumn <- .hasTemperatureColumn(basics)
  
  myValues <- .prepareValuesSection(
    kwb.utils::selectElements(result, "VALUES"), 
    starttime.utc = starttime.utc, 
    timestep.s = timestep.s,
    hasTempColumn = hasTempColumn
  )
  
  structure(myValues, metadata = result[- which(names(result) == "VALUES")])
}

# .getStartTimeFromBasicsSection -----------------------------------------------

.getStartTimeFromBasicsSection <- function(x) 
{
  as.POSIXct(
    x = as.numeric(x$V2[x$V1 == "START_TIME"]) * 86400, 
    origin = "1899-12-30", tz = "UTC"
  )  
}

# .getTimeStepFromBasicsSection ------------------------------------------------

.getTimeStepFromBasicsSection <- function(x) 
{
  as.numeric(x$V2[x$V1 == "UPDATE_TIMEBASE"])
}

# .hasTemperatureColumn --------------------------------------------------------

.hasTemperatureColumn <- function(x) 
{
  as.numeric(x$V2[x$V1 == "TEMPERATURE"]) == 1  
}

# .prepareValuesSection --------------------------------------------------------

.prepareValuesSection <- function(
  x, starttime.utc, timestep.s, hasTempColumn, tcolname = "myDateTime"
)
{
  columns <- c("pressure", "unknown", if (hasTempColumn) "Temp_K")

  names(x) <- columns
  
  tstamps <- seq(starttime.utc, by = timestep.s, length.out = nrow(x))
  
  x[[tcolname]] <- as.character(tstamps)
  
  kwb.utils::selectColumns(x, c(tcolname, columns))
}

# .splitSection ----------------------------------------------------------------

.splitSection <- function(x, sep = ";", dec = ".", trim = TRUE)
{
  mylines <- strsplit(x, "\n")[[1]]
  sectionName <- gsub("\\[", "", gsub("\\]", "", mylines[1]))
  
  colClasses <- NA
  
  if (sectionName == "BASICS") {
    
    colClasses <- "character"
  }
  
  body <- mylines[-1]
  body <- body[! grepl("^\\s*$", body)]
  
  mydata <- utils::read.table(
    textConnection(body), sep = sep, dec = dec, as.is = TRUE, 
    colClasses = colClasses
  )

  if (trim) {
    
    for (colname in names(mydata)) {
      
      if (class(mydata[[colname]]) == "character") {
        
        mydata[[colname]] <- kwb.utils::hsTrim(mydata[[colname]])
      }
    }
  }
  
  list(name = sectionName, data = mydata)
}
