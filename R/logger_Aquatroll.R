# read_aquatroll_data ----------------------------------------------------------

#' Read Aquatroll Data
#' 
#' Use readLogger_InSituInc_Aquatroll instead!
#' 
#' @param file full path to logger file
#' @export
read_aquatroll_data <- function(file)
{
  x <- utils::read.csv(
    file = file,
    sep = ";",
    dec = ".", 
    fill = TRUE, # fill empty columns with NA
    header = TRUE,
    stringsAsFactors = FALSE, # text as character
    na.strings = "0.000(ERR)"
  )
  
  columns <- c("DateandTime", "Temperature.C.", "SpecificConductivity.uS.")
  
  x <- kwb.utils::renameColumns(kwb.utils::selectColumns(x, columns), list(
    "DateandTime" = "DateTime",
    "Temperature.C." = "WaterTemp.C",
    "SpecificConductivity.uS." = "SpecCond.uS"
  ))
  
  x$DateTime <- as.POSIXct(
    x$DateTime, format = "%d.%m.%Y%H:%M:%S", tz = "Etc/GMT+1"
  )
  
  x
}

# readLogger_InSituInc_Aquatroll -----------------------------------------------

#' Read File From InSituInc Aquatroll
#' 
#' @param csv full path to logger file
#' @param headerPattern pattern matching the header of the table. See source
#'   code for the default.
#' @param timestampFormat vector of possible timestamp formats. See help for the
#'   default.
#' @param tz time zone. default: "Etc/GMT+1"
#' @param maxRowToLookForHeader number of first rows in the file to read in
#'   advance to look for the header line. Default: 700
#' @param model model of the Aquatroll (either "" or "600")
#' @param dbg if \code{TRUE}, debug messages are shown
#' @param fileEncoding encoding of the input file. Default: "latin1"
#' @return if model = "600" data frame with columns \code{DateTime}, 
#'   \code{Seconds}, \code{Temp}, \code{ActCond}, \code{SpecCond},
#'   \code{TDS}, \code{Resis}, \code{WaterD}, \code{DOC}, \code{DOP},
#'   \code{Tur}, \code{Sal} otherwise data frame with columns
#'   \code{DateTime}, \code{WaterTemp.C}, \code{SpecCond.uS}
#' @export
#' @examples 
#' # set path to example file (contained in this package)
#' file_1 <- extdataFile("InSituInc/example_InSituInc_Aquatroll.csv")
#' file_2 <- extdataFile("InSituInc/example_InSituInc_Aquatroll_600.csv")
#' 
#' # let's have a look on the file structure (read first 75 lines)
#' writeLines(kwb.utils::readLinesWithEncoding(file_1, 75, fileEncoding = "latin1"))
#'   
#' # now read the file
#' x <- readLogger_InSituInc_Aquatroll(csv = file_1)
#'   
#' # show the first lines
#' head(x)
#'   
#' # Read a file of Aquatroll, model "600"
#' x600 <- readLogger_InSituInc_Aquatroll(csv = file_2, model = "600")
#'   
#' # show the first lines
#' head(x600)
#'   
#' # show the structure
#' str(x600)
#' 
readLogger_InSituInc_Aquatroll <- function(
  csv, headerPattern = NULL, 
  timestampFormat = c("%d.%m.%Y %H:%M:%S", "%d.%m.%Y%H:%M:%S"),
  tz = "Etc/GMT+1", maxRowToLookForHeader = 700, model = "", dbg = FALSE,
  fileEncoding = "latin1"
)
{
  #kwb.utils:::assignArgumentDefaults(kwb.logger::readLogger_InSituInc_Aquatroll)
  
  # Set default header pattern
  headerPattern <- kwb.utils::defaultIfNULL(
    headerPattern, "Date\\s?and\\s?Time;Seconds"
  )
  
  coldesc <- if (model == "600") {
    
    list(
      DateTime = list(match = "Date\\s?and\\s?Time"),
      Seconds = list(match = "Seconds\\s?"),
      Temp = list(match = "Temperature?\\s?\\(C\\)"),
      #ActCond = list(match = "Actual\\s?Conductivity\\s?\\(\\xB5S/cm\\)"),
      #SpecCond = list(match = "Specific\\s?Conductivity\\s?\\(\\xB5S/cm\\)"),
      ActCond = list(match = "Actual\\s?Conductivity\\s?\\(\265S/cm\\)"),
      SpecCond = list(match = "Specific\\s?Conductivity\\s?\\(\265S/cm\\)"),
      TDS = list(match = "Total\\s?Dissolved\\s?Solids\\s?\\(ppt\\)"),
      Resis = list(match = "Resistivity?\\s?\\(ohm-cm\\)"),
      WaterD = list(match = "Water\\s?Density\\s?\\(g/cm3\\)"),
      DOC = list(match = "Dissolved\\s?Oxygen\\s?\\(concentration\\)\\s?\\(mg/L\\)"),
      DOP = list(match = "Dissolved\\s?Oxygen\\s?\\(%saturation)\\s?\\(%Sat\\)"),
      Tur = list(match = "Turbidity?\\s?\\(NTU\\)"),
      Sal = list(match = "Salinity?\\s?\\(PSU\\)")       
    )
    
  } else {
    
    list(
      DateTime = list(match = "Date\\s?and\\s?Time"),
      WaterTemp.C = list(match = "Temperature?\\s?\\(C\\)"),
      #SpecCond.uS = list(match = "Specific\\s?Conductivity\\s?\\((\\xB5|u)S\\)")
      SpecCond.uS = list(match = "Specific\\s?Conductivity\\s?\\((\265|u)S\\)")
    )
  }

  dat <- kwb.utils::readCsvInputFile(
    csv, 
    sep = ";", 
    dec = ".", 
    headerPattern = headerPattern, 
    maxRowToLookForHeader = maxRowToLookForHeader, 
    columnDescription = coldesc, 
    na = c("0.000(ERR)", "                   0.000 (ERR)"),
    stringsAsFactors = FALSE, 
    fill = TRUE,
    fileEncoding = fileEncoding
    #, encoding = "Latin-1"
  )
  
  format.ok <- kwb.datetime::matchingTimeFormat(
    timestamp = dat$DateTime[1L], timeFormats = timestampFormat
  )
  
  stopifnot(!is.null(format.ok))
  
  # At the end of the table there may be a block of notes. We will find the
  # corresponding rows by checking if the values in the first column look
  # like timetamps
  invalid <- !kwb.datetime::hasTimeFormat(dat$DateTime, format.ok)
  
  # We extract the invalid rows
  dat <- dat[!invalid, ]
  
  # Convert character timestamp to POSIXct
  dat$DateTime <- as.POSIXct(
    x = kwb.datetime::reformatTimestamp(dat$DateTime, old.format = format.ok), 
    tz = tz
  )
  
  # Convert numeric columns to numeric
  columns <- if (model == "600") {
    
    c(
      "Seconds", "Temp", "ActCond", "SpecCond", "TDS", "Resis", "WaterD", "DOC", 
      "DOP", "Tur", "Sal"
    )
    
  } else {
    
    c("WaterTemp.C", "SpecCond.uS")
  }
  
  # Reduce to existing columns
  columns <- intersect(columns, names(dat))

  # Function to convert to numeric (or not)
  to_numeric <- function(x, column) {
    if (is.numeric(x)) {
      kwb.utils::catIf(
        dbg, "Column", kwb.utils::hsQuoteChr(column), "is already numeric.\n"
      )
      return(x)
    }
    kwb.utils::hsChrToNum(x, country = "en")
  }
  
  for (column in columns) {
    dat[[column]] <- to_numeric(x = dat[[column]], column)
  }

  dat  
}
