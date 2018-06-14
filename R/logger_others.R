# readLogger_FLUKE_1730 --------------------------------------------------------

#' Read Logger File from FLUKE_1730
#' 
#' @param filepath full path to logger file
#' @param sep column separator
#' @param dec decimal character
#' 
#' @references \url{http://assets.fluke.com/manuals/1730____umeng0000.pdf}
#' 
#' @export
#' 
#' @examples 
#' # set path to example file (contained in this package)
#' filepath <- grep("FLUKE_1730", exampleLoggerFiles(), value = TRUE)[1]
#'   
#' # read the file
#' x <- readLogger_FLUKE_1730(filepath)
#'   
#' # examine the list structure of the result
#' str(x)  
#'
readLogger_FLUKE_1730 <- function(filepath, sep = ";", dec = ".") 
{
  utils::read.csv(
    filepath, header = TRUE, sep = sep, dec = dec, na.strings = "1.#QNAN",
    stringsAsFactors = FALSE
  )
}

# readLogger_STS_DL70 ----------------------------------------------------------

#' Read Logger File from STS DL70
#' 
#' @param filepath full path to logger file
#' @param sep column separator
#' @param dec decimal character
#' @param dateformat date format string
#' @param timeformat time format string
#' 
#' @references \url{http://www.stssensoren.de/app/download/5648435717/Manual_DL70-PC-Software_de_DDB013B.pdf?t=1372317244}
#' 
#' @return data frame with attribute "metadata"
#' 
#' @export
#' 
#' @examples 
#' # set path to example file (contained in this package)
#' (filepath <- grep("STS_DL", exampleLoggerFiles(), value=TRUE))
#'   
#' # read the file
#' x <- readLogger_STS_DL70(filepath)
#'   
#' # examine the structure of the result
#' str(x)  
#'   
readLogger_STS_DL70 <- function(
  filepath, sep = "\t", dec = ".", dateformat = .defaultTimeFormat("v5"),
  timeformat = .defaultTimeFormat("v1")
)
{
  header <- readLines(filepath, n = 2)
  
  meta <- c(
    kwb.utils::subExpressionMatches(
      regularExpression = "Identification: (.*)\tSN/TD: (.*)\tFirmware: (.*)$", 
      text = header[1], 
      match.names = c("Identification", "SN_TD", "Firmware")
    ),
    kwb.utils::subExpressionMatches(
      regularExpression = paste0(
        "Minimum value (.*) mWS\tMaximum value (.*) mWS\t",
        "Average value (.*) mWS\tPressure_Type=\"(.*)\"\t"
      ), 
      text = header[2], 
      match.names = c("min", "max", "avg", "pressure_type")
    )
  )
  
  #Header
  #"Time\tDate\tPressure [mWS]\tTemperature [?C]\tConductivity [mS/cm]\tCounter [mm]\t"   
  result <- kwb.utils::readCsvInputFile(
    filepath, sep = sep, dec = dec, headerPattern = "Time\tDate", 
    stringsAsFactors = FALSE
  )
  
  tcolname <- "myDateTime"
  
  result[[tcolname]] <- kwb.datetime::reformatTimestamp(
    x = paste(result$Date, result$Time), 
    old.format = paste(dateformat, timeformat)
  )
  
  columns <- c(
    "myDateTime", setdiff(names(result), c(tcolname, "Date", "Time"))
  )

  structure(
    kwb.utils::selectColumns(result, columns), 
    metadata = as.data.frame(meta, stringsAsFactors = FALSE)
  )
}

# readLogger_PCE_PA8000 --------------------------------------------------------

#' Read Logger File from PCE PA8000
#' 
#' @param filename full path to logger file
#' @param sep column separator
#' @param dec decimal character
#' @param timeformat time format string
#' @param headerPattern pattern matching the table header row
#' 
#' @references \url{http://www.industrial-needs.com/manual/manual-pce-pa8000.pdf}
#' 
#' @export
#' 
#' @examples 
#' # set path to example file (contained in this package)
#' (filepath <- grep("PCE_PA8000", exampleLoggerFiles(), value = TRUE))
#'   
#' # read the file
#' x <- readLogger_PCE_PA8000(filepath)
#'   
#' # examine the structure of the result
#' str(x)  
#'   
readLogger_PCE_PA8000 <- function(
  filename, sep = "\t", dec = ",", timeformat = .defaultTimeFormat("v2"),
  headerPattern = "Position\tDate\tTime"
) 
{
  dat <- kwb.utils::readCsvInputFile(
    filename, sep = sep, dec = dec, headerPattern = headerPattern, 
    stringsAsFactors = FALSE, stopOnMissingColumns = FALSE
  )
  
  dat$myDateTime <- kwb.datetime::reformatTimestamp(
    paste(dat$Date, dat$Time), timeformat
  )
  
  columns <- c(
    "myDateTime", setdiff(names(dat), c("myDateTime", "Date", "Time"))
  )
  
  kwb.utils::selectColumns(dat, columns)
}
