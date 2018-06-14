# read_logger_LT_EDGE_M100 -----------------------------------------------------

#' Read Logger File from LT EDGE M100
#' 
#' Read an input file of water level logger "LT EDGE M100"
#' 
#' @param file full path to the file (either .xle or .csv)
#' @param timeFormat time format in which the timestamps are given in the file.
#'   Default: "yyyy/mm/dd HH:MM:SS". It is assumed that the timestamps represent
#'   Berlin normal time = Berlin winter time (no daylight saving)
#' @param metaToColumns if TRUE, metadata are written into the last columns of
#'   the returned data frame
#' @param dbg if TRUE (default) debug messages (e.g. "Reading...") are shown
#' @param \dots arguments passed to \code{\link{read_logger_LT_EDGE_M100_csv}}
#'   or \code{\link{read_logger_LT_EDGE_M100_xle}}
#'   
#' @return data frame with columns \emph{TimestampInFile} (character timestamp
#'   as it appeared in the file), \emph{DateTimeUTC} (POSIXct timestamp in time
#'   zone "UTC"), \emph{LocalDateTime} (POSIXct timestamp in time zone 
#'   "Europe/Berlin"), \emph{UTCOffset} (integer Offset in hours between UTC and
#'   local time), \emph{Level} (water level in unit as given in the file 
#'   header), \emph{Temperature} (water temperature in unit as given in the file
#'   header)
#'   
#' @export
#' 
read_logger_LT_EDGE_M100 <- function(
  file, timeFormat = .defaultTimeFormat("v4"), metaToColumns = FALSE, 
  dbg = TRUE, ...
)
{
  FUN <- if (grepl("\\.csv", file)) {
    
    read_logger_LT_EDGE_M100_csv
    
  } else if (grepl("\\.xle", file)) {
    
    read_logger_LT_EDGE_M100_xle
    
  } else {
    
    stop(
      "file with extension '.csv' or '.xle' expected (filename is '", file, "'"
    )
  }  
  
  kwb.utils::catIf(dbg, sprintf("Reading '%s'... ", basename(file)))
  content <- FUN(file, ...)
  kwb.utils::catIf(dbg, "ok.\n")
  
  # save attribute "meta"
  metadata <- kwb.utils::getAttribute(content, "meta")
  
  content <- cbind(content, .to_timestamp_columns(
    paste(content$Date, content$Time), timeFormat
  ))
  
  columns <- c(
    "TimestampInFile", "DateTimeUTC", "LocalDateTime", "UTCOffset",
    "Level", "Temperature"
  )

  content <- kwb.utils::selectColumns(content, columns)
  
  # Append metadata as columns
  if (metaToColumns) {
    
    content <- data.frame(
      content, 
      lapply(metadata, rep, nrow(content)),
      stringsAsFactors = TRUE
    )
    
    # Reset metadata
    metadata <- NULL
  }
  
  # restore attribute "meta"
  structure(content, meta = metadata)
}

# .to_timestamp_columns --------------------------------------------------------

.to_timestamp_columns <- function(
  TimestampInFile, timeFormat = "%Y/%m/%d %H:%M:%S"
)
{
  DateTimeUTCplus1 <- kwb.datetime::toGmtRelativePosix(
    TimestampInFile, -1, timeFormat
  )
  
  DateTimeUTC <- kwb.datetime:::toTimezone(DateTimeUTCplus1, tz = "UTC")
  
  LocalDateTime = kwb.datetime:::toTimezone(DateTimeUTC, tz = "Europe/Berlin")
  
  data.frame(
    TimestampInFile = TimestampInFile,
    DateTimeUTC = DateTimeUTC,
    LocalDateTime = LocalDateTime,    
    UTCOffset = kwb.datetime:::utcOffset(
      as.character(LocalDateTime), as.character(DateTimeUTC)
    ),
    stringsAsFactors = FALSE
  )
}

# read_logger_LT_EDGE_M100_xle -------------------------------------------------

#' Read Logger File (.xle) from LT EDGE M100
#' 
#' Read an input file of water level logger "LT EDGE M100" from .xle file
#' 
#' @param file full path to logger file
#' @param timeFormat time format string
#' @param country one of "en" (English format: decimal sign = ".") or "de"
#'   (German format: decimal sign = ","). If country is NA (default) it is
#'   guessed from the file content
#'   
#' @export
#'   
read_logger_LT_EDGE_M100_xle <- function(
  file, timeFormat = .defaultTimeFormat("v4"), country = NA
)
{
  doc <- XML::xmlInternalTreeParse(file, encoding = "Windows-1252")
  
  dataStringList <- XML::xpathApply(doc, "//Log", XML::getChildrenStrings)
  
  # Find out the country code ("en" or "de") by looking for "," in
  # <Event_threshold>
  
  if (is.na(country)) {
    
    country <- .getCountryCode(doc, property = "Event_threshold")
  }  
  
  instrument_infos <- XML::xpathApply(
    doc, "//Instrument_info", XML::getChildrenStrings
  )
  
  metaInfo <- lapply(.getMetaConfig(), FUN = function(x) {
    
    path <- paste0("//", gsub(" ", "_", x))
    
    as.character(XML::xpathApply(doc, path, XML::getChildrenStrings)[[1]])
  })
  
  num_channels <- as.integer(instrument_infos[[1]]["Channel"])
  
  stopifnot(num_channels == 2)

  channelInfos <- lapply(seq_len(num_channels), function(i) {
    
    XML::xpathApply(doc, sprintf("//Ch%d_data_header", i), XML::getChildrenStrings)[[1]]
  })
    
  to_data_frame <- function(x) {
    data.frame(kwb.utils::rbindAll(x), stringsAsFactors = FALSE)
  }
  
  channels <- to_data_frame(channelInfos)
  mydata <- to_data_frame(dataStringList)
  
  mydata <- kwb.utils::renameColumns(mydata, list(
    ch1 = .allButFirstToLower(channels$Identification[1]), 
    ch2 = .allButFirstToLower(channels$Identification[2])
  ))
  
  kwb.utils::checkForMissingColumns(mydata, c("ms", "Level", "Temperature"))
  
  # Convert from character to numeric
  mydata$ms <- as.integer(mydata$ms)
  mydata$Level <- kwb.utils::hsChrToNum(mydata$Level, country)
  mydata$Temperature <- kwb.utils::hsChrToNum(mydata$Temperature, country)
  
  # set meta information in attribute "meta"
  structure(mydata, meta = metaInfo)
}

# .getCountryCode --------------------------------------------------------------

.getCountryCode <- function(doc, property)
{  
  value <- XML::xpathApply(doc, paste0("//", property), XML::getChildrenStrings)
  
  if (! length(value)) {
    
    stop(sprintf("Could not find <%s> in '%s'", property, file))
  }
  
  ifelse(grepl(",", value[[1]]), "de", "en")  
}

# .getMetaConfig ---------------------------------------------------------------

.getMetaConfig <- function() 
{
  list(
    Serial_number = "Serial_number", 
    Project_ID = "Project ID",
    Location = "Location"
  )
}

# .allButFirstToLower ----------------------------------------------------------

.allButFirstToLower <- function(x)
{
  stopifnot(length(x) == 1)
  paste0(substr(x, 1, 1), tolower(substr(x, 2, nchar(x))))
}

# read_logger_LT_EDGE_M100_csv -------------------------------------------------

#' Read Logger File (.csv) from LT EDGE M100
#' 
#' Read an input file of water level logger "LT EDGE M100" from .csv file
#' 
#' @param file full path to logger file
#' @param country one of "en" (English format: column separator = ",", decimal
#'   sign = ".") or "de" (German format: column separator = ";", decimal sign =
#'   ",")
#' @param maxRowToLookForHeader number of rows to look for column headers
#'   (default: 15)
#'   
#' @export
#' 
read_logger_LT_EDGE_M100_csv <- function(
  file, country = "en", maxRowToLookForHeader = 15
)
{
  sep = ifelse(country == "de", ";", ",")
  dec = ifelse(country == "de", ",", ".")
  
  content <- kwb.utils::readCsvInputFile(
    file, 
    sep = sep, 
    dec = dec, 
    headerPattern = "^Date", 
    maxRowToLookForHeader = maxRowToLookForHeader, 
    columnDescription = list(
      Date = list(match = "Date"),
      Time = list(match = "Time"),
      ms = list(match = "ms"),
      Level = list(match = "LEVEL"),
      Temperature = list(match = "TEMPERATURE")
    ), 
    stringsAsFactors = FALSE
  )
  
  metaLines <- readLines(file, maxRowToLookForHeader)
    
  # not (yet?) considered:
  #"LEVEL"                                "UNIT: m"
  #"Offset: 0.000000 m"                  
  #"TEMPERATURE"                          "UNIT: °C"                               
  
  metaRows <- lapply(.getMetaConfig(), FUN = function(x) {
    pattern <- sprintf("^%s:", x)
    rownumber <- grep(pattern, metaLines)
    if (length(rownumber) != 1) {
      stop(sprintf("Pattern '%s' not found in '%s'", pattern, file))
    }
    rownumber
  })
  
  # set meta information in attribute "meta"
  structure(content, meta = lapply(metaRows, function(i) metaLines[i + 1]))
}
