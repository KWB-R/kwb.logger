# readLogger_SIGMA_SD900 -------------------------------------------------------

#' Read Logger File from SIGMA_SD900
#' 
#' @param filepath full path to input file
#' @param successOnly if TRUE, only file entries with "SUCCESS" in column
#'   "-RESULT-" are returned. Default: FALSE
#' @param sep column separator. Default: ","
#' @param dateformat format of timestamp. Default: "\%H:\%M \%m/\%d/\%Y"
#' 
#' @references \url{http://www.hach.com/asset-get.download.jsa?id=7639983273}
#' 
#' @export
#' 
#' @examples 
#' # set path to example file (contained in this package)
#' (filepath <- grep("SIGMA_SD900", exampleLoggerFiles(), value = TRUE))
#'   
#' # read the file
#' (samples <- readLogger_SIGMA_SD900(filepath))
#'   
#' # read only lines representing successful samples
#' (samplesOk <- readLogger_SIGMA_SD900(filepath, successOnly = TRUE))
#'   
#' # show metadata (given in attribute "metadata")
#' kwb.utils::getAttribute(samplesOk, "metadata")    
#' 
readLogger_SIGMA_SD900 <- function(
  filepath, successOnly = FALSE, sep = ",", 
  dateformat = .defaultTimeFormat("v6")
)
{
  descriptor <- kwb.utils::columnDescriptor
  
  columnDescription <- list(
    myDateTime = descriptor(match = "TIME STAMP", fixed = TRUE),
    sample = descriptor(match = "-?SMP-", fixed = FALSE),
    bottle = descriptor(match = "-?BTL-", fixed = FALSE),
    volume = descriptor(match = "SAMPLE VOLUME", fixed = TRUE),
    result = descriptor(match = "-?RESULT-", fixed = FALSE)
  )
  
  headerPattern <- "TIME STAMP"
  
  dat <- kwb.utils::readCsvInputFile(
    filepath, sep = sep, dec = ".", headerPattern = headerPattern,
    columnDescription = columnDescription, stringsAsFactors = FALSE, 
    stopOnMissingColumns = FALSE
  )
  
  dat$myDateTime <- kwb.datetime::reformatTimestamp(dat$myDateTime, dateformat)
  
  if (! is.null(dat$volume)) {
    
    volumeFields <- strsplit(dat$volume, "\\s+")
    dat$volume <- sapply(volumeFields, "[[", 1)
    dat$unit <- sapply(volumeFields, "[[", 2)
  }
  
  dat$result <- kwb.utils::hsTrim(dat$result)
  
  if (successOnly) {
    
    dat <- dat[dat$result == "SUCCESS", ]
  }
  
  columns <- c("myDateTime", "sample", "bottle", "volume", "unit", "result")

  structure(
    kwb.utils::selectColumns(dat, intersect(names(dat), columns)), 
    metadata = .getMeta_SIGMA_SD900(filepath, headerPattern, sep = sep)
  )
}

# .getMeta_SIGMA_SD900 ---------------------------------------------------------

.getMeta_SIGMA_SD900 <- function(filepath, headerPattern, sep)
{
  mylines <- readLines(filepath, warn = FALSE)
  headerLine <- grep(headerPattern, mylines)
  
  if (! length(headerLine)) {
    
    stop(sprintf(
      "Could not find header line \"%s\" in file \"%s\"",
      headerPattern, filepath
    ))
  }
  
  metadata <- utils::read.table(
    filepath, sep = sep, nrows = headerLine - 1, fill = TRUE, header = FALSE,
    stringsAsFactors = FALSE
  )
  
  metadata$V1 <- kwb.utils::hsSubstSpecChars(kwb.utils::hsTrim(metadata$V1))
  metadata <- metadata[grep("\\:$", metadata$V1), ]
  
  # remove non-alphanumeric characters
  metadata$V1 <- gsub("[^a-zA-Z0-9_]", "", metadata$V1)
  
  metadata <- kwb.utils::hsDelEmptyCols(metadata)  

  # trim all values in all columns
  for (column in seq(2, by = 1, length.out = ncol(metadata) - 1)) {
    
    metadata[[column]] <- kwb.utils::hsTrim(metadata[[column]])
  }  
  
  stats::setNames(as.list(metadata$V2), metadata$V1)
}
