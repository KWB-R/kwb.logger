# readLogger_NIVUS_PCM4 --------------------------------------------------------

#' Read Logger File from NIVUS PCM4
#' 
#' @param csv full path to CSV file
#' @param completenessRequiredFor character string of column names that are
#'   required not to be empty
#' 
#' @references \url{http://www.nivus.de/ximages/1397007_p4ba02en.pdf}
#' 
#' @export
#' 
#' @examples 
#' # set path to example file (contained in this package)
#' filepath <- grep("NIVUS_PCM4", exampleLoggerFiles(), value = TRUE)[1]
#'   
#' # read the file
#' x <- readLogger_NIVUS_PCM4(filepath)
#'   
#' # examine the list structure of the result
#' str(x)  
#' 
readLogger_NIVUS_PCM4 <- function(
  csv, completenessRequiredFor = c("DateTime", "H", "v")
) 
{
  x <- utils::read.table(
    csv, sep = "\t", dec = ",", skip = 10, blank.lines.skip = TRUE, 
    fill = TRUE, stringsAsFactors = FALSE
  )
  
  x$V0 <- kwb.datetime::hsToPosix(kwb.datetime::reformatTimestamp(
    paste(x$V1, x$V2), "%d.%m.%Y %H:%M:%S", "%Y-%m-%d %H:%M:%S"
  ))
  
  renames <- list(V0 = "DateTime", V3 = "H", V4 = "v", V5 = "Q", V6 = "T")
  
  dat <- kwb.utils::renameColumns(x, renames)
  
  dat <- kwb.utils::selectColumns(dat, c(as.character(renames)))
  
  for (colname in completenessRequiredFor) {
    
    dat <- dat[stats::complete.cases(dat[[colname]]), ]
  }
  
  # convert H character values to numeric
  dat$H <- as.numeric(gsub(",", ".", dat$H))
  
  dat
}

# readLogger_NIVUS_PCM4_2 ------------------------------------------------------

#' Read Logger File from NIVUS PCM4
#' 
#' @param filepath full path to logger file
#' @param headerRow number of row containing the header row of the table
#' @param sep column separator
#' @param maxCols maximum number of columns
#' @param removeEmptyColumns if \code{TRUE} empty columns are removed
#' 
#' @references \url{http://www.nivus.de/ximages/1397007_p4ba02en.pdf}
#' 
#' @export
#' 
#' @examples 
#' # set paths to example files (contained in this package)
#' filepaths <- grep("NIVUS_PCM4", exampleLoggerFiles(), value = TRUE)[-1]
#'   
#' # read the files
#' x1 <- readLogger_NIVUS_PCM4_2(filepaths[1])
#' x2 <- readLogger_NIVUS_PCM4_2(filepaths[2])
#' x3 <- readLogger_NIVUS_PCM4_2(filepaths[3])
#'   
#' # compare structures
#' str(x1)
#' str(x2)
#' str(x3)
#'   
#' # get metadata
#' (metadata <- kwb.utils::getAttribute(x1, "metadata"))
#'   
#' # show time adjusts
#' metadata$timeAdjust
#'
readLogger_NIVUS_PCM4_2 <- function(
  filepath, headerRow = 9, sep = "\t", maxCols = 50, removeEmptyColumns = FALSE
)
{
  headerPattern <- "^Datum\tUhrzeit"
  
  # header line is expected in line 9
  headerLines <- readLines(filepath, n = 9)
  headerLine <- utils::tail(headerLines, 1)
  
  if (! grepl(headerPattern, headerLine)) {
    
    stop(sprintf(
      "Could not find header pattern \"%s\" in line number 9!",
      headerPattern
    ))
  }
  
  # Read metadata from header lines (last header line excluded)
  metadata <- .getMetadataFromMetalines(headerLines[- length(headerLines)])
  
  headerFields <- .splitHeaderLine(headerLine, sep)
  
  headerCaptions <- kwb.utils::hsSubstSpecChars(headerFields)
  
  col.names <- sprintf("V%d", seq_len(maxCols))
  col.names[seq_along(headerCaptions)] <- headerCaptions
  
  # Treat empty strings ("") as NA in order to let hsDelEmptyCols work
  bodylines <- utils::read.table(
    filepath, sep = sep, fill = TRUE, skip = headerRow, na.strings = "", 
    colClasses = "character", comment.char = "", col.names = col.names, 
    stringsAsFactors = FALSE
  )
  
  # delete fully empty columns to the right
  bodylines <- .delEmptyColumnsToTheRight(bodylines)  
  
  # set "#-1" to NA
  bodylines[bodylines == "#-1"] <- NA
  
  # Look for time adjustments  
  indicesTimeAdjust <- which(bodylines[, 3] == "//Uhrzeit verstellt")
  
  if (length(indicesTimeAdjust) > 0) {
    
    timeBefore <- paste(bodylines[indicesTimeAdjust -1, 1:2], collapse = " ")
    timeAfter  <- paste(bodylines[indicesTimeAdjust +1, 1:2], collapse = " ")
    
    metadata$timeAdjust <- data.frame(
      timeBefore = timeBefore, 
      timeAfter = timeAfter, 
      stringsAsFactors = FALSE
    )    
  }
  
  # Handle invalid rows (rows without a date) 
  indicesInvalid <- which(is.na(bodylines$Datum))
  
  if (length(indicesInvalid)) {
    
    # invalid rows that are not time adjusts
    indicesInvalidOther <- setdiff(indicesInvalid, indicesTimeAdjust)
    
    # Save "invalidRows" together with row before and row after in metadata
    if (length(indicesInvalidOther)) {
      
      metadata$invalidRows <- bodylines[.indicesOfContext(indicesInvalidOther), ]
    }
    
    # Remove invalid rows
    bodylines <- bodylines[- indicesInvalid, ]
  }
  
  myDateTime <- kwb.datetime::reformatTimestamp(
    paste(bodylines$Datum, bodylines$Uhrzeit), "%d.%m.%Y %H:%M:%S"
  )
  
  if (removeEmptyColumns) {
    
    bodylines <- kwb.utils::hsDelEmptyCols(bodylines)        
  }
  
  valueColumns <- ! (names(bodylines) %in% c("Datum", "Uhrzeit"))
  
  body <- .allDataFrameColumnsToNumeric(bodylines[, valueColumns])
  
  body <- data.frame(myDateTime = myDateTime, body, stringsAsFactors = FALSE)
  
  structure(body, metadata = metadata)
}

# .splitHeaderLine -------------------------------------------------------------

.splitHeaderLine <- function(headerLine, sep)
{
  utils::read.table(
    textConnection(headerLine), sep = sep, colClasses = "character", 
    stringsAsFactors = FALSE
  )[1, ]
}

# .delEmptyColumnsToTheRight ---------------------------------------------------

.delEmptyColumnsToTheRight <- function(bodylines)
{
  all.na <- sapply(seq_len(ncol(bodylines)), FUN = function(i) {
    all(is.na(bodylines[, i]))
  })
  
  bodylines[, seq_len(max(which(! all.na)))]
}

# .allDataFrameColumnsToNumeric ------------------------------------------------

.allDataFrameColumnsToNumeric <- function(dframe)
{
  x <- as.matrix(dframe)
  x <- gsub(",", ".", x)
  x <- apply(x, 2, as.numeric)
  
  as.data.frame(x)  
}

# .indicesOfContext ------------------------------------------------------------

.indicesOfContext <- function(indices, before = 1, after = 1)
{
  as.integer(sapply(
    indices - before, seq, by = 1, length.out = before + after + 1
  ))
}

# .getMetadataFromMetalines ----------------------------------------------------

.getMetadataFromMetalines <- function(metalines)
{
  metadata <- list()
  
  # Remove empty lines
  metalines <- metalines[metalines != ""]  
  
  x <- .findAndSplitLine("^NIVUS", metalines, "\t")
  metadata$monitoringPoint <- x[length(x)]
  
  x <- .findAndSplitLine("^CPU32", metalines, "\t")
  metadata$version <- x[2]
  metadata$versionDate <- x[3]
  metadata$flash <- x[4]
  metadata$dsp <- x[5]
  
  x <- .findAndSplitLine("^Datum", metalines, "\t")
  metadata$date <- x[2]
  metadata$filepath <- x[3]
  
  x <- .findAndSplitLine("Fenster min", metalines, "\t")
  metadata$window.min <- x[length(x)]
  
  x <- .findAndSplitLine("Fenster max", metalines, "\t")
  metadata$window.max <- x[length(x)]
  
  metadata
}

# .findAndSplitLine ------------------------------------------------------------

.findAndSplitLine <- function(pattern, metalines, split)
{
  textline <- grep(pattern, metalines, value = TRUE)
  
  if (length(textline) == 0) {
    
    stop(sprintf("Could not find a line matching pattern \"%s\"", pattern))
  }
  
  if (length(textline) > 1) {
    
    stop(sprintf("More than one line matches the pattern \"%s\"", pattern))
  }
  
  strsplit(textline, split)[[1]]
}
