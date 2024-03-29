# readLogger_PCE_TDS100 --------------------------------------------------------

#' Read Logger File from PCE TDS100
#' 
#' Read logger file of flow meter TDS 100 by PCE Deutschland GmbH
#' 
#' @param txt full path to text file generated by logger
#' @param timeformat Date and time format used in the file. 
#'   Default: "\%y-\%m-\%d \%H:\%M:\%S"
#' @return data frame with columns \emph{tstamp}, \emph{flow}, \emph{flowunit}, 
#'   \emph{vel}, \emph{velunit}, \emph{UP}, \emph{DN}, \emph{Q}. Duplicate lines
#'   are removed. The data frame has an attribute \emph{metadata} containing the
#'   meta data of the file, as returned by \code{.getMetadata}
#' 
#' @references \url{http://www.warensortiment.de/bedienung/ba_durchflussmessgeraet-pce-tds100h_de-v-1-1.pdf}
#' @references \url{http://www.industrial-needs.com/manual/manual-pce-tds-100h.pdf}
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' # set path to example file (contained in this package)
#' file_1 <- extdataFile("PCE/example_PCE_TDS100.log")
#' file_2 <- extdataFile("PCE/example_PCE_TDS100_noMeta.log")
#' 
#' # read a file containing metadata
#' x1 <- readLogger_PCE_TDS100(file_1) # warnings about duplicate timestamps
#'   
#' # read a file not containing metadata
#' x2 <- readLogger_PCE_TDS100(file_2) # warning about missing meta data
#'   
#' # examine the structures of the results
#' str(x1)
#' str(x2)
#'   
#' # get meta data from attribute "metadata"
#' kwb.utils::getAttribute(x1, "metadata")
#' }
#' 
readLogger_PCE_TDS100 <- function(txt, timeformat = .defaultTimeFormat("v3"))
{
  mylines <- readLines(txt)
  
  timeInfo <- .getValidTimes(mylines, timeformat=timeformat)
  
  syslines <- grep("^SYS:", mylines)
  flowlines <- grep("^Flow", mylines)
  vellines <- grep("^Vel:", mylines)
  uplines <- grep("^UP:", mylines)
  
  sys <- .splitValueAndUnit(mylines[syslines], valueindex = 2, unitindex = NA)
  flow <- .splitValueAndUnit(mylines[flowlines])
  vel <- .splitValueAndUnit(mylines[vellines])    
  
  fields <- strsplit(mylines[uplines], "\\s+")
  ups <- sapply(fields, "[[", 1)
  dns <- sapply(fields, "[[", 2)
  Qs <-  sapply(fields, "[[", 3)
  
  UPvalues <- as.numeric(.getValuesAfterDelimiter(ups, ":", "UP"))
  DNvalues <- as.numeric(.getValuesAfterDelimiter(dns, ":", "DN"))
  Qvalues <- as.numeric(.getValuesAfterDelimiter(Qs, "=", "Q"))
  
  upDnQ <- data.frame(
    myDateTime = timeInfo$tstamps[timeInfo$tstampIndex[uplines]], 
    UP = UPvalues, 
    DN = DNvalues, 
    Q = Qvalues, 
    stringsAsFactors = FALSE
  )
  
  sys  <- data.frame(
    myDateTime = timeInfo$tstamps[timeInfo$tstampIndex[syslines]], 
    sys = sys$values,
    stringsAsFactors = FALSE
  )
  
  flow <- data.frame(
    myDateTime=timeInfo$tstamps[timeInfo$tstampIndex[flowlines]], 
    flow = as.numeric(flow$values), 
    flowunit = flow$units,
    stringsAsFactors = FALSE
  )
  
  vel  <- data.frame(
    myDateTime = timeInfo$tstamps[timeInfo$tstampIndex[vellines]], 
    vel = as.numeric(vel$values), 
    velunit = vel$units,
    stringsAsFactors = FALSE
  )
  
  result <- data.frame(
    myDateTime = sort(unique(timeInfo$tstamps)), 
    stringsAsFactors = FALSE
  )
  
  result <- merge(result, sys, all.x = TRUE)
  result <- unique(merge(result, flow, all.x = TRUE))
  result <- unique(merge(result, vel, all.x = TRUE))
  result <- unique(merge(result, upDnQ, all.x = TRUE))
  
  structure(result, metadata = .getMetadata(mylines))
}

# .getValidTimes ---------------------------------------------------------------

#' Read Times from Given Lines
#' 
#' @param mylines vector of character, each representing a line of a text file
#' @param timeformat time format to be looked for in \emph{mylines}. Default:
#' "\%y-\%m-\%d \%H:\%M:\%S"
#' @return list with elements \emph{tstamps} (timestamps as text in ISO format)
#'   and \emph{tstampIndex} (contains for each text line the index in 
#'   \emph{tstamps} at which the timestamp corresponding to the line is found.
.getValidTimes <- function(mylines, timeformat = "%y-%m-%d %H:%M:%S")
{  
  tpattern <- sprintf("^%s", kwb.datetime:::timeFormatToRegex(timeformat))
  timelines <- grep(tpattern, mylines)
  
  tstamps.orig <- mylines[timelines]  
  
  tstamps.iso <- kwb.datetime::reformatTimestamp(tstamps.orig, timeformat)
  
  notimelines <- timelines[is.na(tstamps.iso)]
  
  if (length(notimelines) > 0) {
    
    warning(paste(collapse = "\n", c(
      "The following lines could not be converted to times:",
      sprintf("line %5d: >>>%s<<<", notimelines, mylines[notimelines])
    )))
  }
  
  timelines <- setdiff(timelines, notimelines)
  tstamps <-   tstamps.iso[!is.na(tstamps.iso)]
  
  timeline.1 <- timelines[1]
  timeline.n <- utils::tail(timelines, 1)
  
  times <- c(timeline.1, diff(timelines), length(mylines) - timeline.n)
  tstampIndex <- rep(c(NA, seq_along(timelines)), times)
  
  stopifnot(length(tstampIndex) == length(mylines))
  
  if (length(tstamps) != length(unique(tstamps))) {
    
    agg <- stats::aggregate(tstamps, by = list(tstamps = tstamps), length)
    dups <- agg$tstamps[agg$x > 1]
    
    warning(sprintf(
      "There are %d duplicate timestamps:\n  %s\n", 
      length(dups), paste(dups, collapse = "\n  ")
    ))
  }
  
  list(tstamps = tstamps, tstampIndex = tstampIndex)
}

# .splitValueAndUnit -----------------------------------------------------------

.splitValueAndUnit <- function(
  valueAndUnit, valueindex = 2, unitindex = 3, sep = "\\s+"
)
{
  fields <- strsplit(valueAndUnit, sep)
  
  vlist <- NULL
  ulist <- NULL
  
  if (! is.na(valueindex)) {
    
    vlist <- list(values = sapply(fields, "[[", valueindex))
  }
  
  if (! is.na(unitindex)) {
    
    ulist <- list(units = sapply(fields, "[[", unitindex))    
  }
  
  c(vlist, ulist)
}

# .getValuesAfterDelimiter -----------------------------------------------------

.getValuesAfterDelimiter <- function(namevarpairs, sep, varname)
{
  fields <- strsplit(namevarpairs, sep)
  
  stopifnot(all(sapply(fields, "[[", 1) == varname))
  
  sapply(fields, "[[", 2)  
}

# .getMetadata -----------------------------------------------------------------

#' Get Meta Data from File Lines
#'
#' Lines between lines containing "Outer Diameter" and  "Txducers Spacing", 
#' respectively, are extrated.
#' 
#' @param mylines vector of character, each representing a line of a text file  
#' @param beginPattern pattern matching the line in which meta data block begins
#' @param endPattern pattern matching the line in which meta data block ends
#' @param transpose  (default: TRUE)
#' @return meta data read from \emph{mylines} in the form of a 
#'   data frame with one row containing the meta data values in the different
#'   columns (transpose == FALSE) or a data frame with columns \emph{names} 
#'   and \emph{values} (transpose == TRUE)
#' 
.getMetadata <- function(
  mylines, beginPattern = "Outer Diameter", endPattern = "Txducers Spacing",
  transpose = TRUE
)
{
  specialBegin <- grep(beginPattern, mylines)
  specialEnd <- grep(endPattern, mylines)
  
  if (! length(specialBegin)) {
    
    warning("Could not find begin of meta data: ", beginPattern)
    return()
  }
  
  if (! length(specialEnd)) {
    
    warning("Could not find end of meta data: ", endPattern)
    return()
  }
  
  mynames <- mylines[seq(specialBegin, specialEnd, 2)]
  myvalues <- mylines[seq(specialBegin + 1, specialEnd + 1, 2)]
  
  x <- data.frame(names = mynames, values = myvalues, stringsAsFactors = FALSE)  
  
  if (transpose) {
    
    x <- stats::setNames(
      object = data.frame(as.list(x$values), stringsAsFactors = FALSE), 
      nm = kwb.utils::substSpecialChars(x$names)
    )
  }
  
  x
}
