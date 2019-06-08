# getExampleTimestamps ---------------------------------------------------------

#' Timestamp Strings from Example Looger Files
#' 
#' @export
#' 
getExampleTimestamps <- function()
{
  files <- exampleLoggerFiles()
  
  raw_contents <- lapply(files, readLines)
  
  list(
    get_timestamps_1(x = raw_contents[[1]]),
    get_timestamps_1(x = raw_contents[[2]]),
    get_timestamps_2(x = raw_contents[[3]]),
    get_timestamps_2(x = raw_contents[[4]]),
    # skip 5 and 6
    get_timestamps_3(x = raw_contents[[7]]),
    get_timestamps_3(x = raw_contents[[8]]),
    get_timestamps_4(x = raw_contents[[9]]),
    get_timestamps_4(x = raw_contents[[10]]),
    get_timestamps_4(x = raw_contents[[11]]),
    get_timestamps_4(x = raw_contents[[12]]),
    # skip 13 (sampler)
    get_timestamps_5(x = raw_contents[[14]]),
    get_timestamps_5(x = raw_contents[[15]]),
    get_timestamps_6(x = raw_contents[[16]]),
    get_timestamps_7(x = raw_contents[[17]])
  )
}

# get_timestamps_7 -------------------------------------------------------------
get_timestamps_7 <- function(x)
{
  timestamps <- grep("\\d\\d:\\d\\d:\\d\\d$", x, value = TRUE)
  timestamps[nchar(timestamps) == 17]
}

# get_timestamps_6 -------------------------------------------------------------
get_timestamps_6 <- function(x)
{
  content <- utils::read.table(text = x[-(1:3)], sep = "\t")
  kwb.utils::pasteColumns(content, columns = c("V2", "V3"))
}

# get_timestamps_5 -------------------------------------------------------------
get_timestamps_5 <- function(x)
{
  unlist(lapply(strsplit(x, ";"), "[", 1))[-1]
}

# get_timestamps_4 -------------------------------------------------------------
get_timestamps_4 <- function(x)
{
  start_row <- grep("^Datum\tUhrzeit", x, useBytes = TRUE)
  
  content <- utils::read.table(
    comment.char = "", text = x[- seq_len(start_row)], sep = "\t", fill = TRUE,
    stringsAsFactors = FALSE
  )
  
  content <- content[! grepl("^#", content$V1), ]
  
  timestamps <- kwb.utils::pasteColumns(content, columns = c("V1", "V2"))
  
  timestamps[grepl(":", timestamps)]
}

# get_timestamps_3 -------------------------------------------------------------
get_timestamps_3 <- function(x)
{
  start_row <- grep("^Date and Time", x)
  end_row <- grep("^Log Data", x)
  text <- x[(start_row + 1):(end_row - 3)]
  utils::read.table(text = text, sep = ";", stringsAsFactors = FALSE)$V1
}

# get_timestamps_2 -------------------------------------------------------------
get_timestamps_2 <- function(x)
{
  utils::read.table(text = x, sep = ";", stringsAsFactors = FALSE)[, 1]
}

# get_timestamps_1 -------------------------------------------------------------
get_timestamps_1 <- function(x)
{
  start_row <- grep("^\\\\DATA", x)
  end_row <- grep("^\\\\END\\s*$", x)
  
  kwb.utils::pasteColumns(columns = c("V2", "V3"), utils::read.table(
    text = x[(start_row+3):(end_row-1)]
  ))
}
