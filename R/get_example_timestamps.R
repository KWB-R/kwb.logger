# getExampleTimestamps ---------------------------------------------------------

#' Timestamp Strings from Example Looger Files
#' 
#' @export
getExampleTimestamps <- function()
{
  path <- extdataFile()
  rel_paths <- dir(path, recursive = TRUE)
  X <- lapply(file.path(path, rel_paths), readLines, warn = FALSE)
  names(X) <- rel_paths
  
  list(
    get_timestamps_flexim(x = X[["FLEXIM/example_FLEXIM_F601.txt"]]),
    get_timestamps_flexim(x = X[["FLEXIM/example_FLEXIM_F601_short.txt"]]),
    get_timestamps_fluke(x = X[["FLUKE/example_FLUKE_1730_1.txt"]]),
    get_timestamps_fluke(x = X[["FLUKE/example_FLUKE_1730_2.txt"]]),
    # skip 5: FLUKE_1730_3.fca               
    # skip 6: example_FLUKE_1730_4.fca
    get_timestamps_aquatroll(x = X[["InSituInc/example_InSituInc_Aquatroll.csv"]]),
    get_timestamps_aquatroll(x = X[["InSituInc/example_InSituInc_Aquatroll_600.csv"]]),
    get_timestamps_nivus(x = X[["NIVUS/example_NIVUS_PCM4.TXT"]]),
    get_timestamps_nivus(x = X[["NIVUS/example_NIVUS_PCM4_ALT.TXT"]]),
    get_timestamps_nivus(x = X[["NIVUS/example_NIVUS_PCM4_NEU.TXT"]]),
    get_timestamps_nivus(x = X[["NIVUS/example_NIVUS_PCM4_STR.TXT"]]),
    # skip 13: example_Ori_BasicEx1.csv
    get_timestamps_ori_mlog(x = X[["Ori/example_Ori_MLog_1.csv"]]),
    get_timestamps_ori_mlog(x = X[["Ori/example_Ori_MLog_2.csv"]]),
    get_timestamps_pce_pa8000(x = X[["PCE/example_PCE_PA8000.txt"]]),
    get_timestamps_pce_tds100(x = X[["PCE/example_PCE_TDS100.log"]])
  )
}

# get_timestamps_pce_tds100 ----------------------------------------------------
get_timestamps_pce_tds100 <- function(x)
{
  timestamps <- grep("\\d\\d:\\d\\d:\\d\\d$", x, value = TRUE)
  timestamps[nchar(timestamps) == 17]
}

# get_timestamps_pce_pa8000 ----------------------------------------------------
get_timestamps_pce_pa8000 <- function(x)
{
  content <- utils::read.table(text = x[-(1:3)], sep = "\t")
  kwb.utils::pasteColumns(content, columns = c("V2", "V3"))
}

# get_timestamps_ori_mlog ------------------------------------------------------
get_timestamps_ori_mlog <- function(x)
{
  unlist(lapply(strsplit(x, ";"), "[", 1))[-1]
}

# get_timestamps_nivus ---------------------------------------------------------
get_timestamps_nivus <- function(x)
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

# get_timestamps_aquatroll -----------------------------------------------------
get_timestamps_aquatroll <- function(x)
{
  start_row <- grep("^Date and Time", x)
  end_row <- grep("^Log Data", x)
  text <- x[(start_row + 1):(end_row - 3)]
  utils::read.table(text = text, sep = ";", stringsAsFactors = FALSE)$V1
}

# get_timestamps_fluke ---------------------------------------------------------
get_timestamps_fluke <- function(x)
{
  sapply(strsplit(x, ";"), "[", 1L)[-1L]
}

# get_timestamps_flexim --------------------------------------------------------
get_timestamps_flexim <- function(x)
{
  start_row <- grep("^\\\\DATA", x)
  end_row <- grep("^\\\\END\\s*$", x)
  
  kwb.utils::pasteColumns(columns = c("V2", "V3"), utils::read.table(
    text = x[(start_row+3):(end_row-1)]
  ))
}
