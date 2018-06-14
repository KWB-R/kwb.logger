# .defaultTimeFormat -----------------------------------------------------------

.defaultTimeFormat <- function(version = "v1")
{
  versionToFormat <- list(
    "v1" = "%H:%M:%S",
    "v2" = "%d.%m.%Y%H:%M:%S",
    "v3" = "%y-%m-%d %H:%M:%S",
    "v4" = "%Y/%m/%d %H:%M:%S",
    "v5" = "%d.%m.%Y",
    "v6" = "%H:%M %m/%d/%Y",
    "v7" = "%d.%m.%Y %H:%M",
    "v8" = "%d.%m.%Y %H:%M:%S"
  )
  
  versionToFormat[[version]]
}
