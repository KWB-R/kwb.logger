#library(kwb.logger)
test_that(".getMeta_SIGMA_SD900() works", {

  f <- kwb.logger:::.getMeta_SIGMA_SD900
  
  expect_error(f())
  
  file <- extdataFile("SIGMA/example_SIGMA_SD900.csv")
  
  result <- f(file, "TIME STAMP", sep = ",")
  
  expect_type(result, "list")

  expect_identical(names(result), c("1" = "SITE_ID", 
                                    "2" = "SN", 
                                    "3" = "DEVICE_ID", 
                                    "4" = "MFG_ID"
  ))
})
