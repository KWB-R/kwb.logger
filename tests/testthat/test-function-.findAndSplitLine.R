#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".findAndSplitLine() works", {

  expect_error(
    kwb.logger:::.findAndSplitLine(pattern = 1, metalines = 1)
    # Argument "split" fehlt (ohne Standardwert)
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = 1, metalines = "a")
    # Could not find a line matching pattern "1"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = 1:2, metalines = "a")
    # Could not find a line matching pattern "1"Could not find a line matching pattern "2"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = "a", metalines = 1)
    # Could not find a line matching pattern "a"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = c("a", "b"), metalines = 1)
    # Could not find a line matching pattern "a"Could not find a line matching pattern "b"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = TRUE, metalines = 1)
    # Could not find a line matching pattern "TRUE"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = FALSE, metalines = 1)
    # Could not find a line matching pattern "FALSE"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = as.POSIXct("2018-06-03 23:50:00"), metalines = 1)
    # Could not find a line matching pattern "2018-06-03 23:50:00"
  )
   expect_error(
    kwb.logger:::.findAndSplitLine(pattern = list(key = c("a", "b"), value = 1:2), metalines = 1)
    # Could not find a line matching pattern "c("a", "b")"Could not find a line matching pattern "1:2"
  )

})

