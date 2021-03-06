#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".renameWithValidCaptions() works", {

  kwb.logger:::.renameWithValidCaptions(x = 1, captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = 1, captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = 1, captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = 1, captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = 1, captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = 1, captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = 1, captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = 1:2, captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = "a", captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = c("a", "b"), captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = TRUE, captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = FALSE, captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = as.POSIXct("2018-06-03 23:50:00"), captions = list(key = c("a", "b"), value = 1:2))
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = 1)
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = 1:2)
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = "a")
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = c("a", "b"))
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = TRUE)
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = FALSE)
   kwb.logger:::.renameWithValidCaptions(x = list(key = c("a", "b"), value = 1:2), captions = list(key = c("a", "b"), value = 1:2))
   expect_error(
    kwb.logger:::.renameWithValidCaptions(x = 1, captions = as.POSIXct("2018-06-03 23:50:00"))
    # character string is not in a standard unambiguous format
  )

})

