#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".trim() works", {

  kwb.logger:::.trim(x = 1)
   kwb.logger:::.trim(x = 1:2)
   kwb.logger:::.trim(x = "a")
   kwb.logger:::.trim(x = c("a", "b"))
   kwb.logger:::.trim(x = TRUE)
   kwb.logger:::.trim(x = FALSE)
   kwb.logger:::.trim(x = as.POSIXct("2018-06-03 23:50:00"))
   kwb.logger:::.trim(x = list(key = c("a", "b"), value = 1:2))

})

