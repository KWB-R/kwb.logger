#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("readLogger_FLUKE_1730() works", {

  expect_error(
    kwb.logger:::readLogger_FLUKE_1730(filepath = 1)
    # 'file' must be a character string or connection
  )
   expect_error(
    kwb.logger:::readLogger_FLUKE_1730(filepath = "a")
    # kann Verbindung nicht ?ffnen
  )
   expect_error(
    kwb.logger:::readLogger_FLUKE_1730(filepath = c("a", "b"))
    # ung?ltiges 'description' Argument
  )

})

