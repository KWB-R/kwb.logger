#
# This test file has been generated by kwb.test::create_test_files()
#

test_that("readLogger_SIGMA_SD900() works", {

  expect_error(
    kwb.logger:::readLogger_SIGMA_SD900(filepath = 1)
    # ung?ltiges 'file' Argument
  )
   expect_error(
    kwb.logger:::readLogger_SIGMA_SD900(filepath = "a")
    # No such file: a
  )
   expect_error(
    kwb.logger:::readLogger_SIGMA_SD900(filepath = c("a", "b"))
    # No such file: ab
  )

})

