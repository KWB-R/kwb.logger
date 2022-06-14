test_that("readLogger_Ori_MLog() works", {

  f <- kwb.logger:::readLogger_Ori_MLog
  
  expect_error(f())

})
