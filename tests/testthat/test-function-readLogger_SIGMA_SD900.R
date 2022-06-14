test_that("readLogger_SIGMA_SD900() works", {

  f <- kwb.logger:::readLogger_SIGMA_SD900
  
  expect_error(f())

})
