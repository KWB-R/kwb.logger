test_that("readLogger_PCE_PA8000() works", {

  f <- kwb.logger:::readLogger_PCE_PA8000
  
  expect_error( f())

})
