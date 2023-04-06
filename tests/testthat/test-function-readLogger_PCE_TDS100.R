test_that("readLogger_PCE_TDS100() works", {

  f <- kwb.logger:::readLogger_PCE_TDS100
  
  expect_error(f())

})
