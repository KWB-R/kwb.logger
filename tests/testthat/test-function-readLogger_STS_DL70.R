test_that("readLogger_STS_DL70() works", {

  f <- kwb.logger:::readLogger_STS_DL70
  
  expect_error(f())
})
