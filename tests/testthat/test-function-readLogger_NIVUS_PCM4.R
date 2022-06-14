test_that("readLogger_NIVUS_PCM4() works", {

  f <- kwb.logger:::readLogger_NIVUS_PCM4
  
  expect_error(f())

})

