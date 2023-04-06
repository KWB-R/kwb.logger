test_that("readLogger_NIVUS_PCM4_2() works", {

  f <- kwb.logger:::readLogger_NIVUS_PCM4_2
  
  expect_error(f())
  
})
