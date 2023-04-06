test_that("readLogger_FLEXIM_F601() works", {

  f <- kwb.logger:::readLogger_FLEXIM_F601
  
  expect_error(capture.output(f()))
  
})
