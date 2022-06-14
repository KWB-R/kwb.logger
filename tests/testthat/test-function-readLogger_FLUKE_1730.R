test_that("readLogger_FLUKE_1730() works", {

  f <- kwb.logger:::readLogger_FLUKE_1730
  
  expect_error(f())
})
