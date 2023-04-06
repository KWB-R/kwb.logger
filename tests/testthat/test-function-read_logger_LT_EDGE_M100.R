test_that("read_logger_LT_EDGE_M100() works", {

  f <- kwb.logger:::read_logger_LT_EDGE_M100
  
  expect_error(f())
})
