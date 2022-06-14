test_that("read_logger_LT_EDGE_M100_csv() works", {

  f <- kwb.logger:::read_logger_LT_EDGE_M100_csv
  
  expect_error(f())
  
})
