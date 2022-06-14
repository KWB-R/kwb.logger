test_that("read_aquatroll_data() works", {

  f <- kwb.logger:::read_aquatroll_data
  
  expect_error(f())

})
