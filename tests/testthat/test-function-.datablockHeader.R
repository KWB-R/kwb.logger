test_that(".datablockHeader() works", {

  f <- kwb.logger:::.datablockHeader
  
  expect_true(is.function(f()))
})
