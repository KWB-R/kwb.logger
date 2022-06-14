test_that(".nonEolParser() works", {

  f <- kwb.logger:::.nonEolParser
  
  expect_true(is.function(f()))
})
