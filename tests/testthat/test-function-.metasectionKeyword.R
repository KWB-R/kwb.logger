test_that(".metasectionKeyword() works", {

  f <- kwb.logger:::.metasectionKeyword
  
  expect_true(is.function(f()))
  
})