test_that(".metasectionAppendix() works", {

  f <- kwb.logger:::.metasectionAppendix

  expect_true(is.function(f()))  
})
