test_that(".datablockHeaderLine1() works", {

  f <- kwb.logger:::.datablockHeaderLine1

  expect_true(is.function(f()))
})
