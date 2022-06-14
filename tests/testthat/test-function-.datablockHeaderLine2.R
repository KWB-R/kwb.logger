test_that(".datablockHeaderLine2() works", {

  f <- kwb.logger:::.datablockHeaderLine2

  expect_true(is.function(f()))
})
