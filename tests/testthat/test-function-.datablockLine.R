test_that(".datablockLine() works", {

  f <- kwb.logger:::.datablockLine

  expect_true(is.function(f()))
})
