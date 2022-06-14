test_that(".nonEmptyLine() works", {

  f <- kwb.logger:::.nonEmptyLine

  expect_true(is.function(f()))
})
