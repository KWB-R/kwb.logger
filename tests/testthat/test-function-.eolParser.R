test_that(".eolParser() works", {

  f <- kwb.logger:::.eolParser

  expect_true(is.function(f()))
})
