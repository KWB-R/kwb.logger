test_that(".datablockEndTag() works", {

  f <- kwb.logger:::.datablockEndTag

  expect_true(is.function(f()))
})
