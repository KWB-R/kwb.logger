test_that(".datablockBody() works", {

  f <- kwb.logger:::.datablockBody

  expect_true(is.function(f()))
})
