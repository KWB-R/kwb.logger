test_that(".datablockBeginTag() works", {

  f <- kwb.logger:::.datablockBeginTag

  expect_true(is.function(f()))
})
