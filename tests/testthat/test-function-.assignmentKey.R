test_that(".assignmentKey() works", {

  f <- kwb.logger:::.assignmentKey

  expect_true(is.function(f()))
})
