test_that(".assignmentValue() works", {

  f <- kwb.logger:::.assignmentValue
  expect_true(is.function(f()))
})
