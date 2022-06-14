test_that(".assignment() works", {

  f <- kwb.logger:::.assignment

  expect_true(is.function(f()))
})
