test_that(".metasection() works", {

  f <- kwb.logger:::.metasection

  expect_true(is.function(f()))
})
