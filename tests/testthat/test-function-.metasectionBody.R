test_that(".metasectionBody() works", {

  f <- kwb.logger:::.metasectionBody

  expect_true(is.function(f()))
})
