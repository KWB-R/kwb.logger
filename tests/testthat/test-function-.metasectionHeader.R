test_that(".metasectionHeader() works", {

  f <- kwb.logger:::.metasectionHeader

  expect_true(is.function(f()))
})
