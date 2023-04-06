test_that("reset_locale() works", {

  f <- kwb.logger:::reset_locale
  
  expect_error(f())
})
