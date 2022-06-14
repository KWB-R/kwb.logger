test_that(".spacesToZero() works", {

  f <- kwb.logger:::.spacesToZero

  expect_error(f())
  expect_identical(f("a b"), "a0b")
  expect_identical(f("a0b"), "a0b")
  expect_identical(f("a b c "), "a0b0c0")
})
