test_that(".defaultTimeFormat() works", {

  f <- kwb.logger:::.defaultTimeFormat

  expect_length(f(), 1L)
  expect_type(f(), "character")
})
