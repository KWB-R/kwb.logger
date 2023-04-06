test_that(".starttag() works", {

  f <- kwb.logger:::.starttag

  expect_true(is.function(f()))
})
