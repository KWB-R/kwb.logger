test_that(".endtag() works", {

  f <- kwb.logger:::.endtag

  f()
  
  expect_true(is.function(f()))
})
