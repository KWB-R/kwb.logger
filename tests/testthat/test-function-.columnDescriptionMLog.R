test_that(".columnDescriptionMLog() works", {

  f <- kwb.logger:::.columnDescriptionMLog
  
  expect_type(f(), "list")
})
