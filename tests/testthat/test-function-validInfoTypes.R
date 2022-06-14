test_that("validInfoTypes() works", {

  f <- kwb.logger:::validInfoTypes

  expect_identical(f(), c("actions", "times"))
})
