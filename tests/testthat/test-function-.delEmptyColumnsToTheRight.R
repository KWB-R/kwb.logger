test_that(".delEmptyColumnsToTheRight() works", {

  f <- kwb.logger:::.delEmptyColumnsToTheRight
  
  expect_error(suppressWarnings(f(1)))

  m <- matrix(1:12, nrow = 3L)

  expect_identical(m, f(m))
  
  m[, 3:4] <- NA
  
  expect_identical(m[, 1:2], f(m))
})
