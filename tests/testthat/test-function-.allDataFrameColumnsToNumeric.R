#library(testthat)

test_that(".allDataFrameColumnsToNumeric() works", {

  f <- kwb.logger:::.allDataFrameColumnsToNumeric
  
  expect_error(f(1))
  expect_warning(f("a"))
  expect_warning(f(c("a", "b")))
  
  dframe <- data.frame(
    num = c("1.2", "3.4", "5.6"), 
    chr = c("A", "B", "C")
  )

  expect_warning(result <- f(dframe))
              
  expect_true(is.numeric(result$num))
  expect_true(all(is.na(result$chr)))
})

