test_that(".findAndSplitLine() works", {
  
  f <- kwb.logger:::.findAndSplitLine
  
  expect_error(f())
  expect_error(f("abc", c("abc", "abcd")), "More than one line")
  expect_identical(f("abc", c("abc", "def"), split = ""), c("a", "b", "c"))
  expect_identical(f("a,", c("abc", "a,b,c,d,e"), split = ","), letters[1:5])
})
