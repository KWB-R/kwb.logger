test_that(".trim() works", {
   
   f <- kwb.logger:::.trim
   
   expect_error(f())
   expect_identical(f("abc"), "abc")
   expect_identical(f(" abc "), "abc")
   expect_identical(f("  ab  c  "), "ab  c")
})
