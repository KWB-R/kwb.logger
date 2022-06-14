test_that(".collapse() works", {
   f <- kwb.logger:::.collapse
   
   expect_identical(f(1), "1")
   expect_identical(f(1:2), "12")
   expect_identical(f(c("a", "b")), "ab")
   expect_identical(f(c(TRUE, FALSE)), "TRUEFALSE")
   expect_identical(f(LETTERS[1:8]), "ABCDEFGH")
})
