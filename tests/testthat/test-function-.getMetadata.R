test_that(".getMetadata() works", {
   
   f <- kwb.logger:::.getMetadata
   
   expect_error(f())
})
