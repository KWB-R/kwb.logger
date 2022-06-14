test_that(".read_Flaschenbericht.raw() works", {

  f <- kwb.logger:::.read_Flaschenbericht.raw
  
  expect_error(f())

})
