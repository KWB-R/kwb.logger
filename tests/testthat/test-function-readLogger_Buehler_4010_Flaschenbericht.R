test_that("readLogger_Buehler_4010_Flaschenbericht() works", {

  f <- kwb.logger:::readLogger_Buehler_4010_Flaschenbericht
  
  expect_error(f())

})
