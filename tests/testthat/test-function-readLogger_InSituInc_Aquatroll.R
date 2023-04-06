test_that("readLogger_InSituInc_Aquatroll() works", {

  f <- kwb.logger:::readLogger_InSituInc_Aquatroll
  
  expect_error(f())
  
})
