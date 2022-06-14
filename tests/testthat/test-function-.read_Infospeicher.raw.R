test_that(".read_Infospeicher.raw() works", {

  f <- kwb.logger:::.read_Infospeicher.raw
  
  expect_error(f())
})
