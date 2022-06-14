test_that(".datablock() works", {

  f <- kwb.logger:::.datablock

  expect_true(is.function(f()))
})
