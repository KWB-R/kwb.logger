test_that(".metablock() works", {

  f <- kwb.logger:::.metablock

  expect_true(is.function(f()))
})
