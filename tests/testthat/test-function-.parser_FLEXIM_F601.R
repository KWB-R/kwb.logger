test_that(".parser_FLEXIM_F601() works", {

  f <- kwb.logger:::.parser_FLEXIM_F601

  expect_true(is.function(f()))
})
