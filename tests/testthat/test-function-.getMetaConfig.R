test_that(".getMetaConfig() works", {

  result <- kwb.logger:::.getMetaConfig()

  expect_type(result, "list")
  expect_identical(gsub(" ", "_", as.character(result)), names(result))
})
