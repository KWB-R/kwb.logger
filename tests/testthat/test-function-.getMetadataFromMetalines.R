#
# This test file has been generated by kwb.test::create_test_files()
#

test_that(".getMetadataFromMetalines() works", {

  expect_error(
    kwb.logger:::.getMetadataFromMetalines(metalines = 1)
    # Could not find a line matching pattern "^NIVUS"
  )
   expect_error(
    kwb.logger:::.getMetadataFromMetalines(metalines = as.POSIXct("2018-06-03 23:50:00"))
    # character string is not in a standard unambiguous format
  )

})

