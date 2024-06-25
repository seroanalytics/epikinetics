test_that("One of data or file required to run model", {
  expect_error(run_model(), "One of 'data' or 'file_path' must be provided")
})

test_that("Only one of data or file can be passed to run model", {
  expect_error(run_model(data = data.table::data.table(), file_path = "some/where"),
               "Only one of 'data' or 'file_path' should be provided")
})
