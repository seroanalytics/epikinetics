test_that("One of data or file required", {
  expect_error(sam$new(), "One of 'data' or 'file_path' must be provided")
})

test_that("Only one of data or file can be provided", {
  expect_error(sam$new(data = data.table::data.table(), file_path = "some/where"),
               "Only one of 'data' or 'file_path' should be provided")
})

test_that("Priors must be of type 'sam_priors'", {
  expect_error(sam$new(priors = list(t0 = 1, t2 = 3)),
               "'priors' must be of type 'sam_priors'")
})

test_that("Time type must be 'absolute' or 'relative'", {
  expect_error(sam$new(time_type = "bad"),
               "'time_type' must be one of 'relative' or 'absolute'")
})
