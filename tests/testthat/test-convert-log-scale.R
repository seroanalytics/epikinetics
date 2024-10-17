test_that("Can convert to and from log scale in R", {
  inputs <- data.table::fread(test_path("testdata", "testdata.csv"))
  log_inputs <- convert_log2_scale(inputs, "me", simplify_limits = FALSE)
  unlog_inputs <- convert_log2_scale_inverse(log_inputs, "me")

  expect_equal(inputs, unlog_inputs)
})

test_that("Can convert from log scale in R", {
  inputs <- data.table::fread(test_path("testdata", "testdata.csv"))
  res <- convert_log2_scale_inverse(
    inputs, vars_to_transform = c("me", "lo"))

  expect_equal(res$me, 5 * 2^inputs$me)
  expect_equal(res$lo, 5 * 2^inputs$lo)
  expect_equal(res$hi, inputs$hi)
  expect_equal(names(res), names(inputs))
})

test_that("Can convert from log scale in Cpp", {
  inputs <- data.table::fread(test_path("testdata", "testdata.csv"))
  rescpp <- convert_log2_scale_inverse_cpp(
    inputs, vars_to_transform = c("me", "lo"))

  expect_equal(rescpp$me, 5 * 2^(inputs$me))
  expect_equal(rescpp$lo, 5 * 2^(inputs$lo))
  expect_equal(rescpp$hi, inputs$hi)
  expect_equal(names(rescpp), names(inputs))
})
