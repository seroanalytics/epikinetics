test_that("Can convert log scale in R", {
  traj <- data.table::fread(test_path("testdata", "trajectories.csv"))
  res <- convert_log_scale_inverse(
    traj, vars_to_transform = "mu")

  expect_equal(res$mu[[1]], 5 * 2^traj$mu[[1]])
})

test_that("Can convert log scale in Cpp", {
  traj <- data.table::fread(test_path("testdata", "trajectories.csv"))
  rescpp <- convert_log_scale_inverse_cpp(
    traj, vars_to_transform = "mu")

  expect_equal(rescpp$mu[[1]], 5 * 2^(traj$mu[[1]] + 1))
  expect_equal(names(rescpp), names(traj))
})
