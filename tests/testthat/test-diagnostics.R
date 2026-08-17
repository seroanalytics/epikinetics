test_that("chain diagnostics explain non-finite E-BFMI", {
  sampler <- list(
    num_divergent = c(0, 2),
    num_max_treedepth = c(4, 0),
    ebfmi = c(NaN, 0.6)
  )
  energy <- array(
    c(rep(10, 5), seq(10, 14)),
    dim = c(5, 2, 1),
    dimnames = list(NULL, NULL, "energy__")
  )
  result <- epikinetics:::epikinetics_chain_diagnostics(sampler, energy)

  expect_equal(result$unique_energy_values, c(1L, 5L))
  expect_equal(result$energy_variance[1], 0)
  expect_match(result$status[1], "constant energy")
  expect_equal(result$status[2], "divergences")
})
