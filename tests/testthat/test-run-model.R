mock_model <- function(name, package) {
  list(sample = function(x, ...)  list(...))
}

local_mocked_bindings(
  stan_package_model = mock_model, .package = "instantiate"
)

test_that("Can run model with arguments", {
  res <- scova$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                 priors = scova_priors())$fit(chains = 4,
                                            parallel_chains = 4,
                                            iter_warmup = 100,
                                            iter_sampling = 400,
                                            threads_per_chain = 4)
  expect_equal(names(res), c("chains", "parallel_chains", "iter_warmup", "iter_sampling", "threads_per_chain"))
})
