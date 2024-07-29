test_that("Can recover single covariate", {
  dat <- data.table(infection_history = c("Infection naive", "Previously infected"),
                    value = 1,
                    last_vax_type = c("a", "b", "c", "d"))
  mm <- stats::model.matrix(~0 + infection_history, dat)
  lookup <- build_covariate_lookup_table(dat, mm, c("infection_history"))

  values <- data.table(p = c(1, 2, 2, 1), value = c(1, 2, 3, 4))
  res <- na.omit(values[lookup, on = "p"], colnames(values))[, `:=`(p = NULL)]
  expect_equal(names(res), c("value", "infection_history"))
  expect_equal(res$infection_history, c("Infection naive", "Infection naive",
                                        "Previously infected", "Previously infected"))
  expect_equal(res$value, c(1, 4, 2, 3))
})

test_that("Can recover multiple covariate from interaction term", {
  dat <- data.frame(infection_history = c("Infection naive", "Previously infected"),
                    value = 1,
                    last_vax_type = c("a", "b", "c", "d"))
  mm <- stats::model.matrix(~0 + infection_history:last_vax_type, dat)
  lookup <- build_covariate_lookup_table(dat, mm, c("infection_history", "last_vax_type"))

  values <- data.table(p = c(1, 2, 3, 4, 5, 6, 7, 8), value = c(1, 2, 3, 4, 5, 6, 7, 8))
  res <- na.omit(values[lookup, on = "p"], colnames(values))[, `:=`(p = NULL)]
  expect_equal(names(res), c("value", "infection_history", "last_vax_type"))
  expect_equal(res$infection_history, rep(c("Infection naive", "Previously infected"), 4))
  expect_equal(res$last_vax_type, c("a", "a", "b", "b", "c", "c", "d", "d"))
  expect_equal(res$value, c(1, 2, 3, 4, 5, 6, 7, 8))
})

test_that("Can recover multiple covariates without interaction term", {
  dat <- data.frame(infection_history = c("Infection naive", "Previously infected"),
                    value = 1,
                    last_vax_type = c("a", "b", "c", "d"))
  mm <- stats::model.matrix(~0 + infection_history + last_vax_type, dat)
  lookup <- build_covariate_lookup_table(dat, mm, c("infection_history", "last_vax_type"))

  values <- data.table(p = c(1, 2, 3, 4, 5), value = c(1, 2, 3, 4, 5))
  res <- na.omit(values[lookup, on = "p"], colnames(values))[, `:=`(p = NULL)]
  expect_equal(names(res), c("value", "infection_history", "last_vax_type"))
  expect_equal(res$infection_history, c("Infection naive", "Previously infected", NA, NA, NA))
  expect_equal(res$last_vax_type, c(NA, NA, "b", "c", "d"))
  expect_equal(res$value, c(1, 2, 3, 4, 5))
})

test_that("Can recover multiple covariates and interaction term", {
  dat <- data.frame(infection_history = c("Infection naive", "Previously infected"),
                    value = 1,
                    last_vax_type = c("a", "b", "c", "d"))
  mm <- stats::model.matrix(~0 + infection_history + last_vax_type + infection_history:last_vax_type, dat)
  lookup <- build_covariate_lookup_table(dat, mm, c("infection_history", "last_vax_type"))

  values <- data.table(p = c(1, 2, 3, 4, 5, 6, 7, 8), value = c(1, 2, 3, 4, 5, 6, 7, 8))
  res <- na.omit(values[lookup, on = "p"], colnames(values))[, `:=`(p = NULL)]
  expect_equal(names(res), c("value", "infection_history", "last_vax_type"))
  expect_equal(res$infection_history, c("Infection naive", "Previously infected", NA, NA, NA,
                                        "Previously infected", "Previously infected", "Previously infected"))
  expect_equal(res$last_vax_type, c(NA, NA, "b", "c", "d", "b", "c", "d"))
  expect_equal(res$value, c(1, 2, 3, 4, 5, 6, 7, 8))
})
