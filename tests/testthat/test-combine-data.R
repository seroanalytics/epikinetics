test_that("Can combine sero and infection data", {
  dat_sero <- data.table::fread(test_path("testdata/test_sero.csv"))
  dat_inf <- data.table::fread(test_path("testdata/test_inf.csv"))

  res <- add_exposure_data(dat_sero, dat_inf)
  expect_equal(names(res), c("pid", "day", "titre_type", "value", "Age", "last_exp_day"))
  expect_true(all(res[pid == "01-A", "last_exp_day"] == 117))
  expect_true(all(res[pid == "02-B", "last_exp_day"] == 27))
})

test_that("Can add exposure data with different day column", {
  dat_sero <- data.table::fread(test_path("testdata/test_sero.csv"))
  dat_inf <- data.table::fread(test_path("testdata/test_inf.csv"))
  data.table::setnames(dat_inf, "day", "exposure_day")

  res <- add_exposure_data(dat_sero, dat_inf, exposure_column = "exposure_day")
  expect_equal(names(res), c("pid", "day", "titre_type", "value", "Age", "last_exp_day"))
  expect_true(all(res[pid == "01-A", "last_exp_day"] == 117))
  expect_true(all(res[pid == "02-B", "last_exp_day"] == 27))
})

test_that("Required columns are validated", {
  dat_sero <- data.table::fread(test_path("testdata/test_sero.csv"))
  dat_inf <- data.table::fread(test_path("testdata/test_inf.csv"))

  expect_error(add_exposure_data(data.frame(bad = 1:10), dat_inf),
               "Missing required columns: pid, day, titre_type, value")

  expect_error(add_exposure_data(dat_sero, data.frame(bad = 1:10)),
               "Missing required columns: pid, day")
})
