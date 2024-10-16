test_that("Can convert character pids to numeric ids and back again", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))

  dat$pid <- paste0("ID", dat$pid)
  lookup <- build_pid_lookup(dat)

  dat[, nid := lookup[pid]]
  dat[, recovered := names(lookup)[nid]]

  expect_equal(dat$recovered, dat$pid)
})

test_that("Can convert numeric pids to numeric ids and back again", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))

  lookup <- build_pid_lookup(dat)

  dat[, nid := lookup[pid]]
  dat[, recovered := as.numeric(names(lookup)[nid])]

  expect_equal(dat$recovered, dat$pid)
})

test_that("Can convert titre types to numbers and back again", {
  dat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))

  lookup <- build_titre_type_lookup(dat)

  dat[, titre_type_num := lookup[titre_type]]
  dat[, recovered := names(lookup)[titre_type_num]]

  expect_equal(dat$recovered, dat$titre_type)
  expect_equal(dat$recovered, dat$titre_type)
})
