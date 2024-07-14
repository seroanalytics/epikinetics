test_that("Cpp and R function produce same values", {
  t_max <- 6L
  dat_pop <- data.table(t = 0:t_max,
                        t0 = 0,
                        tp = 3,
                        ts = 5,
                        m1 = 0.5,
                        m2 = 1,
                        m3 = 1.5
  )
  res_pop <- dat_pop[, mu := scova_simulate_trajectory(t, t0, tp, ts, m1, m2, m3), by = "t"]
  dat_ind <- data.table(stan_id = 1L,
                        t_max = t_max,
                        titre_type = 3L,
                        draw = 10L,
                        t0_ind = 0,
                        tp_ind = 3,
                        ts_ind = 5,
                        m1_ind = 0.5,
                        m2_ind = 1,
                        m3_ind = 1.5
  )

  res_ind <- scova_simulate_trajectories(dat_ind)
  expect_equal(res_pop$mu, res_ind$mu)
})
