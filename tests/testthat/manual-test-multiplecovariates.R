library(ggplot2)
library(epikinetics)

mod <- biokinetics$new(file_path = system.file("delta_full.rds", package = "epikinetics"),
                       covariate_formula = ~0 + infection_history:last_vax_type)

mod$fit(chains = 4,
        parallel_chains = 4,
        iter_warmup = 50,
        iter_sampling = 200,
        threads_per_chain = 4)

dat <- mod$simulate_population_trajectories()
dat[, titre_type := forcats::fct_relevel(
  titre_type,
  c("Ancestral", "Alpha", "Delta"))]

ggplot(data = dat[!is.na(last_vax_type)]) +
  geom_line(aes(x = t,
                y = me,
                colour = titre_type)) +
  geom_ribbon(aes(x = t,
                  ymin = lo,
                  ymax = hi,
                  fill = titre_type), alpha = 0.65) +
  coord_cartesian(clip = "off") +
  labs(x = "Time since last exposure (days)",
       y = expression(paste("Titre (IC"[50], ")"))) +
  scale_y_continuous(
    trans = "log2") +
  facet_wrap(titre_type ~ infection_history + last_vax_type)

dat <- mod$population_stationary_points()
dat[, titre_type := forcats::fct_relevel(
  titre_type,
  c("Ancestral", "Alpha", "Delta"))]

ggplot(data = dat[!is.na(last_vax_type)], aes(
  x = mu_p, y = mu_s,
  colour = titre_type)) +
  geom_density_2d(
    aes(
      group = interaction(
        infection_history,
        last_vax_type,
        titre_type))) +
  geom_point(alpha = 0.05, size = 0.2) +
  geom_point(aes(x = mu_p_me, y = mu_s_me,
                 shape = interaction(infection_history, last_vax_type)),
             colour = "black") +
  geom_path(aes(x = mu_p_me, y = mu_s_me,
                group = titre_type),
            colour = "black") +
  geom_vline(xintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_x_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120, 10240),
    labels = c(expression(" " <= 40),
               "80", "160", "320", "640", "1280", "2560", "5120", "10240"),
    limits = c(NA, 10240)) +
  geom_hline(yintercept = 2560, linetype = "twodash", colour = "gray30") +
  scale_y_continuous(
    trans = "log2",
    breaks = c(40, 80, 160, 320, 640, 1280, 2560, 5120, 10240),
    labels = c(expression(" " <= 40),
               "80", "160", "320", "640", "1280", "2560", "5120", "10240"),
    limits = c(NA, 5120)) +
  labs(x = expression(paste("Population-level titre value at peak (IC"[50], ")")),
       y = expression(paste("Population-level titre value at set-point (IC"[50], ")")))

dat <- mod$simulate_individual_trajectories()
rawdat <- data.table::fread(system.file("delta_full.rds", package = "epikinetics"))

date_delta <- lubridate::ymd("2021-05-07")
date_ba2 <- lubridate::ymd("2022-01-24")

dat$wave <- "Delta"
rawdat$wave <- "Delta"
plot_data <- merge(
  dat, rawdat[, .(
    min_date = min(date), max_date = max(date)), by = wave])[
  , .SD[calendar_date >= min_date & calendar_date <= date_ba2], by = wave]

plot_data[, titre_type := forcats::fct_relevel(
  titre_type,
  c("Ancestral", "Alpha", "Delta"))]

ggplot() +
  geom_line(
    data = plot_data,
    aes(x = calendar_date,
        y = me,
        group = interaction(titre_type, wave),
        colour = titre_type),
    alpha = 0.2) +
  geom_ribbon(
    data = plot_data,
    aes(x = calendar_date,
        ymin = lo,
        ymax = hi,
        group = interaction(titre_type, wave)
    ),
    alpha = 0.2) +
  labs(title = "Population-level titre values",
       tag = "A",
       x = "Date",
       y = expression(paste("Titre (IC"[50], ")"))) +
  scale_x_date(
    date_labels = "%b %Y",
    limits = c(min(rawdat$date), date_ba2)) +
  geom_smooth(
    data = plot_data,
    aes(x = calendar_date,
        y = me,
        fill = titre_type,
        colour = titre_type,
        group = interaction(titre_type, wave)),
    alpha = 0.5, span = 0.2)