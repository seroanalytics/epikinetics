FROM rocker/r-ver:4.5.1

RUN Rscript -e "install.packages(c('ggplot2', 'posterior', 'cmdstanr'), repos = c(stan = 'https://stan-dev.r-universe.dev', CRAN = 'https://cloud.r-project.org'))"
RUN Rscript -e "cmdstanr::check_cmdstan_toolchain(fix = TRUE); cmdstanr::install_cmdstan(cores = 2)"

WORKDIR /src/epikinetics
COPY . .
RUN R CMD INSTALL .

WORKDIR /workdir
CMD ["R"]
