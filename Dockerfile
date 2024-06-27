FROM rocker/r-ver:4

RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"
RUN Rscript -e "remotes::install_github('seroanalytics/epikinetics')"
