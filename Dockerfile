FROM rocker/r-ver:4
ARG CACHEBUST=1

RUN Rscript -e "install.packages('remotes')"
RUN Rscript -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"

RUN echo "$CAHEBUST"
RUN Rscript -e "remotes::install_github('seroanalytics/epikinetics')"
