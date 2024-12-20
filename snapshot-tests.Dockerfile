FROM rocker/tidyverse:4

RUN apt-get update
RUN apt-get install libtbb-dev -y
RUN Rscript -e "install.packages('cmdstanr', repos = c('https://stan-dev.r-universe.dev', getOption('repos')))"
RUN Rscript -e "cmdstanr::install_cmdstan()"

WORKDIR /epikinetics
COPY DESCRIPTION /epikinetics

RUN Rscript -e "devtools::install_deps()"
RUN Rscript -e "install.packages('decor')"

COPY . /epikinetics
