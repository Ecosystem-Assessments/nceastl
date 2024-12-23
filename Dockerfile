# Base R Shiny image
FROM rocker/r-base:4.4.0


RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libproj-dev \
    libudunits2-dev \
    libgdal-dev \ 
    libgeos-dev

RUN install2.r remotes 

RUN Rscript -e 'remotes::install_version("colorspace", upgrade = "never", version = "2.1-1", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("data.table", upgrade = "never", version = "1.16.4", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("dplyr", upgrade = "never", version = "1.1.4", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("exactextractr", upgrade = "never", version = "0.10.0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("fasterize", upgrade = "never", version = "1.1.0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("fs", upgrade = "never", version = "1.6.5", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("globe", upgrade = "never", version = "1.2-0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("glue", upgrade = "never", version = "1.8.0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("here", upgrade = "never", version = "1.0.1", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("latex2exp", upgrade = "never", version = "0.9.6", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("magick", upgrade = "never", version = "2.8.5", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("magrittr", upgrade = "never", version = "2.0.3", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("plotrix", upgrade = "never", version = "3.8-4", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("purrr", upgrade = "never", version = "1.0.2", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("randomForest", upgrade = "never", version = "4.7-1.2", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("raster", upgrade = "never", version = "3.6-30", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("rmarkdown", upgrade = "never", version = "2.29", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("sf", upgrade = "never", version = "1.0-19", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("stars", upgrade = "never", version = "0.6-7", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("stringr", upgrade = "never", version = "1.5.1", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("tidyverse", upgrade = "never", version = "2.0.0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("units", upgrade = "never", version = "0.8-5", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("viridis", upgrade = "never", version = "0.6.5", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("vroom", upgrade = "never", version = "1.6.5", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("whisker", upgrade = "never", version = "0.4.1", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("yaml", upgrade = "never", version = "2.3.10", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'

COPY . ./

RUN Rscript -e 'remotes::install_deps()'