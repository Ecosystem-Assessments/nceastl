# Base R Shiny image
FROM rocker/r-base:4.4.0

ARG GITHUB_PAT
ENV GITHUB_PAT=${GITHUB_PAT}

RUN apt-get update \
    && apt-get install -y --no-install-recommends \
    libproj-dev \
    libudunits2-dev \
    libgdal-dev \ 
    libgeos-dev \
    libarchive-dev \
    libfontconfig1-dev \
    libmagick++-dev \
    libfreetype6-dev

RUN apt-get install -y --no-install-recommends \    
    r-cran-devtools \
    r-cran-tidyverse 

RUN apt-get install -y --no-install-recommends \  
    r-cran-bibtex \
    r-cran-data.table \
    r-cran-here \
    r-cran-ragg \
    r-cran-randomforest \
    r-cran-raster \
    r-cran-rmarkdown \
    r-cran-magick \
    r-cran-plotrix \
    r-cran-sf \
    r-cran-stars \
    r-cran-terra

RUN apt-get install -y --no-install-recommends \  
    r-cran-rcpparmadillo \
    r-cran-viridis \
    r-cran-bookdown

# RUN install2.r remotes 

# RUN Rscript -e 'remotes::install_version("devtools", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("tidyverse", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("bibtex", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("data", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("here", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("ragg", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("randomforest", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("raster", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("rmarkdown", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("magick", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("plotrix", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("sf", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("stars", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
# RUN Rscript -e 'remotes::install_version("terra", upgrade = "never", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'


RUN Rscript -e 'remotes::install_version("archive", upgrade = "never", version = "1.1.10", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("RefManageR", upgrade = "never", version = "1.4.0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("colorspace", upgrade = "never", version = "2.1-1", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("globe", upgrade = "never", version = "1.2-0", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'
RUN Rscript -e 'remotes::install_version("latex2exp", upgrade = "never", version = "0.9.6", repos = "https://packagemanager.posit.co/cran/__linux__/jammy/latest")'


WORKDIR /usr
COPY . ./

RUN Rscript -e  'remotes::install_github("VLucet/rgovcan", upgrade = "never")'
RUN Rscript -e 'remotes::install_deps(upgrade = "never")'


ENTRYPOINT ["bash"]