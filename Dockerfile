FROM openanalytics/r-base

LABEL maintainer "Tobias Verbeke <tobias.verbeke@openanalytics.eu>"

# system libraries of general use
RUN apt-get update && apt-get install -y \
    sudo \
    pandoc \
    pandoc-citeproc \
    libcurl4-gnutls-dev \
    libcairo2-dev \
    libxt-dev \
    libssl-dev \
    libssh2-1-dev \
    libssl1.0.0

# system library dependency for the euler app
RUN apt-get update && apt-get install -y \
    libmpfr-dev

# basic shiny functionality
RUN R -e "install.packages(c('shiny', 'rmarkdown'), repos='https://cloud.r-project.org/')"

# install dependencies of the euler app
RUN R -e "install.packages(c('shinyTime','leaflet','dplyr','tidyr','stringr','sf','zoo','data.table','DT','leaflet.extras', 'rgdal', 'sp'), repos='https://cloud.r-project.org/')"

# copy the app to the image
RUN mkdir /root/leaflet_location_server_complex
COPY leaflet_location_server_complex /root/leaflet_location_server_complex

COPY Rprofile.site /usr/lib/R/etc/

EXPOSE 3838

CMD ["R", "-e", "shiny::runApp('/root/leaflet_location_server_complex')"]
