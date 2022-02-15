FROM rocker/shiny

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

# Install R packages
# (do this before installing rctr to avoid rebuilding full R stack from scratch)
RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "install.packages('cli')"
RUN Rscript -e "install.packages('bigQueryR')"
RUN Rscript -e "install.packages('bigrquery')"
RUN Rscript -e "install.packages('data.table')"
RUN Rscript -e "install.packages('DT')"
RUN Rscript -e "install.packages('ggplot2')"
RUN Rscript -e "install.packages('glue')"
RUN Rscript -e "install.packages('lubridate')"
RUN Rscript -e "install.packages('scales')"
RUN Rscript -e "install.packages('stringr')"
RUN Rscript -e "install.packages('shiny')"
RUN Rscript -e "install.packages('yaml')"
RUN Rscript -e "install.packages('shinythemes')"
RUN Rscript -e "install.packages('DT')"
RUN Rscript -e "install.packages('googledrive')"
RUN Rscript -e "install.packages('readr')"

# set up local directories
RUN mkdir /src
RUN mkdir /src/rctr
ADD ./ /src/rctr/

# install rctr
RUN Rscript -e "devtools::install('/src/rctr/rctr')"

# copy the app directory and config file into the image
COPY ./act_app/* /srv/shiny-server/
COPY shiny-server.conf /etc/shiny-server/

# expose port and run the app
EXPOSE 80
CMD ["R", "-e", "library(shiny); source('/src/rctr/act_app/app.R')"]
