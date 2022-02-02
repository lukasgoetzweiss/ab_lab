FROM rocker/shiny

RUN apt-get update && apt-get install -y \
    libcurl4-gnutls-dev \
    libssl-dev

RUN Rscript -e "install.packages('devtools')"
RUN Rscript -e "install.packages('cli')"


RUN mkdir /src
RUN mkdir /src/rctr
ADD ./ /src/rctr/
RUN Rscript -e "devtools::install('/src/rctr/rctr')"

# copy the app directory into the image
COPY ./act_app/* /srv/shiny-server/

# run app
CMD ["/usr/bin/shiny-server"]
