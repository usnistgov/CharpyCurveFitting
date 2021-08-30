# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.1.0

## update system libraries and install wget
RUN apt-get update

# copy necessary files
COPY ./ /

# install packages
RUN Rscript -e 'install.packages("shinythemes")'
RUN Rscript -e 'install.packages("readr")'
RUN Rscript -e 'install.packages("stringr")'
RUN Rscript -e 'install.packages("dplyr")'
RUN Rscript -e 'install.packages("minpack.lm")'
RUN Rscript -e 'install.packages("ggplot2")'
RUN Rscript -e 'install.packages("pracma")'
RUN Rscript -e 'install.packages("truncnorm")'
RUN Rscript -e 'install.packages("tinytex")'
RUN Rscript -e 'install.packages("DT")'

# expose port
EXPOSE 3838

CMD R -e 'shiny::runApp(port = 3838, host = "0.0.0.0")'
# app should run from 'inherited' RUN command from rocker/shiny