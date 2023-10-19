# Base image https://hub.docker.com/u/rocker/
FROM rocker/shiny:4.3.1

## update system libraries and install wget
RUN apt-get update
RUN apt-get install texlive-latex-recommended texlive-fonts-recommended -y
RUN apt-get install texlive-latex-extra -y

# copy necessary files
COPY . /srv/shiny-server/

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
RUN Rscript -e "tinytex::install_tinytex()"

WORKDIR /srv/shiny-server
RUN rm -r 01_hello 02_text 03_reactivity 04_mpg 05_sliders 06_tabsets 07_widgets 08_html 09_upload 10_download 11_timer
RUN chmod -R 775 /srv/shiny-server
RUN chgrp -R shiny /srv/shiny-server

# expose port
EXPOSE 3838

# app should run from 'inherited' CMD shiny_server.sh command from rocker/shiny
