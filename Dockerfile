FROM rocker/shiny:latest
RUN R -e  "install.packages(c('ggplot2', 'scales', 'diptest', 'TTR', 'AUC'), repos='https://cran.rstudio.com/')"
