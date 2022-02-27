FROM rocker/tidyverse:4.0.0

RUN R -e "install.packages('ggplot2')"
RUN R -e "install.packages('dplyr')"

COPY /CSV_files /CSV_files
COPY /Plots /Plots
COPY /Shapefile /Shapefile
COPY /Projeto_docker.R /Projeto_docker.R

CMD Rscript /Projeto_docker.R

