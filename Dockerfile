FROM rocker/shiny:4.2.1
RUN install2.r rsconnect shiny dplyr ggrepel reactable nflplotR ggplot2 hrbrthemes nflreadr
WORKDIR /home/routes_shiny/
COPY app.R app.R 
COPY rec_per_snap_data.csv rec_per_snap_data.csv
COPY deploy.R deploy.R
CMD Rscript deploy.R
