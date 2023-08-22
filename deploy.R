library(shiny)
library(dplyr)
library(ggplot2)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(reactable)
library(hrbrthemes)
install.packages("rsconnect", repos='http://cran.us.r-project.org')
library(rsconnect)

# Authenticate
setAccountInfo(name = Sys.getenv("SHINY_ACC_NAME"),
               token = Sys.getenv("TOKEN"),
               secret = Sys.getenv("SECRET"))
# Deploy
deployApp(appFiles = c("app.R", "rec_per_snap_data.csv"), forceUpdate = TRUE)
