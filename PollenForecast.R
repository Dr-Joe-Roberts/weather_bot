## HEADER----
## Project: Twitter weather bot [R]
## Last modified: 22022-03-11
## Last modified by: Joe Roberts


## Background----
# Met Office data function: https://www.r-bloggers.com/2019/02/weather-forecast-from-met-office/
# Met Office coding definitions: https://www.metoffice.gov.uk/services/data/datapoint/uk-3-hourly-site-specific-forecast
# Pollen: https://github.com/JakeRuss/sneezr and https://www.accuweather.com/ 


## Setup----
# Load packages
library(RJSONIO)
library(dplyr)
library(lubridate)
library(emoji)
library(rtweet)

# Edit .Renviron file to include API key
# user_renviron = path.expand(file.path("~", ".Renviron"))
# file.edit(user_renviron)

# Call ACCU_WEATHER_API_KEY from .Renviron file
ACCU_WEATHER_API_KEY <- Sys.getenv("ACCU_WEATHER_API_KEY")


## Weather data----
# Define data download function
.DataDownload <- function(location, ACCU_WEATHER_API_KEY) {
  connectStr <- paste0("http://dataservice.accuweather.com/forecasts/v1/daily/1day/", 
                       location, 
                       "?apikey=", 
                       ACCU_WEATHER_API_KEY,
                       "%20&details=true&metric=true")
  
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse = ""))
  close(con)
  
  # Specify data to extract from json file
  grass_pollen <- data.json[["DailyForecasts"]][[1]][["AirAndPollen"]][[2]]
  weed_pollen <- data.json[["DailyForecasts"]][[1]][["AirAndPollen"]][[4]]
  tree_pollen <- data.json[["DailyForecasts"]][[1]][["AirAndPollen"]][[5]]
  
  pollen <- as_tibble(rbind(grass_pollen, weed_pollen, tree_pollen))
  
}

data <-.DataDownload(location = 2521389, ACCU_WEATHER_API_KEY)


