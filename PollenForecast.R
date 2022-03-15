## HEADER----
## Project: Twitter weather bot [R]
## Last modified: 22022-03-11
## Last modified by: Joe Roberts


## Background----
# Pollen: https://github.com/JakeRuss/sneezr and https://www.accuweather.com/ 


## Setup----
# Load packages
library(RJSONIO)
library(dplyr)
library(emoji)
library(rtweet)
library(glue)

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

# Download data
data <-.DataDownload(location = 2521389, ACCU_WEATHER_API_KEY)


## Twitter post----
# Define tweet function
.tweetDailyPollen <- function() {
  
  # Slice out relevant data
  grass_data <- data %>%
    slice(1)
  
  weed_data <- data %>%
    slice(2)
  
  tree_data <- data %>%
    slice(3)
  
  # Convert pollen value into 'real world' values
  grass_level <- grass_data$Category
  
  if (grass_level == "NULL") {
    grass_level <- "not available" } else if (grass_level == "Low") {
      grass_level <- "low" } else if (grass_level == "High") {
        grass_level <- "high" } else if (grass_level == "Good") {
          grass_level <- "good" } else if (grass_level == "Moderate") {
            grass_level <- "moderate" } else if (grass_level == "Unhealthy") {
              grass_level <- "unhealthy" } else if (grass_level == "Hazardous") {
                grass_level <- "hazardous" } 
  
  weed_level <- weed_data$Category
  
  if (weed_level == "NULL") {
    weed_level <- "not available" } else if (weed_level == "Low") {
      weed_level <- "low" } else if (weed_level == "High") {
        weed_level <- "high" } else if (weed_level == "Good") {
          weed_level <- "good" } else if (weed_level == "Moderate") {
            weed_level <- "moderate" } else if (weed_level == "Unhealthy") {
              weed_level <- "unhealthy" } else if (weed_level == "Hazardous") {
                weed_level <- "hazardous" } 
  
  tree_level <- tree_data$Category
  
  if (tree_level == "NULL") {
    tree_level <- "not available" } else if (tree_level == "Low") {
      tree_level <- "low" } else if (tree_level == "High") {
        tree_level <- "high" } else if (tree_level == "Good") {
          tree_level <- "good" } else if (tree_level == "Moderate") {
            tree_level <- "moderate" } else if (tree_level == "Unhealthy") {
              tree_level <- "unhealthy" } else if (tree_level == "Hazardous") {
                tree_level <- "hazardous" } 
  
  
  # Set emojis
  grass_emoji <- emoji("sheaf of rice")
  weed_emoji <- emoji("herb")
  tree_emoji <- emoji("evergreen tree")
  
  # Create rtweet token using credentials in .Renviron file
  token <- create_token(
    app = "HarperAdamsWeatherBot",
    consumer_key = Sys.getenv("TWITTER_CONSUMER_KEY"),
    consumer_secret = Sys.getenv("TWITTER_CONSUMER_SECRET"),
    access_token = Sys.getenv("TWITTER_ACCESS_TOKEN"),
    access_secret = Sys.getenv("TWITTER_ACCESS_TOKEN_SECRET"),
    set_renv = FALSE
  )
  
  # Compose tweet
  post_tweet(status = glue("Pollen levels today are: 
                            trees: {tree_level} {tree_emoji}
                            grass: {grass_level} {grass_emoji}
                            weeds: {weed_level} {weed_emoji}"), 
             token = token)
}

.tweetDailyPollen()
