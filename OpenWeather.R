## HEADER----
## Project: Twitter weather bot [R]
## Last modified: 22022-03-13
## Last modified by: Joe Roberts


## Background----
# Met Office data function: https://www.r-bloggers.com/2019/02/weather-forecast-from-met-office/
# OpenWeather API: https://openweathermap.org/api/one-call-api
# https://github.com/StotTot/Twitter-Weather-bot 


## Setup----
# Load packages
library(RJSONIO)
library(plyr)
library(dplyr)
library(lubridate)
library(emoji)
library(rtweet)

# Edit .Renviron file to include API key
# user_renviron = path.expand(file.path("~", ".Renviron"))
# file.edit(user_renviron)

# Call OW_API_KEYfrom .Renviron file
OW_API_KEY <- Sys.getenv("OW_API_KEY")


## Weather data----
# Define data download function - this will only pull current and daily data from OpenWeather
.Daily_Data_Download <- function(latitude, longitude, OW_API_KEY) {
  # Constructs API URL
  connectStr <- paste0("https://api.openweathermap.org/data/2.5/onecall?", 
                       "lat=", 
                       latitude, 
                       "&lon=", 
                       longitude, 
                       "&units=metric&exclude=current,minutely,hourly,alerts&appid=", 
                       OW_API_KEY)
  # Open URL connection
  con <- url(connectStr)
  # Convert JSON content to an R object
  data.json <- fromJSON(paste(readLines(con), collapse = ""))
  # Closes URL connection
  close(con)
  # Subset data for current day
  data <- data.json[["daily"]][[1]]
  
  dt <- data[["dt"]]
  weather_type <- data[["weather"]][[1]][["id"]]
  temp_day <- data[["temp"]][["day"]]
  temp_min <- data[["temp"]][["min"]]
  temp_max <- data[["temp"]][["max"]]
  temp_feel <- data[["feels_like"]][["day"]]
  pressure <- data[["pressure"]]
  humidity <- data[["humidity"]]
  wind_speed <- data[["wind_speed"]]
  wind_deg <- data[["wind_deg"]]
  wind_gust <- data[["wind_gust"]]
  precipitation_prob <- data[["pop"]] 

  weather_data <- data.frame(dt,
                             weather_type,
                             temp_day,
                             temp_min,
                             temp_max,
                             temp_feel,
                             pressure,
                             humidity,
                             wind_speed,
                             wind_deg,
                             wind_gust,
                             precipitation_prob) %>%
    
    mutate(dt = as_datetime(dt),
         weather_type = as.numeric(weather_type),
         temp_day = as.numeric(temp_day),
         temp_min = as.numeric(temp_min),
         temp_max = as.numeric(temp_max),
         temp_feel = as.numeric(temp_feel),
         pressure = as.numeric(pressure),
         humidity = as.numeric(humidity),
         wind_speed = as.numeric(round(wind_speed/0.4470), digits = 0), # Convert to mph
         wind_deg = as.numeric(wind_deg),
         wind_gust = as.numeric(round(wind_gust/0.4470), digits = 0), # Convert to mph
         precipitation_prob = as.numeric(round(precipitation_prob*100), digits = 0)) # Convert to percentage
}

# Download data
daily_data <- .Daily_Data_Download(latitude = 52.776576, longitude = -2.426323, OW_API_KEY)


## Twitter post----
# Define tweet function
.tweetDailyWeather <- function() {
  
  # Select data 
  weather_data <- daily_data
  
  # Convert weather description into 'real world' values
  weather_type<- weather_data$weather_type
  if (weather_type == "NA") {
    weather_type <- "not available" } else if (weather_type == "0") {
      weather_type <- "clear" } else if (weather_type == "1") {
        weather_type <- "sunny" } else if (weather_type == "3") {
          weather_type <- "partly cloudy" } else if (weather_type == "5") {
            weather_type <- "misty" } else if (weather_type == "6") {
              weather_type <- "foggy" } else if (weather_type == "7") {
                weather_type <- "cloudy" } else if (weather_type == "8") {
                  weather_type <- "overcast" } else if (weather_type == "10") {
                    weather_type <- "light rain showers" } else if (weather_type == "11") {
                      weather_type <- "drizzle" } else if (weather_type == "12") {
                        weather_type <- "light rain" } else if (weather_type == "14") {
                          weather_type <- "heavy rain showers" } else if (weather_type == "15") {
                            weather_type <- "heavy rain" } else if (weather_type == "17") {
                              weather_type <- "sleet showers" } else if (weather_type == "18") {
                                weather_type <- "sleet" } else if (weather_type == "20") {
                                  weather_type <- "hail showers" } else if (weather_type == "21") {
                                    weather_type <- "hail" } else if (weather_type == "23") {
                                      weather_type <- "light snow showers" } else if (weather_type == "24") {
                                        weather_type <- "light snow" } else if (weather_type == "26") {
                                          weather_type <- "heavy snow showers" } else if (weather_type == "27") {
                                            weather_type <- "heavy snow" } else if (weather_type == "29") {
                                              weather_type <- "thunder showers" } else if (weather_type == "30") {
                                                weather_type <- "thunder" }
  
  # Set weather emoji
  weather_emoji <- weather_type
  if (weather_emoji == "NA") {
    weather_emoji == " " } else if (weather_emoji == "clear") {
      weather_emoji <- emoji("sun") } else if (weather_emoji == "sunny") {
        weather_emoji <- emoji("sun") } else if (weather_emoji == "partly cloudy") {
          weather_emoji <- emoji("sun_behind_cloud") } else if (weather_emoji == "misty") {
            weather_emoji <- emoji("fog") } else if (weather_emoji == "foggy") {
              weather_emoji <- emoji("fog") } else if (weather_emoji == "cloudy") {
                weather_emoji <- emoji("cloud") } else if (weather_emoji == "overcast") {
                  weather_emoji <- emoji("cloud") } else if (weather_emoji == "light rain showers") {
                    weather_emoji <- emoji("sun_behind_rain_cloud") } else if (weather_emoji == "drizzle") {
                      weather_emoji <- emoji("cloud_with_rain") } else if (weather_emoji == "light rain") {
                        weather_emoji <- emoji("cloud_with_rain") } else if (weather_emoji == "heavy rain showers") {
                          weather_emoji <- emoji("cloud_with_rain") } else if (weather_emoji == "heavy rain") {
                            weather_emoji <- emoji("cloud_with_rain") } else if (weather_emoji == "sleet showers") {
                              weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "sleet") {
                                weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "hail showers") {
                                  weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "hail") {
                                    weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "light snow showers") {
                                      weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "light snow") {
                                        weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "heavy snow showers") {
                                          weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "heavy snow") {
                                            weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "thunder showers") {
                                              weather_emoji <- emoji("cloud_with_lightning_and_rain") } else if (weather_emoji == "thunder") {
                                                weather_emoji <- emoji("high_voltage") }
  
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
  post_tweet(status = paste0("The weather forecast for HAU today is ", 
                             weather_type, 
                             " with highs of ", 
                             weather_data$max_temp, 
                             " °C and a ",
                             weather_data$precipitation_prob,
                             " % chance of precipitation ",
                             weather_emoji,
                             sep = ""), 
             token = token)
}

.tweetDailyWeather()