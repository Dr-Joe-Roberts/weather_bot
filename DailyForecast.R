## HEADER----
## Project: Twitter weather bot [R]
## Last modified: 22022-03-14
## Last modified by: Joe Roberts


## Background----
# Met Office data function: https://www.r-bloggers.com/2019/02/weather-forecast-from-met-office/
# OpenWeather API: https://openweathermap.org/api/one-call-api
# https://github.com/StotTot/Twitter-Weather-bot 


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
         temp_max = as.numeric(round(temp_max), digits = 0),
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
    weather_type <- "not available" } else if (weather_type == "200") {
      weather_type <- "thunderstorms with light rain" } else if (weather_type == "201") {
        weather_type <- "thunderstorms with rain" } else if (weather_type == "202") {
          weather_type <- "thunderstorms with heavy rain" } else if (weather_type == "210") {
            weather_type <- "light thunderstomrms" } else if (weather_type == "211") {
              weather_type <- "thunderstorms" } else if (weather_type == "212" | weather_type == "221") {
                weather_type <- "heavy thunderstorms" } else if (weather_type == "230" | weather_type == "231" | weather_type == "232") {
                  weather_type <- "thunderstorms with drizzle" } else if (weather_type == "300" | weather_type == "301" | weather_type == "302" | weather_type == "310" | weather_type == "311" | weather_type == "312" | weather_type == "313" | weather_type == "314" | weather_type == "321") {
                    weather_type <- "drizzle" } else if (weather_type == "500") {
                      weather_type <- "light rain" } else if (weather_type == "501") {
                        weather_type <- "moderate rain" } else if (weather_type == "502" | weather_type == "503" | weather_type == "504") {
                          weather_type <- "heavy rain" } else if (weather_type == "511") {
                            weather_type <- "freezing rain" } else if (weather_type == "520" | weather_type == "521" | weather_type == "522" | weather_type == "531") {
                              weather_type <- "rain showers" } else if (weather_type == "600") {
                                weather_type <- "light snow" } else if (weather_type == "601") {
                                  weather_type <- "snow" } else if (weather_type == "602") {
                                    weather_type <- "heavy snow" } else if (weather_type == "611") {
                                      weather_type <- "sleet" } else if (weather_type == "612" | weather_type == "615" | weather_type == "616") {
                                        weather_type <- "light sleet showers" } else if (weather_type == "613") {
                                          weather_type <- "sleet showers" } else if (weather_type == "620") {
                                            weather_type <- "light snow showers" } else if (weather_type == "621") {
                                              weather_type <- "snow showers" } else if (weather_type == "622") {
                                                weather_type <- "heavy snow showers" } else if (weather_type == "701") {
                                                  weather_type <- "misty" } else if (weather_type == "711") {
                                                    weather_type <- "smokey" } else if (weather_type == "721") {
                                                      weather_type <- "hazy" } else if (weather_type == "731") {
                                                        weather_type <- "dusty" } else if (weather_type == "741" | weather_type == "761" | weather_type == "762") {
                                                          weather_type <- "foggy" } else if (weather_type == "751") {
                                                            weather_type <- "sandy" } else if (weather_type == "771") {
                                                                weather_type <- "squalls" } else if (weather_type == "781") {
                                                                  weather_type <- "tornadoes" } else if (weather_type == "800") {
                                                                    weather_type <- "sunny" } else if (weather_type == "801") {
                                                                      weather_type <- "clear with a few clouds" } else if (weather_type == "802") {
                                                                        weather_type <- "scattered clouds" } else if (weather_type == "803") {
                                                                          weather_type <- "broken clouds" } else if (weather_type == "804") {
                                                                            weather_type <- "overcast" }
                  
  # Set weather emoji
  weather_emoji <- weather_type
  
  if (weather_emoji == "NA" | weather_emoji == "dusty" | weather_emoji == "sandy") {
    weather_emoji == " " } else if (weather_emoji == "thunderstorms with light rain" | weather_emoji == "thunderstorms with rain" | weather_emoji == "thunderstorms with heavy rain" | weather_emoji == "thunderstorms with drizzle") {
      weather_emoji <- emoji("cloud_with_lightning_and_rain") } else if (weather_emoji == "light thunderstorms" | weather_emoji == "thunderstorms" | weather_emoji == "heavy thunderstorms") {
        weather_emoji <- emoji("high_voltage") } else if (weather_emoji == "drizzle" | weather_emoji == "rain showers") {
          weather_emoji <- emoji("sun_behind_rain_cloud") } else if (weather_emoji == "light rain" | weather_emoji == "moderate rain" | weather_emoji == "heavy rain" | weather_emoji == "freezing rain") {
            weather_emoji <- emoji("cloud_with_rain") } else if (weather_emoji == "light snow" | weather_emoji == "snow" | weather_emoji == "heavy snow" | weather_emoji == "sleet" | weather_emoji == "light sleet showers" | weather_emoji == "sleet showers" | weather_emoji == "light snow showers" | weather_emoji == "snow showers" | weather_emoji == "heavy snow showers") {
              weather_emoji <- emoji("cloud_with_snow") } else if (weather_emoji == "misty" | weather_emoji == "hazy" | weather_emoji == "foggy") {
                weather_emoji <- emoji("fog") } else if (weather_emoji == "smokey") {
                  weather_emoji <- emoji("smoke") } else if (weather_emoji == "squalls") {
                    weather_emoji <- emoji("wind") } else if (weather_emoji == "tornadoes") {
                      weather_emoji <- emoji("tornado") } else if (weather_emoji == "sunny") {
                        weather_emoji <- emoji("sun") } else if (weather_emoji == "clear with a few clouds" | weather_emoji == "scattered clouds" | weather_emoji == "broken clouds") {
                          weather_emoji <- emoji("sun_behind_cloud") } else if (weather_emoji == "overcast") {
                            weather_emoji <- emoji("cloud") }
  
  # Define wind direction
  wind_direction <- weather_data$wind_deg 
 
  if (wind_direction < 11.26) {
    wind_direction <- "north" } else if (wind_direction > 11.25 & wind_direction < 78.76) {
      wind_direction <- "north east" } else if (wind_direction > 78.75 & wind_direction < 101.26) {
        wind_direction <- "east" } else if (wind_direction > 101.25 & wind_direction < 168.76) {
          wind_direction <- "south east" } else if (wind_direction > 168.75 & wind_direction < 191.26) {
            wind_direction <- "south" } else if (wind_direction > 191.25 & wind_direction < 258.76) {
              wind_direction <- "south west" } else if (wind_direction > 258.75 & wind_direction < 281.26) {
                wind_direction <- "west" } else if (wind_direction > 281.25 & wind_direction < 348.76) {
                  wind_direction <- "north west" } else if (wind_direction > 348.75 & wind_direction < 360.26) {
                    wind_direction <- "north" }
  
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
  post_tweet(status = paste0("The weather forecast for today is ", 
                             weather_type, 
                             " with highs of ",
                             weather_data$temp_max,
                             " °C and a ",
                             weather_data$precipitation_prob,
                             " % chance of precipitation ",
                             weather_emoji,
                             sep = ""), 
             token = token)
}

.tweetDailyWeather()
