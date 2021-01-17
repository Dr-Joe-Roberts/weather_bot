## HEADER----
## Project: Twitter weather bot [R]
## Last modified: 2021-01-17
## Last modified by: Joe Roberts

## To-do
# Add emojis
# Run on a schedule using GitHub actions

## Background----
# Met Office data function: https://www.r-bloggers.com/2019/02/weather-forecast-from-met-office/
# Mett Office coding definitions: https://www.metoffice.gov.uk/services/data/datapoint/uk-3-hourly-site-specific-forecast

## Setup----
# Load packages
library(RJSONIO)
library(dplyr)
library(lubridate)
library(rtweet)

# Edit .Renviron file to include API key
# user_renviron = path.expand(file.path("~", ".Renviron"))
# file.edit(user_renviron)

# Call MET_API_KEY from .Renviron file
MET_API_KEY <- Sys.getenv("MET_API_KEY")

## Weather data----
# Define data download function
METDataDownload <- function(stationID, product, MET_API_KEY) {
  connectStr <- paste0("http://datapoint.metoffice.gov.uk/public/data/val/wxfcs/all/json/", stationID, "?res=", product, "&key=", MET_API_KEY)

  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse = ""))
  close(con)

  # Station
  LocID <- data.json$SiteRep$DV$Location$`i`
  LocName <- data.json$SiteRep$DV$Location$name
  Country <- data.json$SiteRep$DV$Location$country
  Lat <- data.json$SiteRep$DV$Location$lat
  Lon <- data.json$SiteRep$DV$Location$lon
  Elev <- data.json$SiteRep$DV$Location$elevation

  Details <- data.frame(
    LocationID = LocID,
    LocationName = LocName,
    Country = Country,
    Lon = Lon,
    Lat = Lat,
    Elevation = Elev
  )
  # Parameters
  param <- do.call("rbind", data.json$SiteRep$Wx$Param)

  # Forecast
  if (product == "daily") {
    dates <- unlist(lapply(data.json$SiteRep$DV$Location$Period, function(x) {
      x$value
    }))
    DayForecast <- do.call("rbind", lapply(data.json$SiteRep$DV$Location$Period, function(x) {
      x$Rep[[1]]
    }))
    NightForecast <- do.call("rbind", lapply(data.json$SiteRep$DV$Location$Period, function(x) {
      x$Rep[[2]]
    }))
    colnames(DayForecast)[ncol(DayForecast)] <- "Type"
    colnames(NightForecast)[ncol(NightForecast)] <- "Type"

    ForecastDF <- plyr::rbind.fill.matrix(DayForecast, NightForecast) %>%
      as_tibble() %>%
      mutate(Date = as.Date(rep(dates, 2))) %>%
      mutate(
        Gn = as.numeric(Gn),
        Hn = as.numeric(Hn),
        PPd = as.numeric(PPd),
        S = as.numeric(S),
        Dm = as.numeric(Dm),
        FDm = as.numeric(FDm),
        W = as.numeric(W),
        U = as.numeric(U),
        Gm = as.numeric(Gm),
        Hm = as.numeric(Hm),
        PPn = as.numeric(PPn),
        Nm = as.numeric(Nm),
        FNm = as.numeric(FNm)
      )
  } else {
    dates <- unlist(lapply(data.json$SiteRep$DV$Location$Period, function(x) {
      x$value
    }))
    Forecast <- do.call("rbind", lapply(lapply(data.json$SiteRep$DV$Location$Period, function(x) {
      x$Rep
    }), function(x) {
      do.call("rbind", x)
    }))
    colnames(Forecast)[ncol(Forecast)] <- "Hour"

    DateTimes <- seq(ymd_hms(paste0(as.Date(dates[1]), " 00:00:00")), ymd_hms(paste0(as.Date(dates[length(dates)]), " 21:00:00")), "3 hours")

    if (nrow(Forecast) < length(DateTimes)) {
      extra_lines <- length(DateTimes) - nrow(Forecast)
      for (i in 1:extra_lines) {
        Forecast <- rbind(rep("0", ncol(Forecast)), Forecast)
      }
    }

    ForecastDF <- Forecast %>%
      as_tibble() %>%
      mutate(Hour = DateTimes) %>%
      filter(D != "0") %>%
      mutate(
        F = as.numeric(F),
        G = as.numeric(G),
        H = as.numeric(H),
        Pp = as.numeric(Pp),
        S = as.numeric(S),
        T = as.numeric(T),
        U = as.numeric(U),
        W = as.numeric(W)
      )
  }


  list(Details, param, ForecastDF)
}

# Download data
raw_data <- METDataDownload(stationID = 352811, product = "daily", MET_API_KEY)

## Twitter post----
# Define tweet function
tweetDailyWeather <- function() {
  
  # Subset data for current day
  weather_data <- raw_data[[3]] %>%
    # Slice out first row
    slice(1) %>%
    # Remove columns containing night data
    select(-Gm, -Hm, -PPn, -Nm, -Type, -FNm) %>%
    # Rename columns
    rename(
      wind_direction = D,
      wind_gust = Gn,
      relative_humidity = Hn,
      precipitation_probability = PPd,
      wind_speed = S,
      visibility = V,
      max_temp = Dm,
      feels_like_temp = FDm,
      weather_type = W,
      uv_index = U,
      date = Date
    )
  
  # Convert weather type into 'real world' values
  weather_type <- weather_data$weather_type
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
                             weather_data$max_temp, 
                             " °C and a ",
                             weather_data$precipitation_probability,
                             " % chance of precipitation.",
                             "\n",
                             "\n",
                             "Updated on ", 
                             weather_data$date,
                             sep = ""), 
             token = token)
  }

tweetDailyWeather()
