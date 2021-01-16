## HEADER
## Twitter weather bot
## Last modified: 2021-01-16
## Last modified by: Joe Roberts

## Setup----
# Load packages
library(RJSONIO)
library(dplyr)
library(lubridate)

# API credentials
consumer_key = "XK6MjELofFneNy3tDfKN0rL1D"
consumer_secret = "TnyAyQnv7xCEI2amRKznu1GGI6RwCbPEwRG8jirC6GkOXVw8Oa"
access_token = "1299251889641607169-aKZO3pxIL0611GPB1cJup3YeL9J5yM"
access_token_secret = 'WX2Edz8UDZK8Bi5bB6tBiChDp7oGbSqTdypLMz6a7TyzJ'
weather_api_key = '1ce36b564f86622130057224abab2648'

# Download data
connectStr <- paste0("http://api.openweathermap.org/data/2.5/onecall?lat=52.7725&lon=-2.41254&exclude=current,minutely,hourly&cnt=1&appid=1ce36b564f86622130057224abab2648")

con <- url(connectStr)
data.json <- fromJSON(paste(readLines(con), collapse=""))
close(con)

