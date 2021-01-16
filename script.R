## HEADER
## Twitter weather bot
## Last modified: 2021-01-16
## Last modified by: Joe Roberts

## Setup----
# Edit .Renviron file to include API key
user_renviron = path.expand(file.path("~", ".Renviron"))
file.edit(user_renviron) # open with another text editor if this fails

# Call API_KEY from .Renviron file
key <- Sys.getenv("API_KEY")

## Download weather data----
# Define function
METDataDownload <- function(stationID, product, API_KEY){
  library(RJSONIO) 
  library(dplyr)
  library(lubridate)
  connectStr <- paste0("http://datapoint.metoffice.gov.uk/public/data/val/wxfcs/all/json/", stationID, "?res=", product, "&key=", key)
  
  con <- url(connectStr)
  data.json <- fromJSON(paste(readLines(con), collapse=""))
  close(con)
  
  #Station
  LocID <- data.json$SiteRep$DV$Location$`i`
  LocName <- data.json$SiteRep$DV$Location$name
  Country <- data.json$SiteRep$DV$Location$country
  Lat <- data.json$SiteRep$DV$Location$lat
  Lon <- data.json$SiteRep$DV$Location$lon
  Elev <- data.json$SiteRep$DV$Location$elevation
  
  Details <- data.frame(LocationID = LocID,
                        LocationName = LocName,
                        Country = Country,
                        Lon = Lon,
                        Lat = Lat,
                        Elevation = Elev)
  # Parameters
  param <- do.call("rbind",data.json$SiteRep$Wx$Param)
  
  # Forecast
  if(product == "daily"){
    dates <- unlist(lapply(data.json$SiteRep$DV$Location$Period, function(x){x$value}))
    DayForecast <- do.call("rbind", lapply(data.json$SiteRep$DV$Location$Period, function(x){x$Rep[[1]]}))
    NightForecast <- do.call("rbind", lapply(data.json$SiteRep$DV$Location$Period, function(x){x$Rep[[2]]}))
    colnames(DayForecast)[ncol(DayForecast)] <- "Type"
    colnames(NightForecast)[ncol(NightForecast)] <- "Type"
    
    ForecastDF <- plyr::rbind.fill.matrix(DayForecast, NightForecast) %>%
      as_tibble() %>%
      mutate(Date = as.Date(rep(dates, 2))) %>%
      mutate(Gn = as.numeric(Gn),
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
             FNm = as.numeric(FNm))
    
    
  } else {
    dates <- unlist(lapply(data.json$SiteRep$DV$Location$Period, function(x){x$value}))
    Forecast <- do.call("rbind", lapply(lapply(data.json$SiteRep$DV$Location$Period, function(x){x$Rep}), function(x){do.call("rbind",x)}))
    colnames(Forecast)[ncol(Forecast)] <- "Hour"
    
    DateTimes <- seq(ymd_hms(paste0(as.Date(dates[1])," 00:00:00")),ymd_hms(paste0(as.Date(dates[length(dates)])," 21:00:00")), "3 hours")
    
    if(nrow(Forecast)<length(DateTimes)){
      extra_lines <- length(DateTimes)-nrow(Forecast)
      for(i in 1:extra_lines){
        Forecast <- rbind(rep("0", ncol(Forecast)), Forecast)
      }
    }
    
    ForecastDF <- Forecast %>%
      as_tibble() %>%
      mutate(Hour = DateTimes) %>%
      filter(D != "0") %>%
      mutate(F = as.numeric(F),
             G = as.numeric(G),
             H = as.numeric(H),
             Pp = as.numeric(Pp),
             S = as.numeric(S),
             T = as.numeric(T),
             U = as.numeric(U),
             W = as.numeric(W))
    
  }
  
  
  list(Details, param, ForecastDF)
  
}

# Call function
METDataDownload(stationID = 352811, product = "daily", key)

