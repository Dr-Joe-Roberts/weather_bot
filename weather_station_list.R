## HEADER
## Weather station list
## Last modified: 2021-01-16
## Last modified by: Joe Roberts

# Load library
library(xml2)

# Call API_KEY from .Renviron file
key <- Sys.getenv("API_KEY")

# Define URL to access weather station list
url = paste0("http://datapoint.metoffice.gov.uk/public/data/val/wxfcs/all/daily/sitelist?key=", key)

# Read XML file 
XML_StationList <- read_xml(url)

# Write XML file
write_xml(XML_StationList, "StationList.xml")
