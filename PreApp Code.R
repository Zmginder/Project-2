#Load Libraries
library(tidyverse)
library(httr)
library(jsonlite)

#Function to query API
usgs_water<-function(State="ky",
                     Period="P5D",
                     Site_Status="all",
                     Site_Type="LK",
                     Altitude_Min="0",
                     Altitude_Max="20000"){
  url=paste0("https://waterservices.usgs.gov/nwis/iv/?format=json",
             "&stateCd=", State,
             "&period=", Period,
             "&siteStatus=", Site_Status,
             "&siteType=", Site_Type,
             "&altMin=", Altitude_Min,
             "&altMax=", Altitude_Max)
  unparsed<-GET(url)
  parsed<-fromJSON(rawToChar(unparsed$content))
  USGS<-as_tibble(parsed$value$timeSeries)
  return(USGS)
}
data_1<-usgs_water()


#Second Option
usgs_earthquake<-function(Start_Day="2025-06-01",
                          End_Day="2025-06-30",
                          Minimum_Magnitude="1",
                          Maximum_Magnitude="8.0",
                          Minimum_Latitude="51",
                          Maximum_Latitude="72",
                          Minimum_Longitude="-170",
                          Maximum_Longitude="-129",
                          Limit_Results_To="100",
                          Event_Type="Earthquake"){
  url=paste0("https://earthquake.usgs.gov/fdsnws/event/1/query?format=geojson",
             "&starttime=", Start_Day,
             "&endtime=", End_Day,
             "&minmagnitude=", Minimum_Magnitude,
             "&maxmagnitude=", Maximum_Magnitude,
             "&minlatitude=", Minimum_Latitude,
             "&maxlatitude=", Maximum_Latitude,
             "&minlongitude=", Minimum_Longitude,
             "&maxlongitude=", Maximum_Longitude,
             "&eventtype=", Event_Type,
             "&orderby=magnitude",
             "&limit=", Limit_Results_To)
  unparsed<-GET(url)
  parsed<-fromJSON(rawToChar(unparsed$content))
  Earthquake_Data<-as_tibble(parsed$features)
  return(Earthquake_Data)
}
data_2<-usgs_earthquake()
