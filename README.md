# Project-2
Repo for ST 558 Project 2

About this application:
The purpose of this application is to query data on earthquakes from the U.S. Geological Survey and use that data to perform some exploratory data analysis. The data is sourced from the U.S. Geological Survey (USGS). The USGS earthquake catalog is a comprehensive database of seismic events, containing detailed information such as earthquake location, magnitude, and origin time.

Packages needed to run this application:
tidyverse
httr
jsonlite
shiny
ggplot2
data.table

Code to install all the packages:
install.packages(c("tidyverse","httr","jsonlite","shiny","ggplot2","data.table"))

Code to run application: 
shiny::runGitHub(repo="Project-2",username="Zmginder",subdir = "USGS_Earthquake_Data_Visualization")

