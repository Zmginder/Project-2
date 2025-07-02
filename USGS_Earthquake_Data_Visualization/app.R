#Load in Libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(bslib)
library(DT)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("USGS Earthquake Data Analysis"),
    
    #Create the three tabs
    tabsetPanel(
      #Tab for the About Section
      tabPanel("About",tags$h3("Purpose of the USGS Earthquake Data Analysis App"), 
               tags$p("The purpose of this application is to query data on earthquakes from the U.S. Geological Survey and use that data to perform some exploratory data analysis."), 
               tags$h3("Application Data"),
               tags$p("The data is sourced from the U.S. Geological Survey (USGS). The USGS earthquake catalog is a comprehensive database of seismic events, containing detailed information such as earthquake location, magnitude, and origin time. More information about the data can be found here:"),
               tags$a(href="https://earthquake.usgs.gov/fdsnws/event/1/", "https://earthquake.usgs.gov/fdsnws/event/1/"),
               tags$h3("Tabs"),
               tags$p(tags$b("About"),": This tab provides information on the purpose and layout of the application along with relevant information on the data and its source."),
               tags$p(tags$b("Data Download"),": This tab allows the user to specify and subset the specific data needed from the USGS Earthquake Catalog. The data is displayed and allows the user to save it as a .csv file"),
               tags$p(tags$b("Data Exploration"),": This tab provides allows the user to choose variables/combinations of variables that are then summarized via numerical and graphical summaries. The user is able to change the type of plot shown along with the type of summary reported."),
               tags$p(" "),
               tags$p(tags$img(src="https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",height="100px"))),
      
      #Tab for the Data Download Section
      tabPanel("Data Download",
               sidebarLayout(
                 sidebarPanel(
                   dateRangeInput("dates","Date Range",start="2025-06-01"),
                   sliderInput("magnituderange","Earthquake Magnitude Range",
                               min=0, max=10,value=c(1,8),step=1),
                   sliderInput("latituderange","Latitude Range",
                               min=-90, max=90,value=c(51,72),step=1),
                   sliderInput("longituderange","Longitude Range",
                               min=-180, max=180,value=c(-170,-129),step=1),
                   numericInput("limit","Number of Observations (Observations With Largest Magnitude Are Included First)",
                                value=100,max=20000),
                   checkboxGroupInput("columns","Select Columns of the Dataset",
                                      choices=c("mag","place","time","updated","tz","url","detail","felt","cdi","mmi","alert","status",
                                                "tsunami","sig","net","code","ids","sources","types","nst","dmin","rms",
                                                "gap","magType","type","title")),
                   downloadButton("download","Download the Data")
                 ),
                 mainPanel(tableOutput("datatable"))
               )),
      #Tab for the Data Exploration Section
      tabPanel("Data Exploration",
               sidebarLayout(
                 sidebarPanel(
                   checkboxGroupInput("selectvars","Which Variables For Analysis",
                                      choices = c("T"))
                 ),
                 mainPanel()
               ))
    )
)
# Define server logic
server <- function(input, output) {

  #Function to query API
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
    Earthquake_Data<-as_tibble(parsed$features$properties)
    return(Earthquake_Data)
  }
  
  
  #Create an initial Reactive Data Table
  Data_Input<-reactive({usgs_earthquake(
    Start_Day = input$dates[1],
    End_Day=input$dates[2],
    Minimum_Magnitude=input$magnituderange[1],
    Maximum_Magnitude=input$magnituderange[2],
    Minimum_Latitude=input$latituderange[1],
    Maximum_Latitude=input$latituderange[2],
    Minimum_Longitude=input$longituderange[1],
    Maximum_Longitude=input$longituderange[2],
    Limit_Results_To=input$limit,
    Event_Type="Earthquake"
  )})
  
  #Create a Reactive Final Data Table Based on Inputs
  Final_Data<-reactive({
    
    #Create a temporary dataframe
    temp_df<-Data_Input()
    
    #Allow the temp_df to be subsetted if specific columns are selected, but default to all columns
    ifelse(is.null(input$columns),Final_Data<-temp_df,
           Final_Data<-temp_df|>select(input$columns))
    
    #Render Final Data Table
    Final_Data
  })
  #Output the Data Table
  output$datatable<-renderTable({Final_Data()})
  
  #Download Data as CSV
  output$download<- downloadHandler(
    filename="USGS Earthquake Data.csv",
    content=function(file){
      write.csv(Final_Data(),file,row.names=FALSE)
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

