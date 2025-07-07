#Load in Libraries
library(tidyverse)
library(httr)
library(jsonlite)
library(shiny)
library(bslib)
library(DT)
library(ggplot2)

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
               tags$p(tags$img(src="https://upload.wikimedia.org/wikipedia/commons/0/08/USGS_logo.png",height="600px"))),
      
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
                                      choices=c("Magnitude","Time_UTC_milliseconds_since_epoch_","Place",
                                                "Data_Updated_On_UTC_milliseconds_since_epoch_",
                                                "Time_Zone_Offset","Url",
                                                "Detail","Felt_Reports","Maximum_Reported_Intensity",
                                                "Maximum_Estimated_Instrumental_Intensity",
                                                "Alert","Status","Tsunami_Flag","Significance_0_through_1000_",
                                                "Main_Network_Contributor","Code","Ids","Network_Contributors",
                                                "Product_Types","Number_of_Seismic_Stations_Utilized",
                                                "Horizontal_Distance_from_Epicenter_to_Nearest_Station",
                                                "Root_Mean_Square_Travel_Time_Residual_sec_",
                                                "Largest_Azimuthal_Gap_Between_Azimuthally_Adjacent_Stations",
                                                "Method_to_Determine_Magnitude","Event_Type","Title")),
                   downloadButton("download","Download the Data")
                 ),
                 mainPanel(tableOutput("datatable"))
               )),
      #Tab for the Data Exploration Section
      tabPanel("Data Exploration",
               sidebarLayout(
                 sidebarPanel(
                   selectInput("selectvars1","Variable 1",
                                      choices = c("Magnitude","Time_UTC_milliseconds_since_epoch_","Place",
                                                  "Data_Updated_On_UTC_milliseconds_since_epoch_",
                                                  "Time_Zone_Offset",
                                                  "Felt_Reports","Maximum_Reported_Intensity",
                                                  "Maximum_Estimated_Instrumental_Intensity",
                                                  "Alert","Status","Tsunami_Flag","Significance_0_through_1000_",
                                                  "Main_Network_Contributor","Code","Network_Contributors",
                                                  "Number_of_Seismic_Stations_Utilized",
                                                  "Horizontal_Distance_from_Epicenter_to_Nearest_Station",
                                                  "Root_Mean_Square_Travel_Time_Residual_sec_",
                                                  "Largest_Azimuthal_Gap_Between_Azimuthally_Adjacent_Stations",
                                                  "Method_to_Determine_Magnitude","Event_Type","Title")),
                 selectInput("selectvars2","Variable 2",
                             choices = c("Magnitude","Time_UTC_milliseconds_since_epoch_","Place",
                                         "Data_Updated_On_UTC_milliseconds_since_epoch_",
                                         "Time_Zone_Offset",
                                         "Felt_Reports","Maximum_Reported_Intensity",
                                         "Maximum_Estimated_Instrumental_Intensity",
                                         "Alert","Status","Tsunami_Flag","Significance_0_through_1000_",
                                         "Main_Network_Contributor","Code","Network_Contributors",
                                         "Number_of_Seismic_Stations_Utilized",
                                         "Horizontal_Distance_from_Epicenter_to_Nearest_Station",
                                         "Root_Mean_Square_Travel_Time_Residual_sec_",
                                         "Largest_Azimuthal_Gap_Between_Azimuthally_Adjacent_Stations",
                                         "Method_to_Determine_Magnitude","Event_Type","Title")),
               
               #Create a Dynamic UI For The Sumary Output Dropdown
               uiOutput("SummaryDropdown"),
               uiOutput("Faceting"),
               uiOutput("FacetingVars")
               ),
                 mainPanel(uiOutput("Summarys"))
               ))))
# Define server logic
server <- function(input, output, session) {

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
    temp_df<-Data_Input()|>
      mutate(tz=as.factor(tz),place=sub(".*of\\s+", "", place))|>
      rename(Magnitude=mag, Place=place, "Time_UTC_milliseconds_since_epoch_"=time, 
             "Data_Updated_On_UTC_milliseconds_since_epoch_"=updated, "Time_Zone_Offset"=tz, 
             Url=url,Detail=detail,"Felt_Reports"=felt,"Maximum_Reported_Intensity"=cdi,
             "Maximum_Estimated_Instrumental_Intensity"=mmi,Alert=alert,Status=status,
             "Tsunami_Flag"=tsunami,"Significance_0_through_1000_"=sig,"Main_Network_Contributor"=net,
             Code=code,Ids=ids,"Network_Contributors"=sources,"Product_Types"=types,
             "Number_of_Seismic_Stations_Utilized"=nst,"Horizontal_Distance_from_Epicenter_to_Nearest_Station"=dmin,
             "Root_Mean_Square_Travel_Time_Residual_sec_"=rms,
             "Largest_Azimuthal_Gap_Between_Azimuthally_Adjacent_Stations"=gap,
             "Method_to_Determine_Magnitude"=magType,"Event_Type"=type,Title=title)|>
      select("Magnitude","Time_UTC_milliseconds_since_epoch_","Place",
             "Data_Updated_On_UTC_milliseconds_since_epoch_",
             "Time_Zone_Offset","Url",
             "Detail","Felt_Reports","Maximum_Reported_Intensity",
             "Maximum_Estimated_Instrumental_Intensity",
             "Alert","Status","Tsunami_Flag","Significance_0_through_1000_",
             "Main_Network_Contributor","Code","Ids","Network_Contributors",
             "Product_Types","Number_of_Seismic_Stations_Utilized",
             "Horizontal_Distance_from_Epicenter_to_Nearest_Station",
             "Root_Mean_Square_Travel_Time_Residual_sec_",
             "Largest_Azimuthal_Gap_Between_Azimuthally_Adjacent_Stations",
             "Method_to_Determine_Magnitude","Event_Type","Title")
    
    #Allow the temp_df to be subsetted if specific columns are selected, but default to all columns
    ifelse(is.null(input$columns),Final_Data<-temp_df,
           Final_Data<-temp_df|>
             select(input$columns))
    
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
  
  #Create Dynamic Ui for Summary Types
  output$SummaryDropdown<-renderUI({
    NumericVariables<-c("Magnitude","Time_UTC_milliseconds_since_epoch_",
                       "Data_Updated_On_UTC_milliseconds_since_epoch_",
                       "Felt_Reports","Maximum_Reported_Intensity",
                       "Maximum_Estimated_Instrumental_Intensity",
                       "Significance_0_through_1000_",
                       "Number_of_Seismic_Stations_Utilized",
                       "Horizontal_Distance_from_Epicenter_to_Nearest_Station",
                       "Root_Mean_Square_Travel_Time_Residual_sec_",
                       "Largest_Azimuthal_Gap_Between_Azimuthally_Adjacent_Stations")
    is_var_1_numeric<- input$selectvars1 %in% NumericVariables
    is_var_2_numeric<- input$selectvars2 %in% NumericVariables
    
    if(is_var_1_numeric & is_var_2_numeric){
      selectInput("SummaryDropdown","What Summary?",
                  choices=c("Density Plot","Scatter Plot", "Histogram", "*Creative"))
    }
    else if(!is_var_1_numeric & !is_var_2_numeric){selectInput("SummaryDropdown","What Summary?",
                                                               choices=c("Contingency Table"))}
         else{selectInput("SummaryDropdown","What Summary?",
                          choices=c("Table Summary","Bar Plot"))}
  })
  
  #Dynamically show the faceting option if plots are selected
  output$Faceting<-renderUI({
    if(input$SummaryDropdown %in% c("Density Plot","Scatter Plot","Histogram","Bar Plot","*Creative")){
      checkboxInput("facetbox","Facet Plot?")}
    else{NULL}
  })
  
  #Dynamically Show the faceting variables if faceting is selected
  output$FacetingVars<-renderUI({
    if(input$facetbox){
      selectInput("facetvar","Facet Variable",
                  choices = c("Place","Status","Tsunami_Flag",
                              "Main_Network_Contributor","Network_Contributors",
                              "Method_to_Determine_Magnitude","Event_Type"))}
    else{NULL}
  })
  
  #Create Density Plot
  output$DensityPlot<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1))+
      geom_density()+
      labs(title="Density Plot",
           x=input$selectvars1)
  })
  
  #Create faceted Density Plot
  output$DensityPlotF<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1,fill=input$facetvar))+
      geom_density()+
      facet_wrap(as.formula(paste("~", input$facetvar)))+
      labs(title="Density Plot",
           x=input$selectvars1,
           fill=input$facetvar)
  })
  
  #Create scatterplot
  output$Scatterplot<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1,y=input$selectvars2))+
      geom_point()+
      labs(title="Scatterplot",
           x=input$selectvars1,
           y=input$selectvars2)
  })
  #Create faceted scatterplot
  output$ScatterplotF<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1,y=input$selectvars2))+
      geom_point()+
      facet_wrap(as.formula(paste("~", input$facetvar)))+
      labs(title="Scatterplot",
           x=input$selectvars1,
           y=input$selectvars2)
  })
  #Create histogram
  output$Histogram<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1))+
      geom_histogram()+
      labs(title="Histogram",
           x=input$selectvars1)
  })
  #Create faceted histogram
  output$HistogramF<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1))+
      geom_histogram()+
      facet_wrap(as.formula(paste("~", input$facetvar)))+
      labs(title="Histogram",
           x=input$selectvars1)
  })
  #Create barplot
  output$Barplot<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1,y=input$selectvars2))+
      geom_bar(stat="identity")+
      labs(title="Bar Plot",
           x=input$selectvars1,
           y=input$selectvars2)
  })
  #Create faceted barplot
  output$BarplotF<-renderPlot({
    data<-Final_Data()
    
    ggplot(data,aes_string(x=input$selectvars1,y=input$selectvars2))+
      geom_bar(stat="identity")+
      facet_wrap(as.formula(paste("~", input$facetvar)))+
      labs(title="Bar Plot",
           x=input$selectvars1,
           y=input$selectvars2)
  })
  #Create Numerical Table Summary
  output$Table<-renderTable({
    data<-Final_Data()
    
    dt<-data|>
      group_by(Group=get(input$selectvars1))|>
      summarise(
        Mean=mean(get(input$selectvars2),na.rm=TRUE),
        SD=sd(get(input$selectvars2),na.rm=TRUE),
        Variance=var(get(input$selectvars2),na.rm=TRUE),
        Minimum=min(get(input$selectvars2),na.rm=TRUE),
        Maximum=max(get(input$selectvars2),na.rm=TRUE))
    
    names(dt)[names(dt)=="Group"]<-input$selectvars1
    dt
  })
  #Create a contingency table
  output$ContTable<-renderTable({
    data<-Final_Data()
    
    table(data[[input$selectvars1]],data[[input$selectvars2]])
  })
  #Create Outputs
  output$Summarys<-renderUI({
    if((input$SummaryDropdown %in% c("Density Plot")) & input$facetbox){
      plotOutput("DensityPlotF")
    }else if(input$SummaryDropdown %in% c("Density Plot")){
      plotOutput("DensityPlot")
    }else if((input$SummaryDropdown %in% c("Scatter Plot")) & input$facetbox){
      plotOutput("ScatterplotF")
    }else if(input$SummaryDropdown %in% c("Scatter Plot")){
      plotOutput("Scatterplot")
    }else if((input$SummaryDropdown %in% c("Histogram")) & input$facetbox){
      plotOutput("HistogramF")
    }else if(input$SummaryDropdown %in% c("Histogram")){
      plotOutput("Histogram")
    }else if((input$SummaryDropdown %in% c("Bar Plot"))&input$facetbox){
      plotOutput("BarplotF")
    }else if(input$SummaryDropdown %in% c("Bar Plot")){
      plotOutput("Barplot")
    }else if(input$SummaryDropdown %in% c("Table Summary")){
      tableOutput("Table")
    }else if(input$SummaryDropdown %in% c("Contingency Table")){
      tableOutput("ContTable")
    }else{NULL}
  })
}

# Run the application 
shinyApp(ui = ui, server = server)

