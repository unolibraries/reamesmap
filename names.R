library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

data <- readxl::read_excel("GreekData.xlsx")
dataTwo <- readxl::read_excel("ReamesDataTwo.xlsx")
dataThree <- readxl::read_excel("ReamesDataThree.xlsx")

names(data) <- c("Name", "Location", "Lat/Long", "Latitude", "Longitude", "Actual", "Date", "Region", "URL")
names(dataTwo) <- c("NameT", "LocationT", "Lat/LongT", "Latitude", "Longitude", "ActualT", "DateT", "RegionT", "URLT")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100%"),
  tags$head(includeCSS("styles.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(h3("Usage of Hephestian"),
                id = "controls", class = "panel panel-default", top = 10, right = 10,
                fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
                
                # Histogram 
                # plotOutput("histCentile", height = 200),
                # plotOutput("lineTrend", height = 140),
               
                # Region filters 
                selectInput("Region", "Region:", choices = NULL),
                selectInput("Name", "Name:", choices = NULL),
                
                selectInput("RegionT", "Region Two:", choices = NULL),
                
                # 
                # tags$p(tags$small(includeHTML("attr.html")))
                
                # Conditions filter 
                selectInput("Region", "Region:", choices = NULL),
                selectInput("timeframe", "Era:", choices = NULL),
                # temp plot
                plotOutput("timebar", height = 200),
                
                # TODO: Add legend for names to help distinguish points
                 
                tags$p(tags$small(includeHTML("attr.html")))                
  )
)

server <- function(input, output, session) {
 
  # First we set up dropdown options for 1) regions, and 2) time periods 
  region_list <- data$Region
  names(region_list) <- region_list
  regionChoice <- c("All", "None", region_list)
  updateSelectInput(session, "Region", choices = regionChoice)
  
  name_list <- data$Name
  names(name_list) <- name_list
  nameChoice <- c("All", name_list)
  updateSelectInput(session, "Name", choices = nameChoice)
  
  region_listT <- dataTwo$RegionT
  names(region_listT) <- region_listT
  regionChoiceT <- c("All", "None", region_listT)
  updateSelectInput(session, "RegionT", choices = regionChoiceT)
  
  pallete <- brewer.pal(9, "Set1")

  time_list <- data$Date
  names(time_list) <- time_list
  updateSelectInput(session, "timeframe", choices = time_list)
  
  # Setup the color palette for displaying names 
  palette <- brewer.pal(10, "Paired")
  
  colorpal <- reactive({
    colorFactor(palette, data$Name)
  })
  
  output$map <- renderLeaflet({
    leaflet(data) %>% 
      addProviderTiles(provider = "Stamen.Watercolor") %>% 
      fitBounds(~min(Longitude), ~min(Latitude), ~max(Longitude), ~max(Latitude))
  })
  
  filteredData <- reactive({
    if ("All" %in% input$Region && "All" %in% input$Name){
      data
    }
    else if ("All" %in% input$Region && !("All" %in% input$Name)) {
      data %>% filter(Name == input$Name)
    }
    else if (!("All" %in% input$Region) && "All" %in% input$Name) {
      data %>% filter(Region == input$Region)
    }
    else {
      data %>% filter(Region == input$Region,
                      Name == input$Name)
    }
  })
  
  
  filteredDataTwo <- reactive({
    if ("All" %in% input$RegionT){
      dataTwo
    }
    else {
      dataTwo %>% filter(RegionT == input$RegionT)
    }
  })
  
  
  observe({
    pal <- colorpal()
    
    ### Map dataset one
    if ("None" %in% input$Region) {
      leafletProxy("map", data = filteredData()) %>%
        clearMarkerClusters() %>%
        clearMarkers()  
    } else {
      data %>% filter(Region == input$Region)
    }
  })
  
  observe({
    pal <- colorFactor(c("navy", "red", "green", "orange", "pink", "purple", "yellow" ), domain = c("Ἡφαιστίων", "Ἡφαιστόδωρος", "Ἡφαίστιος", "Ἡφαιστόκλῆς", "Ἡφαιστόδοτος", "Ἡφαιστῆς", "Ἡφαιστιάδης"))
   
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>%
      #addTiles() %>%
      addCircleMarkers(radius = 7,
                       stroke = TRUE,
                       opacity = 1,
                       weight = 1,
                       color = '#000000',
                       fillOpacity = 1,
                       fillColor = ~pal(Name),
                       popup = ~paste(Name),
                       clusterOptions = markerClusterOptions()) %>%
      addLegend("bottomleft", pal = pal, values = data$Name,
                  title = "Names",
                  opacity = 1)
    
    ### Map dataset two
    if ("None" %in% input$RegionT) {
      leafletProxy("map", data = filteredDataTwo()) %>%
        clearMarkers()
    } else {
    leafletProxy("map", data = filteredDataTwo()) %>%
      clearMarkers() %>%
      #clearControls() %>%
      #addTiles() %>%
      addMarkers(popup = ~paste(NameT),
                 clusterOptions = markerClusterOptions())
    }
  })
  
  output$timebar <- renderPlot({
    ggplot(data, aes(x = Date)) +
      geom_bar(stat = "count", aes(fill = Region)) +
      theme_minimal() +
      theme(legend.position = "none") +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      ggtitle("TEMPORARY: Number of events") +
      xlab("Time period") +
      ylab("No. of events")
  })
  
}

shinyApp(ui, server)








