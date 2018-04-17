library(tidyverse)
library(leaflet)
library(shiny)
library(RColorBrewer)

data <- readxl::read_excel("GreekData.xlsx")
names(data) <- c("Name", "Location", "Lat/Long", "Latitude", "Longitude", "Actual", "Date", "Region", "URL")

ui <- bootstrapPage(
  tags$style(type = "text/css", "html, body { width: 100%; height: 100%"),
  tags$head(includeCSS("styles.css")),
  leafletOutput("map", width = "100%", height = "100%"),
  absolutePanel(h3("Usage of Hephestian"),
                id = "controls", class = "panel panel-default", top = 10, right = 10,
                fixed = TRUE, draggable = FALSE, width = 330, height = "auto",
               
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
  updateSelectInput(session, "Region", choices = region_list)
  
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
   data %>% filter(Region == input$Region)
   })
  
  observe({
    pal <- colorFactor(c("navy", "red", "green", "orange", "pink", "purple", "yellow" ), domain = c("Ἡφαιστίων", "Ἡφαιστόδωρος", "Ἡφαίστιος", "Ἡφαιστόκλῆς", "Ἡφαιστόδοτος", "Ἡφαιστῆς", "Ἡφαιστιάδης"))
   
    leafletProxy("map", data = filteredData()) %>%
      clearMarkerClusters() %>%
      clearMarkers() %>%
      clearControls() %>%
      addCircleMarkers(radius = 7,
                       stroke = TRUE,
                       fillOpacity = 1,
                       color = ~pal(Name),
                       popup = ~paste(Name),
                       clusterOptions = markerClusterOptions())
    
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
