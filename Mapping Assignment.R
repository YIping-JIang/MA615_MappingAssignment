library(tidyverse)
library(magrittr)
library(readxl)
library(ggmap)
library(shiny)
library(leaflet)
library(maps)
library(htmlwidgets)

data = read.csv("tmphbl23vi6.csv") %>% 
  select(INCIDENT_NUMBER, OFFENSE_CODE, YEAR, MONTH, Lat, Long)
code <- read_excel("rmsoffensecodes.xlsx")
police = read.csv("Boston_Police_Stations.csv") %>% 
  rename("Long" = "X", "Lat" = "Y") %>% 
  select(Long, Lat, NAME)

data = left_join(x = data,y = code,by = c("OFFENSE_CODE" = "CODE"))
data = na.omit(data)
a <- unique(data$MONTH)
a <- a[order(a)]

library(shiny)

ui <- fluidPage(
  
  titlePanel("Boston Police Stations Crime Incident Reports (Aug 2015 - To Date)"),
  
  sidebarLayout(
    
    sidebarPanel(
      selectInput("cod", "Select an Offense Code", unique(data$OFFENSE_CODE)),
      
      selectInput("ye", "Select Year", unique(data$YEAR)), 
      selectInput("mo", "Select Month", a),
      numericInput("nu", "Select how many crimes to show", value = 30)
    ),
    
    mainPanel(
      leafletOutput(outputId = "Plot", width = 1200, height = 800)
    )
  )
)

server <- function(input, output, session) {
  
  output$Plot = renderLeaflet({
    sub_da = data %>% filter(OFFENSE_CODE == input$cod & YEAR == input$ye & MONTH == input$mo)
    sites = sub_da
    sites = head(sub_da, input$nu)
    bounds <- map(database = "state", c('Massachusetts'), fill = TRUE, plot = FALSE)
    
    icons <- awesomeIcons(
      icon = 'disc',
      iconColor = 'black',
      library = 'ion', 
      markerColor = 'blue',
      squareMarker = F
    )
    
    icons_po <- awesomeIcons(
      icon = 'home', 
      iconColor = 'fa', 
      library = 'ion', 
      markerColor = 'red', 
      squareMarker = T, 
      spin = T
    )
    
    leaflet(data = sites) %>%
      setView(-71.098849, 42.350434, zoom = 12) %>% 
        addProviderTiles(providers$CartoDB.Positron, group = "Map") %>%
        addProviderTiles("Esri.WorldImagery", group = "Satellite") %>% 
        addProviderTiles("Esri.WorldShadedRelief", group = "Relief") %>% 
        addAwesomeMarkers(lng = ~police$Long, lat = ~police$Lat, label = ~police$NAME, 
                          group = "Police Station", icon=icons_po) %>% 
        addAwesomeMarkers(lng = ~Long, lat = ~Lat, label = ~NAME, 
                          group = "OFFENSE", icon=icons) %>%
        addPolygons(data=bounds, group="States", weight=2, fillOpacity = 0) %>%
        addScaleBar(position = "bottomleft") %>%
        addLayersControl(
          baseGroups = c("Map", "Satellite", "Relief"),
          overlayGroups = c("OFFENSE", "States","Police Station"),
          options = layersControlOptions(collapsed = T)
        )
  })
}

shinyApp(ui = ui, server = server)
