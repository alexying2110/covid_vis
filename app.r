library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(DT)
library(data.table)
library(ggplot2)
library(rsconnect)

hospitals <- read.csv("./our_data/US/hospitals.csv")

counties <- readOGR("our_data/US/counties_low.json")
states <- readOGR("our_data/US/states.json")

stateNames <- setNames(as.list(as.character(states$NAME)), states$STATE)

data <- fread("csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv")
data <- data[data$Country_Region == "US"]

counties$STATENAME <- sapply(counties$STATE, function(x) {stateNames[[as.character(x)]]})

counties$cases <- mapply(function(x, y) {data[data$Admin2 == as.character(x) & data$Province_State == as.character(y)]$Confirmed}, counties$NAME, counties$STATENAME)
counties$cases[sapply(counties$cases, function(x) length(x) == 0)] <- 0
counties$cases <- as.numeric(counties$cases)

pal <- colorBin(colorRamp(c("#FFDD00","#FF0000")), domain = NULL, bins = 15)

ui <- fluidPage(
  fluidRow(
    mainPanel(
      leafletOutput(outputId = "mymap")
    )
  )
)

server <- function(input, output, session) {
  pal <- colorBin(colorRamp(c("#FFBB00","#FF0000"), interpolate = "linear"), domain = NULL, bins = 15)
  output$mymap <- renderLeaflet({
    leaflet(counties) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
                  color = ~ifelse(cases == 0, "#00FF00", pal(as.numeric(log10(cases)))),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", cases),
                  group = "Tested"
                ) %>%
      addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = ~ifelse(cases == 0, .5, .7),
                  color = ~ifelse(cases == 0, "#00FF00", pal(as.numeric(log10(cases)))),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", cases),
                  group = "Positive"
                ) %>%
      addLayersControl(
        baseGroups = c("Positive", "Tested"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
}

shinyApp(ui, server)
