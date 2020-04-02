library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(DT)
library(data.table)
library(bit64)
library(ggplot2)
library(rsconnect)
library(profvis)

#setwd("/home/ubuntu/covid_vis")
setwd("/home/lofatdairy/code/sialab/covid_vis")

counties <- readOGR("our_data/US/counties.json")
#data <- fread("csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv")
#data <- data[Country_Region == "US"]

ui <- fluidPage(
  fluidRow(
    mainPanel(
      leafletOutput(outputId = "mymap")
    )
  )
)

pal <- colorBin(colorRamp(c("#FFDD00","#FF0000")), domain = NULL, bins = 10)
server <- function(input, output, session) {
  obs <- fread("our_data/test/test.csv")
  aggregated <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
  aggregated[, Location := paste0(County, ", ", State)]
  setkey(aggregated, Location)
  
  #Remove if_else if counties are actually reported correctly
  counties$cases <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Positive]
  counties$cases[is.na(counties$cases)] <- 0
  
  output$mymap <- renderLeaflet({
    leaflet(counties) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(cases == 0, .5, .7),
                  color = ~if_else(cases == 0, "#00FF00", pal(as.numeric(log10(cases)))),
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