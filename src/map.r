library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

setwd("/home/lofatdairy/code/sialab/covid_vis")
data <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-13-2020.csv")

ui <- fluidPage(
  mainPanel(
    leafletOutput(outputId = "mymap"), 
  )
)

server <- function(input, output, session) {
  #pal <- colorBin(colorRamp(c("#00FF00","#EEFF00","#FF0000"), interpolate = "linear"), domain = c(0:1), bins = 15)
  pal <- colorBin(colorRamp(c("#FFBB00","#FF0000"), interpolate = "linear"), domain = c(0:1), bins = 10)
  
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      setView(lng = -99, lat = 45, zoom = 2) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addCircles(
        data = data, 
        lat = ~Latitude, 
        lng = ~Longitude, 
        weight = 1, 
        radius = ~log(Confirmed) * 50000, 
        #radius = ~Confirmed * 50, 
        fillOpacity = 0.5,
        popup=~paste0(
          ifelse(Province.State == '', as.character(Country.Region), paste0(Province.State, ", ", Country.Region)),
          "<br># Confirmed: ", Confirmed, 
          "<br># Recovered: ", Recovered, 
          "<br># Deaths: ", Deaths, 
          "<br># Active: ", Confirmed - Deaths - Recovered,
          "<br>Rel. Lethality: ", round(Deaths / Confirmed, 3),
          "<br>Recovery Rate: ", round(Recovered / Confirmed, 3)
        ), 
        color=~pal(.5 - ((Recovered - Deaths) / 2 / Confirmed)),
        #color=~pal(Deaths / Confirmed),
        label = ~ifelse(Province.State == '', as.character(Country.Region), paste0(Province.State, ", ", Country.Region)), 
      )
  })
}
