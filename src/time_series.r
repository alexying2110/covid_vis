library(shiny)
library(leaflet)
library(dplyr)
library(leaflet.extras)

setwd("/home/lofatdairy/code/sialab/covid_vis")
confirmed <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recovered <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

data = confirmed %>% 
  inner_join(., deaths, by = c("Province.State", "Country.Region", "Lat", "Long"), suffix = c(".confirmed", ".deaths")) %>%
  inner_join(., recovered, by = c("Province.State", "Country.Region", "Lat", "Long"), suffix = c(".recovered", ".recovered"))

ui <- fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput(
        "Date",
        "Dates:",
        min = as.Date("2020-01-22","%Y-%m-%d"),
        max = as.Date("2020-03-13","%Y-%m-%d"),
        value = as.Date("2020-03-13","%Y-%m-%d"),
        timeFormat="%m-%d-%Y",
        animate = (animationOptions(interval = 500, loop = T))
      )
    ),
    mainPanel(
      leafletOutput(outputId = "mymap"), 
    ),
  )
)

server <- function(input, output, session) {
  pal <- colorBin(colorRamp(c("#00FF00","#FFDD00","#FF0000")), domain = NULL, bins = 15)
  pal <- colorBin(colorRamp(c("#FFBB00","#FF0000"), interpolate = "linear"), domain = c(0:1), bins = 10)
  
  output$mymap <- renderLeaflet({
    leaflet(data) %>%
      #setView(lng = 0, lat = 0, zoom = 1.5) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels)
  })
  
  observeEvent(input$Date, {
    date <- gsub("\\.0", ".", format(input$Date, "%m.%d.%y"))
    
    Confirmed = paste0("X", substring(date, 2), ".confirmed")
    Deaths = paste0("X", substring(date, 2), ".deaths")
    Recovered = paste0("X", substring(date, 2))
    
    proxy <- leafletProxy("mymap", data = data)
    proxy %>% clearMarkers() %>% clearShapes()
    proxy %>% addCircles(
        data = data,
        lat = ~Lat,
        lng = ~Long,
        weight = 1,
        radius = ~log(eval(as.symbol(Confirmed))) * 50000,
        fillOpacity = 0.5,
        popup=~paste0(
          ifelse(Province.State == '', as.character(Country.Region), paste0(Province.State, ", ", Country.Region)),
          "<br># Confirmed: ", eval(as.symbol(Confirmed)),
          "<br># Recovered: ", eval(as.symbol(Recovered)),
          "<br># Deaths: ", eval(as.symbol(Deaths)),
          "<br># Active: ", eval(as.symbol(Confirmed)) - eval(as.symbol(Deaths)) - eval(as.symbol(Recovered))
        ),
        #color=~pal(.5 - ((eval(as.symbol(Recovered)) - eval(as.symbol((Deaths)))) / 2 / eval(as.symbol(((Confirmed)))))),
        color = ~pal(sqrt(eval(as.symbol(Deaths)) / eval(as.symbol(Confirmed)))),
        label = ~ifelse(Province.State == '', as.character(Country.Region), paste0(Province.State, ", ", Country.Region)),
    )
  })
}
