library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(DT)
library(data.table)
library(ggplot2)
library(rsconnect)

setwd("/home/lofatdairy/code/sialab/covid_vis")

confirmed <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Confirmed.csv")
deaths <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Deaths.csv")
recovered <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_time_series/time_series_19-covid-Recovered.csv")

data = confirmed %>% 
  inner_join(., deaths, by = c("Province.State", "Country.Region", "Lat", "Long"), suffix = c(".confirmed", ".deaths")) %>%
  inner_join(., recovered, by = c("Province.State", "Country.Region", "Lat", "Long"), suffix = c(".recovered", ".recovered"))

today <- read.csv("./csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-15-2020.csv")

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
    sidebarLayout(
      sidebarPanel(
        sliderInput(
          "Date",
          "Dates:",
          min = as.Date("2020-01-22","%Y-%m-%d"),
          max = as.Date("2020-03-25","%Y-%m-%d"),
          value = as.Date("2020-03-25","%Y-%m-%d"),
          timeFormat="%m-%d-%Y",
          animate = (animationOptions(interval = 500, loop = T))
        )
      ),
      mainPanel(
        leafletOutput(outputId = "mymap"), 
      ),
    )
  ),
  # fluidRow(
  #   h1("Click row to display time curves"),
  #   column(12, DT::dataTableOutput('tbl')),
  # ), 
  # fluidRow(
  #   column(4, plotOutput('confirmed', height = 500)),
  #   column(4, plotOutput('deaths', height = 500)),
  #   column(4, plotOutput('recovered', height = 500))
  # )
)

server <- function(input, output, session) {
  #pal <- colorBin(colorRamp(c("#00FF00","#FFDD00","#FF0000")), domain = NULL, bins = 15)
  pal <- colorBin(colorRamp(c("#FFBB00","#FF0000"), interpolate = "linear"), domain = NULL, bins = 15)
  
  output$mymap <- renderLeaflet({
    # leaflet() %>%
    #   addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
    #   setView(lng = 0, lat = 0, zoom = 1.5) %>%
    #   addCircleMarkers(data = hospitals, lng = ~X, lat = ~Y, radius = 10, group = "hospitals") %>%
    #   addLayersControl(
    #     overlayGroups = c("hospitals"),
    #     options = layersControlOptions(collapsed = FALSE)
    #   )
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
  
  # observeEvent(input$Date, {
  #   date <- gsub("\\.0", ".", format(input$Date, "%m.%d.%y"))
  #   
  #   Confirmed = paste0("X", substring(date, 2), ".confirmed")
  #   Deaths = paste0("X", substring(date, 2), ".deaths")
  #   Recovered = paste0("X", substring(date, 2))
  #   
  #   proxy <- leafletProxy("mymap", data = data)
  #   proxy %>% clearMarkers() %>% clearShapes()
  #   proxy %>% addCircles(
  #       data = data,
  #       lat = ~Lat,
  #       lng = ~Long,
  #       weight = 1,
  #       radius = ~log(eval(as.symbol(Confirmed))) * 50000,
  #       fillOpacity = 0.5,
  #       popup=~paste0(
  #         ifelse(Province.State == '', as.character(Country.Region), paste0(Province.State, ", ", Country.Region)),
  #         "<br># Confirmed: ", eval(as.symbol(Confirmed)),
  #         "<br># Recovered: ", eval(as.symbol(Recovered)),
  #         "<br># Deaths: ", eval(as.symbol(Deaths)),
  #         "<br># Active: ", eval(as.symbol(Confirmed)) - eval(as.symbol(Deaths)) - eval(as.symbol(Recovered))
  #       ),
  #       #color=~pal(.5 - ((eval(as.symbol(Recovered)) - eval(as.symbol((Deaths)))) / 2 / eval(as.symbol(((Confirmed)))))),
  #       color = ~pal(sqrt(eval(as.symbol(Deaths)) / eval(as.symbol(Confirmed)))),
  #       label = ~ifelse(Province.State == '', as.character(Country.Region), paste0(Province.State, ", ", Country.Region)),
  #   )
  # })
  # 
  # output$tbl = DT::renderDataTable({
  #   datatable(today[c(1:6)], selection = "single")
  #   }, 
  #   server = FALSE
  # )
  # 
  # 
  # output$confirmed = renderPlot({
  #   selected <- input$tbl_rows_selected
  #   if (length(selected) > 0) {
  #     province <- as.character(today[selected,]$Province.State)
  #     country <- as.character(today[selected,]$Country.Region)
  #     selectedRow <- intersect(which(confirmed$Province.State == province), which(confirmed$Country.Region == country))
  #     
  #     dates <- gsub("X", "", names(confirmed)[-c(1:4)])
  #     
  #     confirmed_data <- data.frame(date = as.Date(dates, format="%m.%d.%y"), confirmed = as.numeric(confirmed[selectedRow,-c(1:4)]))
  #     ggplot(data = confirmed_data, aes(x=date, y=confirmed)) + geom_line(color="orange") + geom_point(color="orange")
  #   }
  # })
  # 
  # output$deaths = renderPlot({
  #   selected <- input$tbl_rows_selected
  #   if (length(selected) > 0) {
  #     province <- as.character(today[selected,]$Province.State)
  #     country <- as.character(today[selected,]$Country.Region)
  #     selectedRow <- intersect(which(deaths$Province.State == province), which(deaths$Country.Region == country))
  #     
  #     dates <- gsub("X", "", names(deaths)[-c(1:4)])
  #     
  #     deaths_data <- data.frame(date = as.Date(dates, format="%m.%d.%y"), deaths = as.numeric(deaths[selectedRow,-c(1:4)]))
  #     ggplot(data = deaths_data, aes(x=date, y=deaths)) + geom_line(color="red") + geom_point(color="red")
  #   }
  # })
  # 
  # output$recovered = renderPlot({
  #   selected <- input$tbl_rows_selected
  #   if (length(selected) > 0) {
  #     province <- as.character(today[selected,]$Province.State)
  #     country <- as.character(today[selected,]$Country.Region)
  #     selectedRow <- intersect(which(recovered$Province.State == province), which(recovered$Country.Region == country))
  #     
  #     dates <- gsub("X", "", names(recovered)[-c(1:4)])
  #     
  #     recovered_data <- data.frame(date = as.Date(dates, format="%m.%d.%y"), recovered = as.numeric(recovered[selectedRow,-c(1:4)]))
  #     ggplot(data=recovered_data, aes(x=date, y=recovered)) + geom_line(color="green") + geom_point(color="green")
  #   }
  # })
}
