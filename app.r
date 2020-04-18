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
library(scales)
library(shinydashboard)

# setwd("/home/ubuntu/covid_vis")
setwd("/home/lofatdairy/code/sialab/covid_vis")

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "maps", icon = icon("dashboard")),
    menuItem("Graphs", tabName = "graphs", icon = icon("th")),
    menuItem("Charts", tabName = "charts", icon = icon("th")),
    menuItem("Filters", tabName = "filters", icon = icon("th")),
    width = 230
  )
)
      
body <- dashboardBody(
  tabItems(
    #first dashboard tab content
    tabItem(tabName = "maps",
            fluidRow(
              sidebarPanel(
                sliderInput("time", 
                          label = h3("Time"), 
                          min = 0, 
                          max = 0, 
                          value = 0, 
                          step = 300000,
                          animate = animationOptions(interval = 100, loop = T),
                          ticks = T
                ),
                radioButtons("markers", label = h3("Placeholder"), choices = c("Cases", "Tests", "Cases Per Capita")),
                radioButtons("counties", label = h3("Placeholder 2"), choices = c("Population", "Beds", "Elderly Population", "Comorbidities"))
              ),
              mainPanel(
                leafletOutput(outputId = "map")
              ),
            ),
            fluidRow(
              column(4,
                     plotOutput("logisticCurve")
              ),
              column(4,
                     plotOutput("posAge")
              ),
              column(4,
                     plotOutput("posRace")
              )
            )
    ),
    #second dashboard tab content
    tabItem(tabName = "graphs", h2("Graphs tab content here")
    ),
    #third dashboard tab content
    tabItem(tabName = "charts",
            h2("Charts tab content here")
    ),
    tabItem(tabName = "filters",
            h2("Filters tab content here")
    )
  )
)

ui <- dashboardPage( 
  skin="blue",
  dashboardHeader(title = "Covid-19 Dashboard"),
  sidebar,
  body
)
    
pal <- colorBin(colorRamp(c("#FFDD00","#FF0000")), domain = NULL, bins = 10)

counties <- readOGR("our_data/US/counties.json")
countyCenters <- fread("our_data/US/county_centers.csv", key = "Location")

beds <- fread("our_data/US/hospitals.csv")
beds <- beds[, .(Beds = sum(BEDS)), by = .(COUNTY, STATE)]
beds[, Location := paste0(COUNTY, ", ", STATE)]
setkey(beds, Location)

#have to clean ths fucking beds dt jfc

server <- function(input, output, session) {
  obs <- fread("our_data/test/test.csv")
  
  # Round to nearest 5 minute
  updateMax <- ceiling(max(obs$Updated) / 300) * 300
  updateMin <- floor(min(obs$Updated) / 300) * 300
  updateSliderInput(
    session, 
    "time", 
    value = as.POSIXct(updateMax, origin = "1970-01-01"), 
    min   = as.POSIXct(updateMin, origin = "1970-01-01"),
    max   = as.POSIXct(updateMax, origin = "1970-01-01"),
    timeFormat = "%b %d %Y, %H:%M"
  )
  
  counties$beds <- beds[paste0(counties$NAME, ", ", counties$STATENAME), Beds]
  counties$beds[is.na(counties$beds)] <- 0 
  
  # counties$ages <- aggregated[paste0(counties$NAME, ", ", counties$STATENAME), Ages]
  # counties$ages[is.na(counties$ages)] <- 0
  
  # TODO: handle the fact that county names are fucked, and that state names are reproduced
  # pop <- fread("our_data/US/census_pop_2019.csv")
  # pop$CTYNAME[1835] <- "Dona Ana County"
  # pop <- pop[!(CTYNAME == "District of Columbia" & COUNTY == 1)]
  # pop[Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
  # setkey(pop, Location)
  
  listener <- reactive({
    list(input$markers, input$time)
  })
  
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng = -97, lat = 39, zoom = 3) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(ages == 0, .5, .7),
                  color = ~pal(as.numeric(beds)),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", ages),
                  group = "population"
      ) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(ages == 0, .5, .7),
                  color = ~pal(as.numeric(beds)),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", ages),
                  group = "beds"
      ) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(ages == 0, .5, .7),
                  color = ~pal(as.numeric(beds)),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", ages),
                  group = "elderly"
      ) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(ages == 0, .5, .7),
                  color = ~pal(as.numeric(beds)),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", ages),
                  group = "comorbidity"
      )
  })
  
  observeEvent(listener(), {
    unixTime <- as.numeric(input$time)
    aggregated <- obs[Updated < unixTime, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
    aggregated[, Location := paste0(County, ", ", State)]
    setkey(aggregated, Location)
    
    if (input$markers == "Tests") {
      aggregated[, Markers := Tests]
    }
    
    if (input$markers == "Cases") {
      aggregated[, Markers := Positive]
    }
    
    if (input$markers == "Cases per Capita") {
      aggregated[, Markers := Tests]
    }
    
    aggregated$Lat <- countyCenters[aggregated$Location, Lat]
    aggregated$Long <- countyCenters[aggregated$Location, Long]
    
    leafletProxy("map", data = aggregated) %>%
      clearGroup(group = "marker") %>%
      addCircleMarkers(lng = ~Long, 
                 lat = ~Lat, layerId = ~Location,
                 radius = ~log10(Markers) * 5,
                 opacity = .6,
                 color = ~ifelse(input$markers == "Tests", "#FFDD00", "#FF0000"), 
                 stroke = F,
                 group = "marker",
                 label = ~ifelse(State == '', as.character(County), Location),
                 popup = ~ifelse(State == '', as.character(County), Location),
      )
  })
  
  observeEvent(input$counties, {
    groupToShow = "population"
    
    if (input$counties == "Beds") {
      groupToShow = "beds"
    }
    if (input$counties == "Elderly Population") {
      groupToShow = "elderly"
    }
    if (input$counties == "Comorbidities") {
      groupToShow = "comorbidities"
    }
    leafletProxy("map", data = aggregated) %>%
      hideGroup("population") %>%
      hideGroup("beds") %>%
      hideGroup("elderly") %>%
      hideGroup("comorbidities") %>%
      showGroup(groupToShow)
  })
  
  observeEvent(input$map_shape_click, {
    print("click")
    if (input$map_shape_click$group != "marker") {
      return()
    }
    
    id <- strsplit(input$map_shape_click$id, ", ")[[1]]
    output$logisticCurve <- renderPlot({
      subset <- obs[County == id[1] & State == id[2] & Positive]
      setorder(subset, Updated)
      subset[, nCases := as.numeric(row.names(subset))]
      ggplot(subset, aes(x = Updated, y = nCases)) + 
        geom_step() + 
        theme_minimal() +
        scale_y_continuous(name = "Number of Cases") +
        scale_x_continuous(name = "Date", labels = function(x) {as.Date(as.POSIXct(x, origin = "1970-01-01"))})
    })
    
    output$posRace <- renderPlot({
      subset <- obs[County == id[1] & State == id[2] & Positive]
      ggplot(subset, aes(Race)) + 
        geom_bar() + 
        theme_minimal()
    })
    
    output$posAge <- renderPlot({
      subset <- obs[County == id[1] & State == id[2] & Positive]
      ggplot(subset, aes(Age)) +
        geom_histogram(bins = 5) +
        theme_minimal()
    })
  })
}

shinyApp(ui, server)