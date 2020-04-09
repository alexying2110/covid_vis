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


#setwd("/home/ubuntu/covid_vis")
setwd("/home/lofatdairy/code/sialab/covid_vis")

# counties <- readOGR("our_data/US/counties.json")
countyCenters <- fread("our_data/US/county_centers")

ui <- fluidPage(
  fluidRow(
    sidebarPanel(
      sliderInput("time", 
                  label = h3("Time"), 
                  min = 0, 
                  max = 0, 
                  value = 0, 
                  animate = T,
                  ticks = T
                  ),
    ),
    mainPanel(
      leafletOutput(outputId = "map")
    )
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
)

server <- function(input, output, session) {
  obs <- fread("our_data/test/test.csv")
  updateSliderInput(
    session, 
    "time", 
    value = as.POSIXct(max(obs$Updated), origin = "1970-01-01"), 
    min   = as.POSIXct(min(obs$Updated), origin = "1970-01-01"),
    max   = as.POSIXct(max(obs$Updated), origin = "1970-01-01"),
    timeFormat = "%b %d %Y, %H:%M"
    )
  
  # TODO: handle the fact that county names are fucked, and that state names are reproduced
  # pop <- fread("our_data/US/census_pop_2019.csv")
  # pop$CTYNAME[1835] <- "Dona Ana County"
  # pop <- pop[!(CTYNAME == "District of Columbia" & COUNTY == 1)]
  # pop[Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
  # setkey(pop, Location)
  
  aggregated <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
  aggregated[, Location := paste0(County, ", ", State)]
  setkey(aggregated, Location)
  
  cases <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Positive]
  cases[is.na(counties$cases)] <- 0
  casesReact <- reactiveVal(cases)
  
  tests <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Tests]
  tests[is.na(counties$tests)] <- 0
  testsReact <- reactiveVal(tests)
  
  observeEvent(input$time, {
    unixTime <- as.numeric(input$time)
    aggregated <- obs[Updated > unixTime, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
    aggregated[, Location := paste0(County, ", ", State)]
    setkey(aggregated, Location)
    
    cases <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Positive]
    cases[is.na(counties$cases)] <- 0
    casesReact(cases)
  })
  
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
      addCircleMarkers(lng = )
  })
  
  observeEvent(input$time, {
    time <- as.numeric(input$time)
  })
  
  observeEvent(input$map_shape_click, {
    id <- strsplit(input$map_shape_click$id, ",")[[1]]
    print(id)
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
