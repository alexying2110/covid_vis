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
# setwd("/home/lofatdairy/code/sialab/covid_vis")

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
                          animate = T,
                          ticks = T
                )
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
countyCenters <- fread("our_data/US/county_centers.csv")

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
  
    #pop <- fread("our_data/US/census_pop_2019.csv")
    #pop$CTYNAME[1835] <- "Dona Ana County"
    #pop[, Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
    #setkey(pop, Location)
    # Basically the same thing as the previous line except now we match counties to the number of tests
    #counties$pop <- pop[paste0(counties$NAME, ", ", counties$STATENAME), POPESTIMATE2019]
    
    
    # counties$pop <- pop[paste0(counties$NAME, ", ", counties$STATENAME), POPESTIMATE2019]
  tests <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Tests]
  tests[is.na(counties$tests)] <- 0
  testsReact <- reactiveVal(tests)
  beds <- fread("our_data/US/hospitals.csv")
  beds <- beds[, .(Beds = sum(BEDS)), by = .(COUNTY, STATE)]
  beds[, Location := paste0(COUNTY, ", ", STATE)]
  setkey(beds, Location)

  counties$beds <- beds[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Beds]
  counties$beds[is.na(counties$beds)] <- 0 
  
  counties$cases <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Positive]
  counties$cases[is.na(counties$cases)] <- 0

  counties$tests <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Tests]
  counties$tests[is.na(counties$tests)] <- 0
  
  # counties$ages <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Ages]
  # counties$ages[is.na(counties$ages)] <- 0

  observeEvent(input$time, {
    unixTime <- as.numeric(input$time)
    
    output$mymap <- renderLeaflet({
      leaflet(counties) %>%
        addTiles() %>%
        addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
        addPolygons(stroke = FALSE, 
                    smoothFactor = 0.3,
                    fillOpacity = ~ifelse(cases == 0, .5, .7),
                    color = ~if_else(cases == 0, "#00FF00", pal(as.numeric(log10(tests)))),
                    label = ~paste0(NAME, ", ", STATENAME, ": ", cases),
                    group = "Positive"
        )%>%
        addPolygons(stroke = FALSE, 
                    smoothFactor = 0.3,
                    fillOpacity = ~ifelse(tests == 0, .5, .7),
                    color = ~if_else(tests == 0, "#00FF00", pal(as.numeric(log10(tests)))),
                    label = ~paste0(NAME, ", ", STATENAME, ": ", tests),
                    group = "Tested"
        ) %>%
        # addPolygons(stroke = FALSE, 
        #             smoothFactor = 0.3,
        #             fillOpacity = ~ifelse(ages == 0, .5, .7),
        #             color = ~if_else(ages == 0, "#00FF00", pal(as.numeric(log10(ages)))),
        #             label = ~paste0(NAME, ", ", STATENAME, ": ", ages),
        #             group = "Ages"
        # ) %>%
        addPolygons(stroke = FALSE, 
                    smoothFactor = 0.3,
                    fillOpacity = ~ifelse(beds == 0, .5, .7),
                    color = ~if_else(beds == 0, "#00FF00", pal(as.numeric(log10(beds)))),
                    label = ~paste0(NAME, ", ", STATENAME, ": ", beds),
                    group = "Hospital Beds"
        ) %>%
        addLayersControl(
          baseGroups = c("Positive", "Tested", "Ages", "Hospital Beds"),
          options = layersControlOptions(collapsed = FALSE)
        )
    })
  }) #closing bracket for observeEvent()
  
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(cases == 0, .5, .7),
                  color = ~if_else(cases == 0, "#00FF00", pal(as.numeric(log10(tests)))),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", cases),
                  group = "Positive"
      )%>%
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(tests == 0, .5, .7),
                  color = ~if_else(tests == 0, "#00FF00", pal(as.numeric(log10(tests)))),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", tests),
                  group = "Tested"
      ) %>%
      # addPolygons(stroke = FALSE, 
      #             smoothFactor = 0.3,
      #             fillOpacity = ~ifelse(ages == 0, .5, .7),
      #             color = ~if_else(ages == 0, "#00FF00", pal(as.numeric(log10(ages)))),
      #             label = ~paste0(NAME, ", ", STATENAME, ": ", ages),
      #             group = "Ages"
      # ) %>%
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(beds == 0, .5, .7),
                  color = ~if_else(beds == 0, "#00FF00", pal(as.numeric(log10(beds)))),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", beds),
                  group = "Hospital Beds"
      ) %>%
      addLayersControl(
        baseGroups = c("Positive", "Tested", "Ages", "Hospital Beds"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  observeEvent(input$time, {
    time <- as.numeric(input$time)
  })
  
  observeEvent(input$map_shape_click, {
    id <- strsplit(input$map_shape_click$id, ",")[[1]]
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
