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
library(HistogramTools)


# setwd("/home/ubuntu/covid_vis")
#setwd("/home/lofatdairy/code/sialab/covid_vis")

# pop <- fread("our_data/US/census_pop_2019.csv")
# pop$CTYNAME[1835] <- "Dona Ana County"
# pop <- pop[!(CTYNAME == "District of Columbia" & COUNTY == 1)]
# pop[Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
# setkey(pop, Location)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("Dashboard", tabName = "maps", icon = icon("dashboard")),
    menuItem("Graphs", tabName = "graphs", icon = icon("th")),
    width = 230
  )
)
      
body <- dashboardBody(
  tabItems(
    #first dashboard tab content
    tabItem(tabName = "maps",
            titlePanel("    Map"),
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
    #second tab
    tabItem(tabName = "graphs",
            fluidRow(
              titlePanel("      Distributions"),
              sidebarPanel(
                fluidRow(
                  DTOutput('table'),
                ),
                fluidPage(
                  sliderInput("bins",
                              "Number of bins:",
                              min = 1,
                              max = 20,
                              value = 30
                  ),
                  selectInput("Graph", 
                              "Select data display type",
                              choices = c("Bar/Hist", "Freq")
                  ),
                  selectInput("State1", 
                              "Select a field to create histograms by age",
                              choices = c("Tests", "Positive")
                  ),
                  selectInput("State2", 
                              "Select a field to create bar graph by race",
                              choices = c("Tests", "Positive")
                  ),
                )
              ),
              mainPanel(
                fluidPage(
                  plotOutput(outputId = "myhist"),
                  br(),
                  br(),
                  plotOutput(outputId = "bar"),
                )
              ),
         )
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

beds <- fread("our_data/US/beds.csv", key = "Location")

# TODO: have to clean the fucking beds dt jfc

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
  
  # TODO: handle the fact that county names are fucked, and that state names are reproduced
  
  output$table <- renderDT(
    #obs, # data
    #only certain columns to display
    obs %>% select(2, 7, 9, 5),
    class = "display nowrap compact", # style
    filter = "top" # location of column filters
  )
  
  range<- c(0, 10, 30, 70, 100)
  tb = c(0,10,20,30,40,50,60,70,80,90,100)
  col <- findInterval(tb, range, all.inside = TRUE)
  col[which(col==4)] <- "firebrick1"
  col[which(col==3)] <- "gold"
  col[which(col==2)] <- "darkolivegreen1"
  col[which(col==1)] <- "forestgreen"
  
  output$myhist <- renderPlot({
    bins<-seq(min(agg$Age), max(agg$Age), length.out = input$bins +1)
    
    if(input$State1 == "Tests" && input$Graph =="Bar/Hist"){
      #ylim<-c(0,1.2*max(agg$Tests))
      ylim<-c(0,20)
      hist(
        agg$Age, 
        freq=agg$Tests, 
        #col=("yellow", "red", "yellow"), 
        col = col,
        #border= col,
        breaks = bins,
        main = "Tests by Age", 
        xlab="Age",
        labels= TRUE,
        ylim=ylim
      )
      
    }
    
    if(input$State1 == "Positive" && input$Graph =="Bar/Hist"){
      #ylim<-c(0,1.2*max(agg$Positive))
      ylim<-c(0,12)
      hist(
        agg$Age, 
        freq=agg$Positive, 
        breaks = bins,
        #col=("yellow", "red", "yellow"), 
        col = col,
        #border=col,
        main = "Cases by Age", 
        xlab="Age",
        labels = TRUE,
        ylim=ylim
      )
      
    }
    
    if(input$State1 == "Tests" && input$Graph =="Freq"){
      PlotRelativeFrequency(hist(agg$Age, freq=agg$Tests, breaks = bins, col = col, main = "Relative Frequency of Tests by Age", xlab="Age", ylab= "Freq of tests", labels=TRUE, plot=F))
    }
    
    if(input$State1 == "Positive" && input$Graph =="Freq"){
      PlotRelativeFrequency(hist(agg$Age, freq=agg$Positive, breaks = bins, col = col, main = "Relative Frequency of Cases by Age", xlab="Age", ylab="Freq of cases", labels=TRUE, plot=F))
    }
  })
  
  output$bar <- renderPlot({
    if(input$State2 == "Tests" && input$Graph =="Bar/Hist"){
        ylim<-c(0,1.2*max(RaceTest))
        xx<- barplot(
        RaceTest, 
        main = "Tests per Race", 
        #col = "darkolivegreen1",
        col=rainbow(length(RaceTest)),
        ylab= "Count", xlab="Races", 
        ylim=ylim,
        names.arg=c("Black", "White", "Asian"),
      )
      
      text(x = xx, y = RaceTest, label = RaceTest, pos = 3, cex = 0.8)
      #barplot(agg1$Race, freq=agg1$Tests, main = "Tests per Race", ylab= "Count", xlab="Races", names.arg=c("Black", "White", "Asian"))
    }
    if(input$State2 == "Positive" && input$Graph =="Bar/Hist"){
        ylim<-c(0,1.2*max(RacePos))
        xx<- barplot(
        RacePos, 
        main ="Cases per Race", 
        col=rainbow(length(RaceTest)),
        ylab= "Count", 
        xlab="Races", 
        ylim=ylim,
        names.arg=c("Black", "White", "Asian"),
      )
      text(x = xx, y = RacePos, label = RacePos, pos = 3, cex = 0.8)
      #barplot(agg1$Race, freq=agg1$Positive, main ="Cases per Race", ylab= "Count", xlab="Races", names.arg=c("Black", "White", "Asian"))
    }
    if(input$State2 == "Tests" && input$Graph =="Freq"){
      slices <- c(RaceTest) 
      lbls <- c("Black", "White", "Asian")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Tests Given by Race")
    }
    if(input$State2 == "Positive" && input$Graph =="Freq"){
      slices <- c(RacePos) 
      lbls <- c("Black", "White", "Asian")
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels 
      lbls <- paste(lbls,"%",sep="") # ad % to labels 
      pie(slices,labels = lbls, col=rainbow(length(lbls)),
          main="Cases by Race")
    }
  })
  
  pal = colorBin(colorRamp(c("#ff0000", "#00ff00")), domain = NULL, bins = 20)
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng = -97, lat = 39, zoom = 3) %>%
      # addPolygons(stroke = FALSE,
      #             smoothFactor = 0.3,
      #             color = ~pal(as.numeric(beds)),
      #             label = ~paste0(NAME, ", ", STATENAME),
      #             group = "population"
      # ) %>%
      addPolygons(stroke = FALSE,
                  smoothFactor = 0.3,
                  color = ~pal(log10(as.numeric(beds + 1))),
                  label = ~paste0(NAME, ", ", STATENAME),
                  group = "beds",
                  layerId = ~paste0(NAME, ", ", STATENAME),
                  popup = ~paste0(
                    ifelse(STATENAME == '', as.character(NAME), Location),
                    "<br>Population: ",          "Not yet implemented",
                    "<br>Elderly Population: ",  "Not yet implemented",
                    "<br>Total hospital beds: ", beds,
                    "<br>Smoking Population: ",  "Not yet implemented",
                    "<br> Last Updated: ",       "Not yet implemented"
                  )
      )
      # addPolygons(stroke = FALSE,
      #             smoothFactor = 0.3,
      #             color = ~pal(as.numeric(beds)),
      #             label = ~paste0(NAME, ", ", STATENAME),
      #             group = "elderly"
      # ) %>%
      # addPolygons(stroke = FALSE,
      #             smoothFactor = 0.3,
      #             color = ~pal(as.numeric(beds)),
      #             label = ~paste0(NAME, ", ", STATENAME),
      #             group = "comorbidity"
      # )
  })

  # for histogram
  # County <- obs$County 
  # State <- obs$State
  # Lat <- obs$Lat
  # Long <- obs$Long
  # Positive <- obs$Positive
  # Race <- obs$Race #number of cases by race bar graph, filter by state, county
  # Age <- obs$Age  #number cases by age histogram, filter by state, county
  
  agg <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(Age)]
  agg1 <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(Race)]
  
  RacePos<-agg1[, Positive]
  RaceTest <- agg1[, Tests]
  #AgePos <- agg[, Positive]
  #AgeTest <- agg[, Tests]
  
  # aggregated <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
  # aggregated[, Location := paste0(County, ", ", State)]
  # setkey(aggregated, Location)

  # counties$pop <- pop[paste0(counties$NAME, ", ", counties$STATENAME), POPESTIMATE2019]
  
  
  observeEvent(c(input$time, input$markers), {
    unixTime <- as.numeric(input$time)
    if (unixTime == 0) {
      return(NULL)
    }
    aggregated <- obs[Updated < unixTime, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
    aggregated[, Location := paste0(County, ", ", State)]
    setkey(aggregated, Location)
    if (input$markers == "Tests") {
      aggregated[, Markers := Tests]
    }
    if (input$markers == "Cases") {
      aggregated[, Markers := Positive]
    }
    if (input$markers == "Cases Per Capita") {
      aggregated[, Markers := Tests]
    }
    aggregated$Lat <- countyCenters[aggregated$Location, Lat]
    aggregated$Long <- countyCenters[aggregated$Location, Long]
    leafletProxy("map", data = aggregated) %>%
      clearGroup(group = "marker") %>%
      addCircleMarkers(
                 lng = ~Long, 
                 lat = ~Lat, 
                 layerId = ~Location,
                 radius = ~log10(Markers) * 5,
                 opacity = 0.6,
                 color = ~ifelse(input$markers == "Tests", "#FFDD00", "#FF0000"),
                 stroke = T, 
                 weight = 0.8,
                 group = "marker",
                 label = ~ifelse(State == '', as.character(County), Location),
                 popup = ~paste0(
                   ifelse(State == '', as.character(County), Location),
                   "<br># Positive: ",          eval(Positive),
                   "<br># Tested: ",            eval(Tests),
                   "<br>Population: ",          "Not yet implemented",
                   "<br>Elderly Population: ",  "Not yet implemented",
                   "<br>Total hospital beds: ", "Not yet implemented",
                   "<br>Smoking Population: ",  "Not yet implemented",
                   "<br> Last Updated: ",       "Not yet implemented"
                 )
      )
  })
  
  observeEvent(input$map_shape_click, {
    id <- strsplit(input$map_shape_click$id, ", ")[[1]]
    locationObs <- obs[County == id[1] & State == id[2] & Positive]
    output$posRace <- renderPlot({
      ggplot(locationObs, aes(Race)) + 
        geom_bar() + 
        theme_minimal()
    })
    output$posAge <- renderPlot({
      ggplot(locationObs, aes(Age)) +
        geom_histogram(bins = 5) +
        theme_minimal()
    })
    output$logisticCurve <- renderPlot({
      setorder(locationObs, Updated)
      locationObs[, nCases := as.numeric(row.names(locationObs))]
      updateData <- locationObs[, .(Updated, nCases)]
      updateData <- rbind(data.frame(Updated = updateMin, nCases = 0), updateData)
      ggplot(updateData, aes(x = Updated, y = nCases)) + 
        geom_step() + 
        theme_minimal() +
        scale_y_continuous(name = "Number of Cases") +
        scale_x_continuous(name = "Date", labels = function(x) {as.Date(as.POSIXct(x, origin = "1970-01-01"))})
    })
  })
  
  observeEvent(input$map_marker_click, {
    id <- strsplit(input$map_marker_click$id, ", ")[[1]]
    locationObs <- obs[County == id[1] & State == id[2] & Positive]
    output$posRace <- renderPlot({
      ggplot(locationObs, aes(Race)) +
        geom_bar() +
        theme_minimal()
    })
    output$posAge <- renderPlot({
      ggplot(locationObs, aes(Age)) +
        geom_histogram(bins = 5) +
        theme_minimal()
    })
    output$logisticCurve <- renderPlot({
      setorder(locationObs, Updated)
      locationObs[, nCases := as.numeric(row.names(locationObs))]
      updateData <- locationObs[, .(Updated, nCases)]
      updateData <- rbind(data.frame(Updated = updateMin, nCases = 0), updateData)
      ggplot(updateData, aes(x = Updated, y = nCases)) +
        geom_step() +
        theme_minimal() +
        scale_y_continuous(name = "Number of Cases") +
        scale_x_continuous(name = "Date", labels = function(x) {as.Date(as.POSIXct(x, origin = "1970-01-01"))})
    })
  })
  
  
  #for the graphs tab
  observeEvent(input$tableId_row_last_clicked , {
    id <- strsplit(input$tableID_row_last_clicked$id, ", ")[[1]]
    locationObs <- obs[State == id[1]]
    output$posRace <- renderPlot({
      ggplot(locationObs, aes(Race)) + 
        geom_bar() + 
        theme_minimal()
    })
    output$posAge <- renderPlot({
      ggplot(locationObs, aes(Age)) +
        geom_histogram(bins = 5) +
        theme_minimal()
    })
  })
  
  # observeEvent(input$counties, {
  #   groupToShow = "population"
  #   
  #   if (input$counties == "Beds") {
  #     groupToShow = "beds"
  #   }
  #   if (input$counties == "Elderly Population") {
  #     groupToShow = "elderly"
  #   }
  #   if (input$counties == "Comorbidities") {
  #     groupToShow = "comorbidities"
  #   }
  #   leafletProxy("map", data = aggregated) %>%
  #     hideGroup("population") %>%
  #     hideGroup("beds") %>%
  #     hideGroup("elderly") %>%
  #     hideGroup("comorbidities") %>%
  #     showGroup(groupToShow)
  # })
}

shinyApp(ui, server)