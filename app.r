library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(readr)
library(dplyr)
library(shinyjs)
library(htmlwidgets)
library(DT)
library(data.table)
library(bit64)
library(ggplot2)
library(rsconnect)
library(profvis)
library(scales)
library(shinydashboard)
library(HistogramTools)
library(shinyWidgets)
library(ggplot2)

# setwd("/home/ubuntu/covid_vis")
setwd("/home/lofatdairy/code/sialab/covid_vis")

# pop <- fread("our_data/US/census_pop_2019.csv")
# pop$CTYNAME[1835] <- "Dona Ana County"
# pop <- pop[!(CTYNAME == "District of Columbia" & COUNTY == 1)]
# pop[Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
# setkey(pop, Location)

states<-fread("our_data/test/test.csv")

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
              useShinyjs(),
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
                #radioButtons("counties", label = h3("Placeholder 2"), choices = c("Population", "Beds", "Elderly Population", "Comorbidities")),
                radioButtons("markers", label = h3("Display"), choices = c("Cases", "Tests")),
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
    ),
    #second tab
    tabItem(tabName = "graphs",
            fluidRow(
                titlePanel("      Distributions"),
                fluidPage(
                  DTOutput('table')
                ),
              sidebarPanel(
                fluidPage(
                  br(),
                  selectInput("State1", 
                              "Select a field to create histogram by age",
                              choices = c("Tested", "Positive")
                  ),
                  sliderInput("bins",
                              "Number of bins:",
                              min = 1,
                              max = 20,
                              value = 30
                  ),
                  radioButtons("Graph1", "Display data type", choices = c("Counts", "Freq")),
                  selectInput(inputId = "location1", label = ("Location to Filter by"),
                              choices = unique(states$State)
                  ),
                  br(), br(),br(),br(), br(), br(),
                  selectInput("State2", 
                              "Select a field to create bar graph by race",
                              choices = c("Tested", "Positive")
                  ),
                  radioButtons("Graph2", "Display data type", choices = c("Counts", "Freq")),
                  selectInput(inputId = "location2", label = ("Location to Filter by"),
                              choices = unique(states$State)
                  ),
                  br(),br(),br(),br(),br(),br(),br(),br(),br(),
                  selectInput("State3", 
                      "Comorbidity data", 
                      choices = list("All", "All-Stacked", "Pediatric", "Adult"),
                      #selected = "Pediatric"
                  ),
                  radioButtons("Graph3", "Display data type", choices = c("Counts", "Freq")),
                  selectInput(inputId = "location3", label = ("Location to Filter by"),
                              choices = unique(states$State)
                  ),
                  br(),br(),br()
                )
              ),
              mainPanel(
                fluidPage(
                  plotOutput(outputId = "myhist"),
                  br(),
                  plotOutput(outputId = "bar"),
                  br(),
                  plotOutput(outputId = "cobar")
                )
              )
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
    
counties <- readOGR("our_data/US/counties.json")

countyCenters <- fread("our_data/US/county_centers.csv", key = "Location")

beds <- fread("our_data/US/beds.csv", key = "Location")

#comorbidities <- fread("our_data/US/counties.json")

# TODO: have to clean the fucking beds dt jfc

server <- function(input, output, session) {
  obs <- fread("our_data/test/test.csv")
  maxObs <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
  MAX_TESTS <- max(maxObs$Tests)
  MAX_POS <- max(maxObs$Positive)
  observe({
    invalidateLater(5 * 60 * 1000, session)
    obs <- fread("our_data/test/test.csv")
  })
  
  #age and comorbidity
  comorbs <- fread("our_data/US/Medical_Conditions.csv")
  #chars <- fread("our_data/US/Characteristics.csv")
  
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
  
  agg <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(Age)]
  agg1 <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(Race)]
  agg2 <- obs[, .(Tests = length(Positive), Age = Age, Positive = sum(Positive)), by = .(State)]
  morbs <- comorbs[, .(Child = CHILDREN, Adult=ADULTS, Condition = CONDITION, percentChild=CHILDRENPERCENT, percentAdult = ADULTSPERCENT, totalCount = CHILDREN+ADULTS), by = .(CONDITION)]
  
  stacked<- comorbs[, .(Child = CHILDREN, Adult=ADULTS),  by = .(CONDITION)]
  count<- morbs[, totalCount]
  Cmorbs <-morbs[, Child]
  Amorbs <-morbs[, Adult]
  
  Cpercentmorbs<-morbs[, percentChild]
  Apercentmorbs<-morbs[, percentAdult]
  percentmorbs<-morbs[, totalCount]
  
  #AgePos<-agg[, Positive]
  #AgeTest<-agg[, Tests]
  RacePos<-agg1[, Positive]
  RaceTest <- agg1[, Tests]
  
  #locPos <- obs[State == State & Positive & length(Positive)] #positive cases
  #locTest <- obs[State == State & length(Positive)] #tested cases
  observeEvent(input$location1, {
    #locPos <- obs[State == State & Positive, by = .(Age)] #positive cases
    locTests<-obs[State == State & length(Positive), .(Tests = length(Positive), Positive = sum(Positive)), by = .(Age)]
    locPos <- obs[State == State & Positive] #positive cases
    #locTests<-obs[State == State & length(Positive)]
    
    output$myhist <- renderPlot({
      bins<-seq(min(agg$Age), max(agg$Age), length.out = input$bins +1)
        #ggplot(locPos, aes(Age)) +geom_histogram(bins = 5) +theme_minimal()
        #ggplot(agg2, aes(Tests)) +geom_histogram(bins = 5) +theme_minimal()
      if(input$State1 == "Tested" && input$Graph1 =="Counts"){
        ylim<-c(0,1.2*max(agg$Tests))
        ylim<-c(0,20)
        #hist(agg$Age, freq=agg$Tests, col=rainbow(length(agg$Tests)), breaks = bins, main = "Tests by Age", xlab="Age", labels= TRUE, ylim=ylim, panel.first = grid(lty= "solid"))
        hist(locTests, freq=agg$Tests, col=rainbow(length(agg$Tests)), breaks = bins, main = "Tests by Age", xlab="Age", labels= TRUE, ylim=ylim, panel.first = grid(lty= "solid"))
        #ggplot(locTests, aes(Age)) +geom_histogram(bins = bins) +theme_minimal()
      }
      
      if(input$State1 == "Positive" && input$Graph1 =="Counts"){
        ylim<-c(0,1.2*max(agg$Positive))
        ylim<-c(0,12)
        #hist(agg$Age, freq=agg$Positive, breaks = bins, col=rainbow(length(agg$Positive)),  main = "Cases by Age", xlab="Age", labels = TRUE, ylim=ylim, panel.first = grid(lty= "solid"))
        hist(locPos, freq=agg$Positive, breaks = bins, col=rainbow(length(agg$Positive)),  main = "Cases by Age", xlab="Age", labels = TRUE, ylim=ylim, panel.first = grid(lty= "solid"))
      }
      
      if(input$State1 == "Tested" && input$Graph1 =="Freq"){
        PlotRelativeFrequency(hist(agg$Age, freq=agg$Tests, breaks = bins, col=rainbow(length(agg$Age)), main = "Relative Frequency of Tests by Age", xlab="Age", ylab= "Freq of tests", labels=TRUE, plot=F, panel.first = grid(lty= "solid")))
        #PlotRelativeFrequency(hist(locTests, freq=agg$Tests, breaks = bins, col=rainbow(length(agg$Age)), main = "Relative Frequency of Tests by Age", xlab="Age", ylab= "Freq of tests", labels=TRUE, plot=F, panel.first = grid(lty= "solid")))
      }
      
      if(input$State1 == "Positive" && input$Graph1 =="Freq"){
        PlotRelativeFrequency(hist(agg$Age, freq=agg$Positive, breaks = bins, col=rainbow(length(agg$Age)), main = "Relative Frequency of Cases by Age", xlab="Age", ylab="Freq of cases", labels=TRUE, plot=F, panel.first = grid(lty= "solid")))
        #PlotRelativeFrequency(hist(locPos, freq=agg$Positive, breaks = bins, col=rainbow(length(agg$Age)), main = "Relative Frequency of Cases by Age", xlab="Age", ylab="Freq of cases", labels=TRUE, plot=F, panel.first = grid(lty= "solid")))
      }
    })
  })
  
  observeEvent(input$location2, {
    output$bar <- renderPlot({
      locPos <- obs[State == State & Positive] #positive cases
      locTests <- obs[State == State & length(Positive)]
      #ggplot(locPos, aes(Race)) +
      #geom_bar() +
      #theme_minimal()
      
      if(input$State2 == "Tested" && input$Graph2 =="Counts"){
        ylim<-c(0,1.2*max(RaceTest))
        #xx<- barplot(RaceTest, main = "Tests per Race", col=rainbow(length(RaceTest)),ylab= "Count", xlab="Races", ylim=ylim, names.arg=c("Black", "White", "Asian"), panel.first = grid(lty= "solid"))
        xx<- barplot(RaceTest, main = "Tests per Race", ylab= "Count", xlab="Races", ylim=ylim, names.arg=c("Black", "White", "Asian"), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown")
        
        text(x = xx, y = RaceTest, label = RaceTest, pos = 3, cex = 0.8)
      }
      if(input$State2 == "Positive" && input$Graph2 =="Counts"){
        ylim<-c(0,1.2*max(RacePos))
        xx<- barplot(RacePos, main ="Cases per Race", ylab= "Count", xlab="Races", ylim=ylim, names.arg=c("Black", "White", "Asian"), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown")
        text(x = xx, y = RacePos, label = RacePos, pos = 3, cex = 0.8)
      }
      if(input$State2 == "Tested" && input$Graph2 =="Freq"){
        slices <- c(RaceTest)
        lbls <- c("Black", "White", "Asian")
        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct) # add percents to labels
        lbls <- paste(lbls,"%",sep="") # ad % to labels
        pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Tests Given by Race")
      }
      if(input$State2 == "Positive" && input$Graph2 =="Freq"){
        slices <- c(RacePos)
        lbls <- c("Black", "White", "Asian")
        pct <- round(slices/sum(slices)*100)
        lbls <- paste(lbls, pct) # add percents to labels
        lbls <- paste(lbls,"%",sep="") # ad % to labels
        pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Cases by Race")
      }
    })
  })
  
  output$table <- renderDT(
    obs %>% select(2, 7, 9, 5),
    class = "display nowrap compact", # style
    filter = "top" # location of column filters
  )
  
   #output$myhist <- renderPlot({
    #bins<-seq(min(agg$Age), max(agg$Age), length.out = input$bins +1)

    # if(input$State1 == "Tested" && input$Graph1 =="Counts"){
    #   ylim<-c(0,1.2*max(agg$Tests))
    #   ylim<-c(0,20)
    #   hist(agg$Age, freq=agg$Tests, col=rainbow(length(agg$Tests)), breaks = bins, main = "Tests by Age", xlab="Age", labels= TRUE, ylim=ylim, panel.first = grid(lty= "solid"))
    # }
    # 
    # if(input$State1 == "Positive" && input$Graph1 =="Counts"){
    #   ylim<-c(0,1.2*max(agg$Positive))
    #   ylim<-c(0,12)
    #   hist(agg$Age, freq=agg$Positive, breaks = bins, col=rainbow(length(agg$Positive)),  main = "Cases by Age", xlab="Age", labels = TRUE, ylim=ylim, panel.first = grid(lty= "solid"))
    # }
    # 
    # if(input$State1 == "Tested" && input$Graph1 =="Freq"){
    #   PlotRelativeFrequency(hist(agg$Age, freq=agg$Tests, breaks = bins, col=rainbow(length(agg$Age)), main = "Relative Frequency of Tests by Age", xlab="Age", ylab= "Freq of tests", labels=TRUE, plot=F, panel.first = grid(lty= "solid")))
    # }
    # 
    # if(input$State1 == "Positive" && input$Graph1 =="Freq"){
    #   PlotRelativeFrequency(hist(agg$Age, freq=agg$Positive, breaks = bins, col=rainbow(length(agg$Age)), main = "Relative Frequency of Cases by Age", xlab="Age", ylab="Freq of cases", labels=TRUE, plot=F, panel.first = grid(lty= "solid")))
    # }
  #})

  #output$bar <- renderPlot({
    # if(input$State2 == "Tested" && input$Graph2 =="Counts"){
    #     ylim<-c(0,1.2*max(RaceTest))
    #     #xx<- barplot(RaceTest, main = "Tests per Race", col=rainbow(length(RaceTest)),ylab= "Count", xlab="Races", ylim=ylim, names.arg=c("Black", "White", "Asian"), panel.first = grid(lty= "solid"))
    #     xx<- barplot(RaceTest, main = "Tests per Race", ylab= "Count", xlab="Races", ylim=ylim, names.arg=c("Black", "White", "Asian"), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown")
    # 
    #     text(x = xx, y = RaceTest, label = RaceTest, pos = 3, cex = 0.8)
    # }
    # if(input$State2 == "Positive" && input$Graph2 =="Counts"){
    #     ylim<-c(0,1.2*max(RacePos))
    #     xx<- barplot(RacePos, main ="Cases per Race", ylab= "Count", xlab="Races", ylim=ylim, names.arg=c("Black", "White", "Asian"), density=c(5,10,20,30,7) , angle=c(0,45,90,11,36) , col="brown")
    #     text(x = xx, y = RacePos, label = RacePos, pos = 3, cex = 0.8)
    # }
    # if(input$State2 == "Tested" && input$Graph2 =="Freq"){
    #   slices <- c(RaceTest)
    #   lbls <- c("Black", "White", "Asian")
    #   pct <- round(slices/sum(slices)*100)
    #   lbls <- paste(lbls, pct) # add percents to labels
    #   lbls <- paste(lbls,"%",sep="") # ad % to labels
    #   pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Tests Given by Race")
    # }
    # if(input$State2 == "Positive" && input$Graph2 =="Freq"){
    #   slices <- c(RacePos)
    #   lbls <- c("Black", "White", "Asian")
    #   pct <- round(slices/sum(slices)*100)
    #   lbls <- paste(lbls, pct) # add percents to labels
    #   lbls <- paste(lbls,"%",sep="") # ad % to labels
    #   pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Cases by Race")
    # }
  #})

  output$cobar <- renderPlot({
    if(input$State3 == "All" && input$Graph3 =="Counts" ){
      ylim<-c(0,1.2*max(count))
      xx<- barplot(count, main = "All Comorbidities", col=rainbow(length(count)),ylim=ylim, ylab= "Count", xlab="Comorbidities", names.arg=c(morbs$Condition), panel.first = grid(lty= "solid"))
      text(x = xx, y = count, label = count, pos = 3, cex = 0.8)
      #barplot(count, main = "All Comorbidities", xlab = "Comorbidities", ylab = "Count", col = c("blue", "green"), names.arg=c(morbs$Condition))
      #legend("topleft", c("Pediatric", "Adult"), fill = c("blue", "green"))
    }
    
    #FIXXXXXXXXX
    if(input$State3 == "All-Stacked" && input$Graph3 =="Counts" ){
      #counts<-table(morbs$Condition, stacked)
      #counts <- table(stacked$Child, stacked$Adult)
      #barplot(counts, main = "All Comorbidities", xlab="Comorbidities", ylab = "Count", col = c("blue", "green"), legend = rownames(counts))
      #legend("topleft", c("Pediatric", "Adult"), fill = c("blue", "green"))

      #data<-data.frame(c(morbs$Condition), c(Cmorbs), c(Amorbs) )
      #ggplot(data, aes(fill=c("Pediatric", "Adult"), y="Count", x="Comorbidity")) +
        #geom_bar(position="stack", stat="identity")

      #test data
      counts <- table(mtcars$vs, mtcars$gear)
      #counts<-table(stacked$Child, stacked$Adult)
      barplot(counts, main = "All Comorbidities", xlab="Comorbidities", ylab = "Count", col = c("blue", "green"), panel.first = grid(lty= "solid"))
      legend("topright", c("Pediatric", "Adult"), fill = c("blue", "green"))

      # create a dataset
      #specie <- c(rep("sorgho" , 2) , rep("poacee" , 2) , rep("banana" , 2) , rep("triticum" , 2) )
      #condition <- rep(c("Pediatric" , "Adult") , 4)

      #specie <- c(rep((morbs$Condition),2 ))
      #condition <- rep(c("Pediatric" , "Adult"), 4)
      #data <- table(condition,specie)

      #barplot(data, main="Pediatric and Adult Comorbidities",
              #xlab="Comorbidities", col=c("blue","green"),
              #legend = rownames(data)
      #)

    }
    if(input$State3 == "Pediatric" && input$Graph3 =="Counts"){
      ylim<-c(0,1.2*max(Cmorbs))
      xx<- barplot(Cmorbs, main = "Child Comorbidities", col=rainbow(length(Cmorbs)),ylim=ylim, ylab= "Count", xlab="Comorbidities", names.arg=c(morbs$Condition), panel.first = grid(lty= "solid"))
      text(x=xx, y=Cmorbs, label=Cmorbs, pos=3, cex=0.8)
    }
    if(input$State3 == "Adult" && input$Graph3 =="Counts" ){
      ylim<-c(0,1.2*max(Amorbs))
      xx<- barplot(Amorbs, main = "Adult Comorbidities", col=rainbow(length(Amorbs)),ylim=ylim, ylab= "Count", xlab="Comorbidities", names.arg=c(morbs$Condition), panel.first = grid(lty= "solid"))
      text(x = xx, y = Amorbs, label = Amorbs, pos = 3, cex = 0.8)
    }
    if(input$State3 == "Pediatric" && input$Graph3 =="Freq"){
      slices <- c(Cpercentmorbs)
      lbls <- c(morbs$Condition)
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Pediatric Cases")
    }
    if(input$State3 == "All" && input$Graph3 =="Freq"){
      slices <- c(percentmorbs)
      lbls <- c(morbs$Condition)
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)), main="All Cases")
    }
    #a percent stacked bar chart
    #FIXXXXXXXXX
    if(input$State3 == "All-Stacked" && input$Graph3 =="Freq" ){

      counts <- table(mtcars$vs, mtcars$gear)

      data_percentage <- apply(counts, 2, function(x){x*100/sum(x,na.rm=T)})
      barplot(data_percentage, main="Percentage of Pediatric and Adult Comorbidities",
              xlab="Comorbidity", col=c("blue","green"), panel.first = grid(lty= "solid"))
      legend("topright", c("Pediatric", "Adult"), fill = c("blue", "green"))
    }
    if(input$State3 == "Adult" && input$Graph3 =="Freq"){
      slices <- c(Apercentmorbs)
      lbls <- c(morbs$Condition)
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=rainbow(length(lbls)), main="Adult Cases")
    }
  })
  
  output$map <- renderLeaflet({
    leaflet(counties) %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      setView(lng = -97, lat = 39, zoom = 3) %>%
      onRender('
        function(el, x, data) {
          let geoObj = JSON.parse(data);
          let counties = geoObj.features;
          countyLayer = L.geoJSON(counties, {
            style: {
              weight: 0.5,
              opacity: 0.7,
              fillOpacity: 0.5
            }
          });
          
          countyLayer.eachLayer(function(layer) {
            layer.on("click", function(ev) {
              Shiny.onInputChange("map_shape_click", {id: layer.feature.properties.Location})
            });
            layer.bindTooltip(layer.feature.properties.Location)
            layer.on("mouseover", function(ev) {
              layer.openTooltip();
            });
            layer.on("mouseout", function(ev) {
              layer.closeTooltip();
            });
            layer.bindPopup(`${layer.feature.properties.Location}<br/>Cases:<br/>Tests:`);
          });
          
          Shiny.addCustomMessageHandler("updateColors", 
            function(aggregated) {
              countyLayer.eachLayer(function(layer) {
                let ind = aggregated.Location.indexOf(layer.feature.properties.Location);
                if (ind > 0) {
                  layer.setStyle({color: aggregated.Markers[ind]})
                } else {
                  layer.setStyle({color: "#00aa00"})
                }
              });
          });
          
          countyLayer.addTo(this);
        }
      ', data = read_file("our_data/US/counties.json"))
  })

  observeEvent(c(input$time, input$markers), {
    unixTime <- as.numeric(input$time)
    if (unixTime == 0) {
      return(NULL)
    }
    
    aggregated <- obs[Updated < unixTime, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
    aggregated[, Location := paste0(County, ", ", State)]
    setkey(aggregated, Location)
    
    pal = colorBin(colorRamp(c("#ffff00", "#ff0000")), domain = c(0:1), bins = 20)
    
    if (input$markers == "Tests") {
      aggregated[, Markers := pal(log10(Tests)/log10(MAX_TESTS))]
    }
    if (input$markers == "Cases") {
      aggregated <- aggregated[Positive > 0,]
      aggregated[, Markers := pal(log10(Positive)/log10(MAX_POS))]
    }
    if (input$markers == "Cases Per Capita") {
      aggregated[, Markers := pal(Tests)]
    }
    
    session$sendCustomMessage(type = "updateColors", aggregated)
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

