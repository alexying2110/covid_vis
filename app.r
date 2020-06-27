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
library(viridis)
library(usmap)


# Sets directory for relative paths. one line is for use on the EC2 instance, the other is my laptop. 
# plz don't make fun of my username
# setwd("/home/ubuntu/covid_vis")
#setwd("/home/lofatdairy/code/sialab/covid_vis")

# pop <- fread("our_data/US/census_pop_2019.csv")
# pop$CTYNAME[1835] <- "Dona Ana County"
# pop <- pop[!(CTYNAME == "District of Columbia" & COUNTY == 1)]
# pop[Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
# setkey(pop, Location)

#data for states
states<-fread("our_data/test/test.csv")


sidebar <- dashboardSidebar(
  #these are the dashboard tabs
  sidebarMenu(
    menuItem("Dashboard", tabName = "maps", icon = icon("dashboard")),
    menuItem("Graphs", tabName = "graphs", icon = icon("th")),
    menuItem("Case Summary", tabName = "data", icon = icon("th")),
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
               #time slider
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
              column(4, plotOutput("logisticCurve")
              ),
              column(4, plotOutput("posAge")
              ),
              column(4, plotOutput("posRace")
              )
            )
    ),
    #second tab
    tabItem(tabName = "graphs",
         fluidRow(
             fluidPage(
               
               titlePanel("    Distributions"),
                 pickerInput(inputId = "location", label = ("Location(s) to Filter by"), choices = unique(states$State), multiple = TRUE, selected = "New York", options = list(`actions-box` = TRUE)),
              
             ),
           sidebarPanel(
             fluidPage(
               br(),
               selectInput("State1",
                           "Select a field to create histogram by age",
                           choices = c("Tested", "Positive")
               ),
               sliderInput("bins",
                           "Bin width:",
                           min = 1,
                           max = 10,
                           value = 30
               ),
               radioButtons("Graph1", "Display data type", choices = c("Counts", "Freq")),
               
               
               br(), br(),br(),br(), br(), br(),br(),
               selectInput("State2",
                           "Select a field to create bar graph by race",
                           choices = c("Tested", "Positive")
               ),
               radioButtons("Graph2", "Display data type", choices = c("Counts", "Freq")),
               
               br(),br(),br(),br(),br(),br(),br(),br(),br(),br(), br(),br(),
               selectInput("State3",
                   "Comorbidity data",
                   choices = list("All", "All-Stacked", "Pediatric", "Adult"),
               ),
               radioButtons("Graph3", "Display data type", choices = c("Counts", "Freq")),
      
               br(),br(),br(),br(),br(),br(),br(),br(), br()
             )
         ),
         mainPanel(
           fluidPage(
             plotOutput(outputId = "myhist"),
             br(),
             plotOutput(outputId = "bar"),
             br(),
             plotOutput(outputId = "cobar"),
             br(),
           )
         )
         )
    ),
   #add back here
   #third tab
   tabItem(tabName = "data",
           titlePanel("    Case Summary"),
           leafletOutput(outputId = "map1"),
           br(),
           sidebarPanel(
             fluidRow(
               selectInput(inputId = "location4", label = ("Location to Filter by"),
                           choices = unique(states$State)
               )
             )
           ),
           #infoBox("Total Cases", "633", icon = icon("credit-card"), fill = TRUE),
           # Dynamic infoBoxes
           infoBoxOutput("cases"),
           infoBoxOutput("progressBox"), 
           infoBoxOutput("progressBox1"),
           infoBoxOutput("approvalBox")
   )
  )
)

# Note here that any additional graphs, buttons, etc must be added to ui, which just holds a fluid page object


ui <- dashboardPage( 
  skin="blue",
  dashboardHeader(title = "Covid-19 Dashboard"),
  sidebar,
  body
)

# Reads in the geojson describing the county boundaries. This gives leaflet a set of points for each county that
# describes the county's border. you can think of counties as actually a dictionary with 7 key-value pairs, the 
# value of each field each being a vector. the most important fields for our use are NAME and STATENAME, 
# which are the county's name and the state's name, respectively.
# A side note: the reason that counties is declared outside the server function is that this allows for its
# value to be cached, so we don't have to read the big ass json file each time the shiny app is started.
counties <- readOGR("our_data/US/counties.json")
countyCenters <- fread("our_data/US/county_centers.csv", key = "Location")
beds <- fread("our_data/US/beds.csv", key = "Location")
#comorbidities <- fread("our_data/US/counties.json")

# TODO: have to clean the fucking beds dt jfc
# This server function handles the data logic, and is run every time a user opens the app. Note that in R,
# returns are not required to be explicit, and in this case, the output variable is actually what is acessed
# by shiny to determine what is drawn on the UI. That means that we append any graphs, maps, figures, etc, onto
# output.
server <- function(input, output, session) {
  # obs is a data.table object (just like pandas data frames if you're familiar with that), with each
  # observation being a single test taken. each observation is the county, state, lat, long, outcome (positive or negative)
  # update time, race, hispanic, and age of the user/test.
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
  agg <- obs[, .(Tests = length(Positive), Positive = sum(Positive), State = State), by = .(Age)]
  agg1 <- obs[, .(Tests = length(Positive), Positive = sum(Positive), Race = Race), by = .(Race)]
  agg2 <- obs[, .(Tests = length(Positive), Age = Age, Positive = sum(Positive)), by = .(State)]
  morbs <- comorbs[, .(Child = CHILDREN, Adult=ADULTS, Condition = CONDITION, percentChild=CHILDRENPERCENT, percentAdult = ADULTSPERCENT, totalCount = CHILDREN+ADULTS), by = .(CONDITION)]
  
  stacked<- comorbs[, .(Child = CHILDREN, Adult=ADULTS),  by = .(CONDITION)]
  count<- morbs[, totalCount]
  Cmorbs <-morbs[, Child]
  Amorbs <-morbs[, Adult]
  
  Cpercentmorbs<-morbs[, percentChild]
  Apercentmorbs<-morbs[, percentAdult]
  percentmorbs<-morbs[, totalCount]
  
  RacePos<-agg1[, Positive]
  RaceTest <- agg1[, Tests]

  observeEvent(c(input$location, input$State1, input$Graph1), {
    output$myhist <- renderPlot({
       bins<-input$bins
       if(input$Graph1=="Counts"){
         if(input$State1 == "Tested"){
           loc<-filter(obs, State == input$location & length(Positive))
         }
         if(input$State1 == "Positive"){
           loc<-filter(obs, State == input$location & Positive)
         }
         print(ggplot(loc, aes(x=Age)) + geom_histogram(binwidth=bins))
       }
       if(input$Graph1=="Freq"){
         if(input$State1 == "Tested"){
           loc<-filter(obs, State == input$location & length(Positive))
         }
         if(input$State1 == "Positive"){
           loc<-filter(obs, State == input$location & Positive)
         }
         
         print(ggplot(loc, aes(x=Age)) + geom_histogram(aes(y = stat(count) / sum(count)), binwidth=bins))
       }
    })
  })
  
  
  observeEvent(c(input$location, input$State2, input$Graph2), {
    output$bar <- renderPlot({
      if(input$Graph2=="Counts"){
        if(input$State2 == "Tested"){
          loc<-filter(obs, State == input$location & length(Positive))
        }
        if(input$State2 == "Positive"){
          loc<-filter(obs, State == input$location & Positive)
        }
        print(ggplot(loc, aes(x=Race)) + geom_bar() +theme_minimal())
      }
      if(input$Graph2=="Freq"){
        if(input$State2 == "Tested"){
          loc<-filter(obs, State == input$location & length(Positive))
        }
        if(input$State2 == "Positive"){
          loc<-filter(obs, State == input$location & Positive)
          
        }
        Race = loc$Race
        blank_theme <- theme_minimal()+
          theme(
            axis.title.x = element_blank(),
            axis.title.y = element_blank(),
            panel.border = element_blank(),
            panel.grid=element_blank(),
            axis.ticks = element_blank(),
            plot.title=element_text(size=14, face="bold")
          )
        print(ggplot(loc, aes(x="", fill=Race))+
          geom_bar(width = 1)+
          coord_polar("y") + scale_fill_brewer(palette="Blues")+
            blank_theme + theme(axis.text.x=element_blank() +
            geom_text(label = paste0(Race, " ", round(stat(count)/ sum(count) * 100, 1), "%"), size=5))
        )
      }
    })
  })
  
  #FIXXXX, these should change based on state, I have it hardcoded in right now
  observeEvent(c(input$location), {
    output$cases <- renderInfoBox({
      loc<-filter(obs, State == input$location & length(Positive))
      infoBox(
        "Total Cases", "633", icon = icon("credit-card"), fill = TRUE
      )
    })
    output$progressBox <- renderInfoBox({
      infoBox(
        "Residents", "610", icon = icon("list"),
        color = "purple"
      )
    })
    output$progressBox1 <- renderInfoBox({
      infoBox(
        "Non-Residents", "23", icon = icon("list"),
        color = "purple"
      )
    })
    output$approvalBox <- renderInfoBox({
      infoBox(
        "Hospitalizations", "80", icon = icon("thumbs-up", lib = "glyphicon"),
        color = "yellow"
      )
    })
  })
  
  
  output$table <- renderDT(
    obs %>% select(2, 7, 9, 5),
    class = "display nowrap compact", # style
    filter = "top" # location of column filters
  )
  output$table1 <- renderDT(
    morbs %>% select(1, 2, 3),
    class = "display nowrap compact", # style
    filter = "top" # location of column filters
  )
  output$cobar <- renderPlot({
    if(input$State3 == "All" && input$Graph3 =="Counts" ){
      par(mar=c(8,4,4,1)+.1) #bottom, left, top, right margins
      ylim<-c(0,1.2*max(count))
      xx<- barplot(count, main = "All Comorbidities", col="azure4",ylim=ylim, ylab= "Cases", names.arg=c(morbs$Condition), las = 2, cex.names = .7, panel.first = grid(lty= "dotted"))
      text(x = xx, y = count, label = count, pos = 3, cex = 0.8)
    }
    #FIXXXXXXXXX
    if(input$State3 == "All-Stacked" && input$Graph3 =="Counts" ){
      #test data
      counts <- table(mtcars$vs, mtcars$gear)
      #counts<-table(stacked$Child, stacked$Adult)
      barplot(counts, main = "All Comorbidities", xlab="Comorbidities", ylab = "Cases", names.arg=c("Heart disease", "Respiratory illness", "Liver disease"), density=c(30,5) , angle=c(0,45), col = "azure4", panel.first = grid(lty= "dotted"))
      legend("topright", c("Adult", "Pediatric"), fill = "azure4", density=c(30,5) , angle=c(0,45))
    }
    if(input$State3 == "Pediatric" && input$Graph3 =="Counts"){
      par(mar=c(8,4,4,1)+.1) #bottom, left, top, right margins
      ylim<-c(0,1.2*max(Cmorbs))
      xx<- barplot(Cmorbs, main = "Child Comorbidities", col="azure4",ylim=ylim, ylab= "Cases", names.arg=c(morbs$Condition), las = 2, cex.names = .7, panel.first = grid(lty= "dotted"))
      text(x=xx, y=Cmorbs, label=Cmorbs, pos=3, cex=0.8)
    }
    if(input$State3 == "Adult" && input$Graph3 =="Counts" ){
      par(mar=c(8,4,4,1)+.1) #bottom, left, top, right margins
      ylim<-c(0,1.2*max(Amorbs))
      xx<- barplot(Amorbs, main = "Adult Comorbidities", col="azure4",ylim=ylim, ylab= "Cases", names.arg=c(morbs$Condition), las = 2, cex.names = .7, panel.first = grid(lty= "dotted"))
      text(x = xx, y = Amorbs, label = Amorbs, pos = 3, cex = 0.8)
    }
    if(input$State3 == "Pediatric" && input$Graph3 =="Freq"){
      slices <- c(Cpercentmorbs)
      lbls <- c(morbs$Condition)
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=viridis(length(lbls)), main="Pediatric Cases")
    }
    if(input$State3 == "All" && input$Graph3 =="Freq"){
      slices <- c(percentmorbs)
      lbls <- c(morbs$Condition)
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col = viridis(length(lbls)), main="All Cases")
    }
    #FIXXXXXXXXX
    if(input$State3 == "All-Stacked" && input$Graph3 =="Freq" ){
      counts <- table(mtcars$vs, mtcars$gear)
      data_percentage <- apply(counts, 2, function(x){x*100/sum(x,na.rm=T)})
      barplot(data_percentage, main="Percentage of Pediatric and Adult Comorbidities",
              xlab="Comorbidity", names.arg=c("Heart disease", "Respiratory illness", "Liver disease"), density = c(30,5), angle=c(0,45), col="azure4", panel.first = grid(lty= "dotted"))
      legend("topright", c("Adult", "Pediatric"), fill = "azure4", density=c(30,5) , angle=c(0,45))
    }
    if(input$State3 == "Adult" && input$Graph3 =="Freq"){
      slices <- c(Apercentmorbs)
      lbls <- c(morbs$Condition)
      pct <- round(slices/sum(slices)*100)
      lbls <- paste(lbls, pct) # add percents to labels
      lbls <- paste(lbls,"%",sep="") # ad % to labels
      pie(slices,labels = lbls, col=c(viridis(length(lbls))), main="Adult Cases")
    }
  })
  
  #map for the third page
  output$map1 <- renderLeaflet({
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
          
          countyLayer.addTo(this);
        }
      ', data = read_file("our_data/US/counties.json"))
  })
  
  # To the output object, we add the map key, and assign it a leaflet map.
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
    
    # aggregated is basically taking all the observations in obs, and grouping them by state and county (some county
    # names are the same in different states, so we use both). For example, if there are 100 observations in LA, California,
    # and 50 are positive, then the aggregated table will look like this:
    # County, State, Tests, Positive, Location
    # Los Angeles, California, 100, 50, "Los Angeles, California"
    # Note that Location is basically the combination of county and state name (again because county name is not unique)
    # to generate a unique string, and then location is made the key for the data.table, basically turning it into a hash
    # map. This is important later, because we have to map number of tests and number of cases to each county for the 
    # counties dictionary, and this lets us accomplish that quickly and concisely
    aggregated <- obs[Updated < unixTime, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
    aggregated[, Location := paste0(County, ", ", State)]
    setkey(aggregated, Location)
    # pal is a function that reads in a vector of numbers, and returns a color. domain being NULL basically means
    # generate the domain from the highest and lowest value in that vector. bins = 10 means that 10 different colors
    # are created from the colorRamp, which is just a gradient from yellow to red
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
  
  #updates the graphs on page 1 based on county location, upon map click
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

