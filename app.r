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

# Sets directory for relative paths. one line is for use on the EC2 instance, the other is my laptop. 
# plz don't make fun of my username

#setwd("/home/ubuntu/covid_vis")
setwd("/home/lofatdairy/code/sialab/covid_vis")

# Reads in the geojson describing the county boundaries. This gives leaflet a set of points for each county that
# describes the county's border. you can think of counties as actually a dictionary with 7 key-value pairs, the 
# value of each field each being a vector. the most important fields for our use are NAME and STATENAME, 
# which are the county's name and the state's name, respectively.
# A side note: the reason that counties is declared outside the server function is that this allows for its
# value to be cached, so we don't have to read the big ass json file each time the shiny app is started.
counties <- readOGR("our_data/US/counties.json")

# pal is a function that reads in a vector of numbers, and returns a color. domain being NULL basically means
# generate the domain from the highest and lowest value in that vector. bins = 10 means that 10 different colors
# are created from the colorRamp, which is just a gradient from yellow to red
pal <- colorBin(colorRamp(c("#FFDD00","#FF0000")), domain = NULL, bins = 10)

# Note here that any additional graphs, buttons, etc must be added to ui, which just holds a fluid page object
ui <- fluidPage(
  fluidRow(
    mainPanel(
      leafletOutput(outputId = "map")
    )
  )
)

# This server function handles the data logic, and is run every time a user opens the app. Note that in R,
# returns are not required to be explicit, and in this case, the output variable is actually what is acessed
# by shiny to determine what is drawn on the UI. That means that we append any graphs, maps, figures, etc, onto
# output.
server <- function(input, output, session) {
  # obs is a data.table object (just like pandas data frames if you're familiar with that), with each
  # observation being a single test taken. each observation is the county, state, lat, long, outcome (positive or negative)
  # update time, race, hispanic, and age of the user/test.
  obs <- fread("our_data/test/test.csv")
  
  pop <- fread("our_data/US/census_pop_2019.csv")
  pop$CTYNAME[1835] <- "Dona Ana County"
  pop[, Location := paste0(unlist(strsplit(CTYNAME, " County")), ", ", STNAME)]
  setkey(pop, Location)
  
  # aggregated is basically taking all the observations in obs, and grouping them by state and county (some county
  # names are the same in different states, so we use both). For example, if there are 100 observations in LA, California,
  # and 50 are positive, then the aggregated table will look like this:
  # County, State, Tests, Positive, Location
  # Los Angeles, California, 100, 50, "Los Angeles, California"
  # Note that Location is basically the combination of county and state name (again because county name is not unique)
  # to generate a unique string, and then location is made the key for the data.table, basically turning it into a hash
  # map. This is important later, because we have to map number of tests and number of cases to each county for the 
  # counties dictionary, and this lets us accomplish that quickly and concisely
  aggregated <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
  aggregated[, Location := paste0(County, ", ", State)]
  setkey(aggregated, Location)
  
  # Remove if_else if counties are actually reported correctly.
  # This line is somewhat busy, and there's a few things going on. The general idea is this: we need a vector containing
  # the number of positive cases in each county, and the counts NEED to be in the same order as the order of the county
  # names in counties$NAMES, otherwise the counts won't be correct. The way this is done is simply taking the counties$NAME
  # and counties$STATENAME vector, and combining them item by item to form "NAME, STATENAME" strings, which match the format
  # of our keys in aggregated. Then we can go along this new vector, and treat it as a key vector for our aggregated hash table,
  # which ends up returning a data.table that contains only the rows we queried for, in the same order we queried them in. We then
  # only care about the "Positive" field, because we're getting number of cases, so we extract that column as a vector at the end.
  # The reason that there's a somewhat convoluted in_else function in there, is because NY state is aggrevating and reports all
  # cases in all 5 boroughs as "NYC", so queries like "Bronx, New York", don't have an entry in aggregated, and would be NA. So
  # we just treat all these locations as "New York City, New York", and return the number of cases there.
  counties$cases <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Positive]
  
  # This line basically takes all NA values in the counties$cases vector and converts them to 0. Counties will no cases are not
  # listed in aggregated
  counties$cases[is.na(counties$cases)] <- 0
  
  # Basically the same thing as the previous line except now we match counties to the number of tests
  counties$tests <- aggregated[if_else(counties$STATENAME == "New York" & counties$NAME %in% c("Bronx", "New York", "Kings", "Queens", "Richmond"), "New York City, New York", paste0(counties$NAME, ", ", counties$STATENAME)), Tests]
  counties$tests[is.na(counties$tests)] <- 0
  
  # Basically the same thing as the previous line except now we match counties to the number of tests
  # counties$pop <- pop[paste0(counties$NAME, ", ", counties$STATENAME), POPESTIMATE2019]
  
  # To the output object, we add the map key, and assign it a leaflet map.
  output$map <- renderLeaflet({
    #the leaflet constructor is given the geojson list
    leaflet(counties) %>%
      addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>% 
      # This generates polygons that go over the map, and color them based on the log of the number of cases. Note you will see an warning
      # that goes something like "pal got some values outside of it's range so it'll treat them as NA", but you can ignore this, because
      # it's referring to log10(0), for counties with no cases, and this exception is being handled by the outer if_else.
      addPolygons(stroke = FALSE, 
                  smoothFactor = 0.3,
                  fillOpacity = ~ifelse(cases == 0, .5, .7),
                  layerId = ~paste0(NAME, ",", STATENAME),
                  color = ~if_else(cases == 0, "#00FF00", pal(as.numeric(log10(cases)))),
                  label = ~paste0(NAME, ", ", STATENAME, ": ", cases),
                  group = "Positive"
                ) %>%
      # This adds a simple selector for which polygons you want to see, based on the group they belong to.
      addLayersControl(
        baseGroups = c("Positive", "Tested"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })
  
  # you can ignore these, i'm still working on this.
  output$logistic_curve <- renderPlot({
    id <- strsplit(input$map_shape_click, ",")
    subset <- obs[County == id[1] & State == id[2] & Positive]
    setorder(subset, Updated)
    subset[, nCases := as.numeric(row.names(subset))]
    ggplot(subset, aes(x = Updated, y = nCases)) + 
      geom_step() + 
      scale_y_continuous(name = "Number of Cases") +
      scale_x_continuous(name = "Date", labels = function(x) {as.Date(as.POSIXct(x, origin = "1970-01-01"))})
  })
  
  output$posRace <- renderPlot({
    id <- strsplit(input$map_shape_click, ",")[[1]]
    subset <- obs[County == id[1] & State == id[2] & Positive]
    breakdown <- subset[, length(Positive)/nrow(subset) * 100, by = Race]
    ggplot(breakdown, aes(y = V1, fill = Race)) + geom_bar(width = 1) + coord_polar("y", start = 0)
  })
  
  output$posAge <- renderPlot({
    id <- strsplit(input$map_shape_click, ",")[[1]]
    subset <- obs[County == id[1] & State == id[2] & Positive]
    breakdown <- subset[]
    ggplot(subset, aes(y = Race))
  })
}

shinyApp(ui, server)