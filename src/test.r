library(readr)
library(leaflet)
library(rgdal)
library(data.table)
library(htmlwidgets)
library(profvis)
setwd("/home/lofatdairy/code/sialab/covid_vis")
obs <- fread("our_data/test/test.csv")
countyCenters <- fread("our_data/US/county_centers.csv", key = "Location")
aggregated <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
aggregated[, Location := paste0(County, ", ", State)]
setkey(aggregated, Location)
aggregated[, Markers := Tests]
aggregated$Lat <- countyCenters[aggregated$Location, Lat]
aggregated$Long <- countyCenters[aggregated$Location, Long]
counties <- readOGR("our_data/US/counties.json")
#counties$cases <- aggregated[counties$Location, counties$]
ui <- fluidPage(
  titlePanel("Laeflet JS Test"),
  mainPanel(leafletOutput(outputId = "map"))
)

server <- function(input, output, session) {
  output$map <- renderLeaflet({
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      onRender('
        function(el, x, data) {
          let geoObj = JSON.parse(data);
          let counties = geoObj.features;
          L.geoJSON(counties).addTo(this);
        }
      ', data = read_file("~/code/sialab/covid_vis/our_data/US/counties.json"))
  })
  
  observe({
    session$sendCustomMessage(type = "updateColors", input)
  })
}

shinyApp(ui, server)
profvis({
    leaflet() %>% addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      onRender('
        function(el, x, data) {
          let geoObj = JSON.parse(data);
          let counties = geoObj.features;
          L.geoJSON(counties).addTo(this);
        }
      ', data = read_file("~/code/sialab/covid_vis/our_data/US/counties.json"))
 
})

profvis({
  leaflet(counties) %>% addTiles() %>%
      addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
      addPolygons()
})
