library(leaflet)
library(data.table)
library(htmlwidgets)
setwd("/home/lofatdairy/code/sialab/covid_vis")
obs <- fread("our_data/test/test.csv")
countyCenters <- fread("our_data/US/county_centers.csv", key = "Location")
aggregated <- obs[, .(Tests = length(Positive), Positive = sum(Positive)), by = .(County, State)]
aggregated[, Location := paste0(County, ", ", State)]
setkey(aggregated, Location)
aggregated[, Markers := Tests]
aggregated$Lat <- countyCenters[aggregated$Location, Lat]
aggregated$Long <- countyCenters[aggregated$Location, Long]
#counties <- readOGR("our_data/US/counties.json")
#counties$cases <- aggregated[counties$Location, counties$]
leaflet() %>% addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  onRender("
    function(el, x, data) {
      for (let i = 0; i < data.Long.length; i++) {
        L.circleMarker([data.Lat[i], data.Long[i]], {radius:3}).addTo(this);
      }
    }
  ", data = aggregated)

