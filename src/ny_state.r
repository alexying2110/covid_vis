library(shiny)
library(leaflet)
library(leaflet.extras)
library(rgdal)
library(dplyr)
library(DT)
library(data.table)
library(ggplot2)
library(rsconnect)

setwd("~/code/sialab/covid_vis")
counties <- readOGR("our_data/US/counties.json")
data <- fread("csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv")

counties <- as.character(nyCounties$county)

cases <- read.csv(file = "our_data/ny_state/cases.tsv", header = T)

projPop <- read.csv(file = "our_data/ny_state/albany-county.csv")

for (county in counties) {
  countyFile <- county %>% gsub(" ", "-", .) %>% gsub("\\.", "", .) %>% tolower()
  countySuffix <- countyFile %>% substr(., 0, nchar(countyFile) - 7) %>% paste0(".", .)
  countyPop <- read.csv(paste0("our_data/ny_state/", countyFile, ".csv"))
  countyPop$Male <- -countyPop$Male
  countyPop[county] <- countyPop$Male + countyPop$Female
  projPop <- full_join(projPop, countyPop, by="Category", suffix=c("", countySuffix))
}

projPop$Male <- NULL
projPop$Female <- NULL
row.names(projPop) <- projPop$Category
projPop$Category <- NULL

cases$County <- paste(cases, "County")

casesMap <- cases$Positive.Cases[match(nyCounties$county, cases$County)]
casesMap[is.na(casesMap)] <- 0

nyCounties$cases <- casesMap

nyCounties$pop <- sapply(nyCounties$county, function(x) {sum(projPop[x])})

print(data.frame(county = nyCounties$county, pop = nyCounties$pop))

pal <- colorNumeric("viridis", NULL)

leaflet(nyCounties) %>%
  addTiles() %>%
  addProviderTiles(providers$CartoDB.DarkMatterNoLabels) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(sqrt(cases)),
              label = ~paste0(county, ": ", formatC(cases, big.mark = ",")),
              group = "cases"
              ) %>%
  addPolygons(stroke = FALSE, smoothFactor = 0.3, fillOpacity = 1,
              fillColor = ~pal(pop),
              label = ~paste0(county, ": ", formatC(pop, big.mark = ",")),
              group = "population"
              ) %>%
  addLegend(title = "Cases", pal = pal, values = ~sqrt(cases), opacity = 1.0,
            labFormat = labelFormat(transform = function(x) x^2)) %>%
  addLayersControl(
    baseGroups = c("cases", "population"),
    options = layersControlOptions(collapsed = FALSE)
  )

