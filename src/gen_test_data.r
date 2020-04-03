library(data.table)
library(dplyr)

#setwd("/home/ubuntu/covid_vis")
setwd("/home/lofatdairy/code/sialab/covid_vis")
startData <- fread("csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv")
startData <- startData[Country_Region == "US"]

currentTime <- floor(as.numeric(as.POSIXct(Sys.time())))
startTime <- currentTime - 60 * 60 * 24

nObs <- 10000
  
testObs <- sample_n(startData, size = nObs, replace = T, weight = Confirmed)
testObs <- testObs[, c("Admin2", "Province_State", "Lat", "Long_")]
names(testObs) <- c("County", "State", "Lat", "Long")
testObs$Positive <- runif(nObs) < .4
testObs$Updated <- sample(startTime:currentTime, nObs, replace = T)
testObs$Race <- ifelse(runif(nObs) < .75, "White", ifelse(runif(200) < .6, "Black", "Asian"))
testObs$Hispanic <- runif(nObs) < .12
testObs$Age <- floor(rnorm(nObs, 48, 12))

fwrite(testObs, file = "./our_data/test/test.csv")