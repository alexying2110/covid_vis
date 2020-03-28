library(data.table)
library(dplyr)

setwd("/home/lofatdairy/code/sialab/covid_vis")
startData <- fread("csse_data/csse_covid_19_data/csse_covid_19_daily_reports/03-24-2020.csv")
startData <- startData[Country_Region == "US"]

currentTime <- 1585363185178
startTime <- currentTime - 1000 * 60 * 60 * 24 * 5
  
testObs <- sample_n(startData, size = 100000, replace = T, weight = Confirmed)
testObs <- testObs[, c("Admin2", "Province_State", "Lat", "Long_")]
names(testObs) <- c("County", "State", "Lat", "Long")
testObs$Positive <- runif(100000) < .4
testObs$Updated <- sample(startTime:currentTime, 100000, replace = T)

fwrite(testObs, file = "./our_data/test/test.csv")