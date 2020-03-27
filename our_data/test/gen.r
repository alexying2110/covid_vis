library(data.table)
nObs = 10000000
test <- data.table(longitude = sample(70000:122000, nObs, replace = T) / 1000, latitute = sample(45000:30000, nObs, replace = T) / 1000, county = rep("test", nObs), state = sample(state.abb, nObs, replace = T), result = sample(c(T,F), nObs, replace = T))

fwrite(test, file="./test.csv")
