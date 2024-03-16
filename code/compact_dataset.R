rm(list=ls())
graphics.off()
cat("\014")

library( car )
library( ellipse )
library( faraway )
library( leaps )
library(MASS)
library( GGally)
library(BAS)
library(rgl)
library(corrplot)
library(caTools)

calcola_moda <- function(vettore) {
  tabella_frequenze <- table(vettore)
  moda <- as.numeric(names(tabella_frequenze)[tabella_frequenze == max(tabella_frequenze)])
  return(moda)
}

dataset = read.csv( "london_bikes.csv", header = TRUE, sep = ";")
compact = NULL

for (i in 1:730) {
  subDS = NULL
  subDS = dataset[which(dataset$dayn == i),]
  
  new_row <- data.frame(
    dayn = i,
    date = subDS$date[1],
    cnt = sum(subDS$cnt),
    t1 = mean(subDS$t1),
    t2 = mean(subDS$t2),
    hum = mean(subDS$hum),
    wind_speed = mean(subDS$wind_speed),
    weather_code = max(calcola_moda(subDS$weather_code))
  )
  
  compact = rbind(compact, new_row)
}
write.csv(compact, file = "london_bikes_compact.csv", row.names = FALSE)

sample <- sample.split(compact$date, SplitRatio = 0.8242)
train <- subset(compact, sample == TRUE)
test <- subset(compact, sample == FALSE)

write.csv(train, file = "london_bikes_train.csv", row.names = FALSE)
write.csv(test, file = "london_bikes_test.csv", row.names = FALSE)