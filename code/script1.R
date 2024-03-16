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

dataset = read.csv( "london_bikes_compact.csv", header = TRUE, sep = ";")
dataset$season <- factor(dataset$season, levels = c("winter", "spring", "summer", "autumn"))
dataset$weather = factor(dataset$weather, levels = c("clear", "cloudy", "rain"))

summary(dataset)

ggpairs(dataset[, c("temp", "feeltemp", "humidity", "wind_speed", "total")],lower = list(continuous=wrap("points", alpha = 0.5, size=0.5)))

data(mtcars)
my_ggpairs <- ggpairs(dataset[, c("temp", "feeltemp", "humidity", "wind_speed", "total")],lower = list(continuous=wrap("points", alpha = 0.5, size=1)), aes(background = "F8CBAD"))

my_ggpairs <- my_ggpairs + 
  scale_color_manual(values = "black")
my_ggpairs <- my_ggpairs + 
  scale_fill_manual(values = "F8CBAD")

layout(matrix(c(1,1,1,2,2,2,3,3,4,4,5,5),nrow=2,ncol = 6, byrow = TRUE))
barplot(table(dataset$season), names.arg = c("winter", "spring", "summer", "autumn"), col = c("lightblue","lightgreen","orange","yellow"), xlab = "Season")
barplot(table(dataset$weather), names.arg = c("clear", "cloudy", "rain"), col = c("lightblue","darkgray","purple"), xlab = "Weather")
barplot(table(dataset$is_holiday), col = c("darkred", "darkgreen"), xlab = "is_holiday")
barplot(table(dataset$is_weekend), col = c("darkred", "darkgreen"), xlab = "is_weekend")
barplot(table(dataset$is_restday), col = c("darkred", "darkgreen"), xlab = "is_restday")

par(mfrow=c(1,2))
boxplot(total ~ season, data = dataset, col=c("lightblue","lightgreen","orange","yellow"), cex.axis = 0.85)
boxplot(total ~ weather, data = dataset, col=c("lightblue","darkgray","purple"))


g = lm(total ~ temp + humidity + wind_speed + is_restday + weather, data = dataset)
summary(g)


g = lm(total ~ temp + humidity + wind_speed + is_restday + weatherrain, data = dataset)
summary(g)

dataset$res = g$res

g = lm(total ~ temp + humidity + wind_speed +  + is_workday, data = dataset)
summary(g)

plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )

qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )

shapiro.test( g$res )$p