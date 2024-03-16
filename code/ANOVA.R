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

dataset = read.csv( "london_bikes_compact.csv", header = TRUE, sep = ";")
dataset$season <- factor(dataset$season, levels = c("winter", "spring", "summer", "autumn"))
dataset$weather = factor(dataset$weather, levels = c("clear", "cloudy", "rain"))

boxplot(total ~ season, data = dataset, col=c("lightblue","lightgreen","orange","yellow"), cex.axis = 0.85)

datasetWi = dataset[which(dataset$season == "winter"),]
datasetSp = dataset[which(dataset$season == "spring"),]
datasetSu = dataset[which(dataset$season == "summer"),]
datasetAu = dataset[which(dataset$season == "autumn"),]

tmp = rbind(datasetSp, datasetSu, datasetAu)

p = ggpairs(data.frame(Winter = datasetWi[,"total"]),lower = list(continuous=wrap("points", alpha = 0.5, size=0.5)))
p + theme_bw()

# ggpairs(data.frame(Spring = datasetSp[,"total"]),lower = list(continuous=wrap("points", alpha = 0.5, size=0.5)))
# ggpairs(data.frame(Summer = datasetSu[,"total"]),lower = list(continuous=wrap("points", alpha = 0.5, size=0.5)))
# ggpairs(data.frame(Autumn = datasetAu[,"total"]),lower = list(continuous=wrap("points", alpha = 0.5, size=0.5)))

tapply( dataset$total, dataset$season, function( x ) ( shapiro.test( x )$p ) )

par(mfrow=c(1,3))
qqnorm( datasetSp$total, ylab = "total in Spring", main = "Spring", pch = 16 )
qqline( datasetSp$total )
qqnorm( datasetSu$total, ylab = "total in Summer", main = "Summer", pch = 16 )
qqline( datasetSu$total )
qqnorm( datasetAu$total, ylab = "total in Autumn", main = "Autumn", pch = 16 )
qqline( datasetAu$total )

q1 = quantile(datasetSu$total, 0.25)
q2 = quantile(datasetSu$total, 0.75)
datasetSu <- datasetSu[datasetSu$total >= q1 - 1.5*(q2-q1) & datasetSu$total <= q2 + 1.5*(q2-q1),]

datasetWW = rbind(datasetSp, datasetSu, datasetAu)


b = boxcox(total ~ season, data = datasetWW)
best_lambda_ind = which.max( b$y )
best_lambda = b$x[ best_lambda_ind ]
best_lambda

datasetSp$bc = (datasetSp$total^best_lambda-1)/best_lambda
datasetSu$bc = (datasetSu$total^best_lambda-1)/best_lambda
datasetAu$bc = (datasetAu$total^best_lambda-1)/best_lambda

datasetWW = rbind(datasetSp, datasetSu, datasetAu)

tapply( (datasetWW$total^best_lambda-1)/best_lambda, datasetWW$season, function( x ) ( shapiro.test( x )$p ) )

bartlett.test( (datasetWW$total^best_lambda-1)/best_lambda, datasetWW$season )
leveneTest( (datasetWW$total^best_lambda-1)/best_lambda, datasetWW$season )

g = lm(total ~ season, datasetWW)
summary(g)
summary(aov((total^best_lambda-1)/best_lambda ~ season, datasetWW))

t.test((datasetSp$total^best_lambda-1)/best_lambda, (datasetSu$total^best_lambda-1)/best_lambda, mu=0, alternative = "two.sided", paired = FALSE, var.equal = TRUE)
t.test((datasetSu$total^best_lambda-1)/best_lambda, (datasetAu$total^best_lambda-1)/best_lambda, mu=0, alternative = "two.sided", paired = FALSE, var.equal = TRUE)
t.test((datasetAu$total^best_lambda-1)/best_lambda, (datasetSp$total^best_lambda-1)/best_lambda, mu=0, alternative = "two.sided", paired = FALSE, var.equal = TRUE)