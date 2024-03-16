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

n = 600
train = read.csv( "london_bikes_train.csv", header = TRUE)
test = read.csv( "london_bikes_test.csv", header = TRUE)


g = lm(total ~ temp + humidity + wind_speed + is_restday + weather, data = train)
summary(g)

train$weatherrain = ifelse(train$weather == "rain",1,0)
g = lm(total ~ temp + humidity + wind_speed + is_restday + weatherrain, data = train)
summary(g)


par(mfrow=c(1,2))
plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16)
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
shapiro.test( g$res )$p

train = read.csv( "london_bikes_train.csv", header = TRUE)
train$weatherrain = ifelse(train$weather == "rain",1,0)
g = lm(total ~ temp + humidity + wind_speed + is_restday + weatherrain, data = train)
summary(g)

lev = hatvalues( g )
watchout_ids_lev = seq_along( lev )[ which( lev > 2*(g$rank+1)/n ) ]
gs = summary(g)
res_std = g$res/gs$sigma
watchout_ids_rstd = which( abs( res_std ) > 2 )
Cdist = cooks.distance( g )
watchout_ids_Cdist = which( Cdist > 4/n ) 

train = train[-unique(c(watchout_ids_lev, watchout_ids_Cdist)),]

g = lm(total ~ temp + humidity + wind_speed + is_restday + weatherrain, data = train)
summary(g)
c(shapiro.test( g$res )$p, AIC(g), summary(g)$adj.r.squared)

par(mfrow=c(1,2))
plot( g$fit, g$res, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Fitted Values", pch = 16)
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
qqnorm( g$res, ylab = "Raw Residuals", pch = 16 )
qqline( g$res )
shapiro.test( g$res )$p

par( mfrow = c( 1, 4 ) )
plot( g$fitted.values, lev, pch = 16, xlab = 'Fitted values', 
      ylab = 'Leverages', main = 'Leverages' )
points( g$fitted.values[ watchout_ids_lev ], lev[ watchout_ids_lev ],
        col = 'red', pch = 16 )
plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', 
      ylab = 'Standardized Residuals', main = 'Standardized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], 
        col = 'red', pch = 16 )
plot( g$fitted.values, stud, pch = 16, xlab = 'Fitted values', 
      ylab = 'Studentized Residuals', main = 'Studentized Residuals' )
points( g$fitted.values[ watchout_ids_stud ], stud[ watchout_ids_stud ], 
        col = 'red', pch = 16 )
plot( g$fitted.values, Cdist, pch = 16, xlab = 'Fitted values', 
      ylab = 'Cooks Distance', main = 'Cooks Distance' )
points( g$fitted.values[ watchout_ids_Cdist ], Cdist[ watchout_ids_Cdist ], 
        col = 'red', pch = 16 )

test$weatherrain = ifelse(test$weather == "rain",1,0)
y.pred = predict( g, test, interval = "prediction", se = T, level = 0.90)

notin = !(test$total >= y.pred$fit[,2] & test$total <= y.pred$fit[,3])

par(mfrow=c(1,1))
plot(y.pred$fit[,1],test$total, xlab = "Predicted Values", ylab = "Real Values", main = "Real Values vs Predicted Values", pch = 16 )
points( y.pred$fit[,1][ notin ], test$total[ notin ], col = 'red', pch = 16 )
abline(a = 0, b = 1, col = "red")
plot( y.pred$fit[,1], y.pred$fit[,1] - test$total, xlab = "Fitted", ylab = "Residuals", main = "Residuals vs Predicted Values", pch = 16 )
points( y.pred$fit[,1][ notin ], (y.pred$fit[,1] - test$total)[ notin ], col = 'red', pch = 16 )
abline( h = 0, lwd = 2, lty = 2, col = 'red' )
qqnorm( y.pred$fit[,1] - test$total, ylab = "Raw Residuals", pch = 16 )
qqline( y.pred$fit[,1] - test$total )

shapiro.test(y.pred$fit[,1] - test$total)$p
1 - norm(y.pred$fit[,1] - test$total, type = "2")^2/norm(test$total - mean(test$total), type = "2")^2
mean(test$total >= y.pred$fit[,2] & test$total <= y.pred$fit[,3])
cor(y.pred$fit[,1],test$total)