rm(list=ls())
library(rio)
library(moments)
cars=import("6304 Multiple Regression Assgt Example Data.xlsx",sheet="6304 Old Auto MPG")
colnames(cars)=tolower(make.names(colnames(cars)))
cars$year=as.factor(cars$year)
cars$cylinders=as.factor(cars$cylinder)
attach(cars)

reg.out=lm(mpg~cubic.inches+horsepower+weight,data=cars)
summary(reg.out)
confint(reg.out)
##Line Assumption
##Linearity
plot(cars$mpg,reg.out$fitted.values,pch=19,
     main="Cars,Actual vs Fitted")
abline(0,1,col="red",lwd=3)
##Normality
qqnorm(reg.out$residuals,pch=19,
       main="Cars,Normality of Residuals")
qqline(reg.out$residuals,lwd=3,col="red")
hist(reg.out$residuals,col="red",
     main="Cars, Histogram of Residuals")
plot(density(reg.out$residuals),lwd=3,
     main="Cars, Density Plot of Residuals")
skewness(reg.out$residuals)
kurtosis(reg.out$residuals)
##Equality of Variances
plot(cars$mpg,rstandard(reg.out),pch=19,
     main="Cars, Standardized Residuals")
abline(0,0,col="red",lwd=3)
##Or
plot(rstandard(reg.out),
     main="Cars, Standardized Residuals",
     pch=19)
##Or
plot(reg.out$fitted.values,rstandard(reg.out),pch=19)
abline(0,0,col="red",lwd=3)
##or
plot(reg.out$fitted.values,reg.out$residuals,pch=19,
     main="Residuals plot")
abline(0,0,col="blue",lwd=3)
##high leverage points
leverage=hat(model.matrix(reg.out))
plot(leverage,pch=19,ylim=c(0,.5))
abline(2*mean(leverage),0,col="blue",lwd=3)
cars[leverage>3*mean(leverage),c(6,8,9)]
##Introducing square terms to the model
reg.out2=lm(mpg~cubic.inches+horsepower+weight+
              I(horsepower^2)+I(weight^2))
summary(reg.out2)

par(mfrow=c(1,2))
plot(cars$mpg,reg.out$fitted.values,
     pch=19,main="Original Model")
abline(0,1,col="red",lwd=3)
plot(cars$mpg,reg.out2$fitted.values,
     pch=19,main="Squared Terms Model")
abline(0,1,col="red",lwd=3)
par(mfrow=c(1,1))

##
reg.out3=lm(mpg~cubic.inches+horsepower+weight+
              cylinders,data=cars)
summary(reg.out3)
par(mfrow=c(1,2))
plot(cars$mpg,reg.out$fitted.values,
     pch=19,main="Original Model")
abline(0,1,col="red",lwd=3)
plot(cars$mpg,reg.out3$fitted.values,
     pch=19,main="Squared Terms Model")
abline(0,1,col="red",lwd=3)
par(mfrow=c(1,1))

##
reg.out4=lm(mpg~cubic.inches+horsepower+weight+
              year,data=cars)
summary(reg.out4)
par(mfrow=c(1,2))
plot(cars$mpg,reg.out$fitted.values,
     pch=19,main="Original Model")
abline(0,1,col="red",lwd=3)
plot(cars$mpg,reg.out4$fitted.values,
     pch=19,main="Squared Terms Model")
abline(0,1,col="red",lwd=3)
par(mfrow=c(1,1))

