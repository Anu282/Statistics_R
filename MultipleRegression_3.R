rm(list=ls())
library(rio)
library(moments)
abuse1=import("6304 Module 5 Data Sets.xlsx",
              sheet="Child Abuse with Binaries")
colnames(abuse1)=tolower(make.names(colnames(abuse1)))
## new.data <- data[ !is.na(data$V1 | data$V2) & ( data$V1 > 2 | data$V2 < 4)  , ]
attach(abuse1)
no.binary.out=lm(reported.victims~pop.under.18,data=abuse1)
summary(no.binary.out)
str(abuse1)
with.binary.out=lm(reported.victims~pop.under.18+se.state,data=abuse1)
abuse1$se.state=relevel(as.factor(se.state),ref="1")
with.binary.out1=lm(reported.victims~pop.under.18+se.state,data=abuse1)
summary(with.binary.out)
summary(with.binary.out1)
str(with.binary.out)
par(mfrow=c(1,2))
plot(abuse1$reported.victims,no.binary.out$fitted.values,
     main = "No Binary Variable",pch=19)
abline(0,1,col="red",lwd=3)
plot(abuse1$reported.victims,with.binary.out$fitted.values,
     main="With Binary Variable",pch=19)
abline(0,1,col="red",lwd=3)
par(mfrow=c(1,1))
#A better way to model a categorical variable.
abuse2=import("6304 Module 5 Data Sets.xlsx",
              sheet="Child Abuse with Binaries 2")
colnames(abuse2)=tolower(make.names(colnames(abuse2)))
attach(abuse2)
##better.binary.out1=lm(reported.victims~pop.under.18+relevel(se.state,ref="Southeastern"),data=abuse2)
better.binary.out=lm(reported.victims~pop.under.18+se.state,data=abuse2)
summary(better.binary.out)
#House Appraisals

rm(list=ls())
house=import("6304 Module 5 Data Sets.xlsx",
             sheet="House Appraisals")
colnames(house)=tolower(make.names(colnames(house)))
str(house)
house$garage=as.factor(house$garage)
house$baths=as.factor(house$baths)
attach(house)
house.out=lm(appraised.value~land.acres+house.size.sqft+
               age+rooms+baths+garage,data=house)
#OR
house.out=lm(appraised.value~.-address,data=house)
summary(house.out)
##
house.only.garage=lm(appraised.value~garage)
summary(house.only.garage)

house$garage=as.numeric(house$garage)
house.numeric.garage=lm(appraised.value~garage,data=house)
summary(house.numeric.garage)
#Linearity
plot(house$appraised.value,house.out$fitted.values,
     pch=19,main="House Data Actuals v. Fitted")
abline(0,1,lwd=3,col="red")
#Normality
par(mfrow=c(1,3))
qqnorm(house.out$residuals,pch=19,
       main="House Data, Residuals QQ Plot")
qqline(house.out$residuals,lwd=3,col="red")
hist(house.out$residuals,col="red",
     main="House Data, Residuals Histogram")
plot(density(house.out$residuals),lwd=3,
     main="House Data, Residuals Density Plot")
moments::skewness(house.out$residuals)
moments::kurtosis(house.out$residuals)
#par(mfrow=c(1,1))
par(mfrow=c(1,3))
qqnorm(house.out$residuals,pch=19,
       main="House Data, Residuals QQ Plot")
qqline(house.out$residuals,lwd=3,col="red")
hist(house.out$residuals,col="red",
     main="House Data, Residuals Histogram")
plot(density(house.out$residuals),lwd=3,
     main="House Data, Residuals Density Plot")
moments::skewness(house.out$residuals)
moments::kurtosis(house.out$residuals)
par(mfrow=c(1,1))
#Equality of Variances
plot(house.out$fitted.values,rstandard(house.out),pch=19)
abline(0,0,col="red",lwd=3)

plot(house$appraised.value,rstandard(house.out),pch=19,
     main="Cars, Standardized Residuals")
abline(0,0,col="red",lwd=3)
