rm(list=ls())
library(rio)
library(car)
cats1=import("6304 Module 9 Data Set.xlsx",sheet="One Way Cats")

# Equality of variances test.       

leveneTest(Eaten~Flavor,data=cats1)

# Conducting and interpreting the ANOVA -- cats1.

cats1.out=aov(Eaten~Flavor,data=cats1)
summary(cats1.out)
names(cats1.out)
cats1.out$coefficients
gilligan=TukeyHSD(cats1.out)
gilligan
plot(gilligan)

# par(mar sets the margins on the upcoming plot.
# Order of values is:  bottom, left, top, right.
# Default values are 5.1,4.1,4.1,2.1
# Set, plot, reset.

par(mar=c(5.1,8,4.1,2.1))
plot(gilligan,las=2)
par(mar=c(5.1,4.1,4.1,2.1)) 

cats2=import("6304 Module 9 Data Set.xlsx",
             sheet="Randomized Block Cats")
leveneTest(Eaten~Flavor,data=cats2)
leveneTest(Eaten~Cat,data=cats2)
cats2.out=aov(Eaten~Flavor+Cat,data=cats2)
summary(cats2.out)
ginger=TukeyHSD(cats2.out)
ginger
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(ginger,las=2,cex.axis=.6)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1)) 

# Reading in cats3 data.

cats3=import("6304 Module 9 Data Set.xlsx",
             sheet="Randomized Block Extra Cats")

# Equality of variances test on cats3.

leveneTest(Eaten~Flavor,data=cats3)
leveneTest(Eaten~Cat,data=cats3)

# Conducting the ANOVA on cats3.

cats3.out=aov(Eaten~Flavor+Cat,data=cats3)
summary(cats3.out)
maryann=TukeyHSD((cats3.out))
maryann
par(mfrow=c(1,2))
par(mar=c(5.1,6,4.1,2.1))
plot(maryann,las=2,cex.axis=.6)
boxplot(cats3$Eaten~cats3$Cat)
par(mfrow=c(1,1))
par(mar=c(5.1,4.1,4.1,2.1))

##Heart condition
hearts=import("Heart Disease.xlsx",sheet="Sheet 1")
colnames(hearts)=tolower(make.names(colnames(hearts)))
attach(hearts)
hearts$age_cat=as.factor(hearts$age_cat)
hearts$chest_pain=as.factor(hearts$chest_pain)
hearts$heart=as.factor(hearts$heart)
attach(hearts)
str(hearts)
leveneTest(max_heart_rate~heart,data=hearts)
aggregate(max_heart_rate~heart,hearts,var)
aggregate(max_heart_rate~heart,hearts,mean)
boxplot(max_heart_rate~heart,data=hearts)

leveneTest(max_heart_rate~chest_pain,data=hearts)
aggregate(max_heart_rate~chest_pain,hearts,var)
boxplot(max_heart_rate~chest_pain,data=hearts)

leveneTest(max_heart_rate~age_cat,data=hearts)
aggregate(max_heart_rate~age_cat,hearts,var)
boxplot(max_heart_rate~age_cat,data=hearts)

heart.out=aov(max_heart_rate~heart,data=hearts)
heart.out=aov(max_heart_rate~chest_pain,data=hearts)
summary(heart.out)

maryann=TukeyHSD(heart.out)
maryann
par(mar=c(5.1,6,4.1,2.1))
plot(maryann,las=2,cex.axis=.7)
par(mar=c(5.1,4.1,4.1,2.1))
plot(TukeyHSD(heart.out))
heart.out=aov(max_heart_rate~age_cat,data=hearts)
summary(heart.out)
maryann=TukeyHSD(heart.out)
maryann
par(mar=c(5.1,8,4.1,2.1))
plot(maryann,las=2,cex.axis=.7)
par(mar=c(5.1,4.1,4.1,2.1))
