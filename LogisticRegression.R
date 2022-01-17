rm(list=ls())
library(rio)

# Civil War Soldiers
###abuse1$se.state=relevel(as.factor(se.state),ref="1")

soldiers=import("6304 Module 8 Data Sets.xlsx",
                which="Union Soldiers",skip=2)
colnames(soldiers)=tolower(make.names(colnames(soldiers)))
attach(soldiers)
soldiers1.out=glm(any_mor~private+infantry,
                  data=soldiers,
                  family="binomial")
summary(soldiers1.out)
soldiers2.out=glm(ill_mor~private+infantry,
                  data=soldiers,
                  family="binomial")
summary(soldiers2.out)
soldiers3.out=glm(inj_mor~private+infantry,
                  data=soldiers,
                  family="binomial")
summary(soldiers3.out)
pred.data=expand.grid(private=unique(soldiers$private),
                      infantry=unique(soldiers$infantry))
any.mortality.predictions=predict(soldiers1.out,
                                  newdata=pred.data,type="response")
any.mortality.predictions=cbind(pred.data,
                                any.mortality.predictions)
any.mortality.predictions
illness.mortality.predictions=predict(soldiers2.out,
                                      newdata=pred.data,type="response")
illness.mortality.predictions=cbind(pred.data,
                                    illness.mortality.predictions)
illness.mortality.predictions
injury.mortality.predictions=predict(soldiers3.out,
                                     newdata = pred.data,type="response")
injury.mortality.predictions=cbind(pred.data,
                                   injury.mortality.predictions)
injury.mortality.predictions

# Comparing all three mortality scenarios.

any.mortality.predictions
illness.mortality.predictions
injury.mortality.predictions

# Slasher Movies

rm(list=ls())
slasher=import("6304 Module 8 Data Sets.xlsx",
               which="Slasher Movies",skip=2)
colnames(slasher)=tolower(make.names(colnames(slasher)))
attach(slasher)
slasher.out=glm(survival~female+sexual.activity,
                data=slasher,family="binomial")
summary(slasher.out)
pred.data=expand.grid(female=unique(slasher$female),
                      sexual.activity=unique(slasher$sexual.activity))
pred.data
slasher.predictions=predict(slasher.out,
                            newdata=pred.data,type="response")

slasher.predictions=cbind(pred.data,slasher.predictions)
slasher.predictions
slasher.predictions=round(predict(slasher.out,
                                  newdata=pred.data,type="response"),4)
slasher.predictions=cbind(pred.data,slasher.predictions)
slasher.predictions

# Childhood Myopia

rm(list=ls())
myopia=import("6304 Module 8 Data Sets.xlsx",
              sheet="Myopia",skip=2)
colnames(myopia)=tolower(make.names(colnames(myopia)))
attach(myopia)
# Full Myopia Model
myopia.full=glm(myopic~.-id-studyyear,
                family="binomial",data=myopia)
# Reduced Myopia Model
myopia.reduced=glm(myopic~sporthr+readhr+mommy+dadmy,
                   family="binomial",data=myopia)
summary(myopia.full)
summary(myopia.reduced)
# Myopia Predictiion on Reduced Model
pred.data=expand.grid(sporthr=quantile(sporthr,
                                       c(0,.25,.5,.75,1)),
                      readhr=quantile(readhr,
                                      c(0,.25,.5,.75,1)),
                      mommy=unique(mommy),
                      dadmy=unique(dadmy))
pred.probs=round(predict(myopia.reduced,
                         newdata=pred.data,
                         type="response"),4)
myopia.predictions=cbind(pred.data,pred.probs)
plot(myopia.predictions$pred.probs,pch=19,
     main="Myopia Probabilities")
myopia.predictions=
  myopia.predictions[order(myopia.predictions$pred.probs),]
plot(myopia.predictions$pred.probs,pch=19,
     main="Myopia Probabilities")
myopia.predictions=
  myopia.predictions[order(-myopia.predictions$pred.probs),]
plot(myopia.predictions$pred.probs,pch=19,
     main="Myopia Probabilities")
myopia.predictions=
  myopia.predictions[order(myopia.predictions$pred.probs),]
plot(myopia.predictions$pred.probs,pch=19,
     main="Myopia Probabilities")
myopia.predictions[which.max
                   (myopia.predictions$pred.probs),]
myopia.predictions[which.min
                   (myopia.predictions$pred.probs),]
# Actuals v. Fitteds
plot(myopia$myopic,myopia.reduced$fitted.values,
     pch=19,
     main="Actual Binaries & Fitted Probabilities - Myopia")
max(myopia.reduced$fitted.values)
myopia[which.max(myopia.reduced$fitted.values),]

# Stepwise Logistic Regression
myopia.step=step(glm(myopic~.-id-studyyear,family="binomial",
                     data=myopia),direction="both")
summary(myopia.step)
