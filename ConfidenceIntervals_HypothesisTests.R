rm(list=ls())
library(rio)
library(moments)
gpa50=import("Module 3 Data Sets.xlsx",sheet="GPAs 50 Stacked")
colnames=tolower(make.names(colnames(gpa50)))
attach(gpa50)
results=t.test(GPAs50,mu=2.1,alternative = c("two.sided"))
results
results90=t.test(GPAs50,mu=2.1,alternative = c("two.sided"),conf.level = 0.9)
results90
names(results)
results$p.value
results$conf.int
results$conf.int[1]
results$conf.int[2]

iq=import("Module 3 Data Sets.xlsx",sheet="IQ")
colnames(iq)=tolower(make.names(colnames(iq)))
attach(iq)
resultsiq=t.test(age.25,age.60,mu=0,alternative = c("two.sided"))
resultsiq
resultsiqvar=t.test(age.25,age.60,mu=0,alternative=c("two.sided"),var.equal = TRUE)
resultsiqvar

rats=import("Module 3 Data Sets.xlsx",sheet="Rat Pups")
colnames(rats)=tolower(make.names(colnames(rats)))
attach(rats)
resultsrats=t.test(male,female,mu=0,alternative = c("two.sided"),paired=TRUE)
resultsrats

grocery=import("Module 3 Data Sets.xlsx",sheet="Grocery")
colnames(grocery)=tolower(make.names(colnames(grocery)))
attach(grocery)
names(grocery)
str(grocery)
division=as.factor(division)
str(grocery)
levels(division)
fairview=subset(grocery,division=="Fairview")
summerfield=subset(grocery,division=="Summerfield")  
set.seed(99)
my.fairview=fairview[sample(1:nrow(fairview),18,replace=FALSE),]
attach(my.fairview)
set.seed(99)
my.summerfield=summerfield[sample(1:nrow(summerfield),15,replace = FALSE),]
attach(my.summerfield)
t.test(my.fairview$customer.penetration,mu=.2,alternative = c("greater"))
t.test(my.fairview$customer.penetration,mu=.22,alternative = c("greater"))
#H0:mu1-m2=0
#HA:mu1-mu2!=0
t.test(my.fairview$deli.sq.ft,my.summerfield$deli.sq.ft,mu=0,alternative = c("two.sided"))
#h0:mu1-mu2<=35000
#ha:mu1-mu2>35000
t.test(my.summerfield$deli.sales,my.fairview$deli.sales,mu=35000,alternative = c("greater"))


my.p.values=data.frame(matrix(ncol=2,nrow=0))
colnames(my.p.values)=c("test","p")
##iterated from sample from 2000 to 3500
for (i in 1:1600){
  results=t.test(my.fairview$deli.sq.ft,mu=2000+i,alternative = c("two.sided"))
  x=results$p.value
  my.p.values[i,1]=i+2000
  my.p.values[i,2]=x
}
plot(my.p.values,type="l",lwd=3,col="red",xlab=c("Test Value"),ylab=c("p value"),main=c("Changing values"))
abline(0.05,0,lwd=3)
abline(v=mean(my.fairview$deli.sq.ft),col="blue",lwd=3)
resultsa=t.test(my.fairview$deli.sq.ft,mu=2204,alternative="two.sided")
resultsa
boxplot(iq$age.25,iq$age.60,col="red",main="IQ Boxplot")
boxplot(iq$age.25,iq$age.60,col="red",notch=TRUE,main="IQ notched Boxplot")
boxplot(male,female,notch=TRUE,col="red",main="Rat pups Boxplot")
##
resultssd=t.test(my.fairview$deli.sq.ft,mu=2500,alternative="two.sided")
resultssd

plot(density(my.fairview$deli.sq.ft,col="blue",lwd=3))
abline(v=qnorm(0.05,mean(my.fairview$deli.sq.ft),sd(my.fairview$deli.sq.ft)),lwd=3,col="red")
abline(v=qnorm(0.95,mean(my.fairview$deli.sq.ft),sd(my.fairview$deli.sq.ft)),lwd=3,col="red")

