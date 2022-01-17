pnorm(36,72,14)
pnorm(48,72,14)
pnorm(48,78,14)
pnorm(48,72,9)
dnorm(48,78,14)
curve(dnorm(x,72,14),from=30,to=120,lwd=3,ylim=c(0,.05))
curve(dnorm(x,78,9),from=30,to=120,lwd=3,ylim=c(0,0.05),add=TRUE,col="Red")
abline(v=48,lwd=3,col="blue")
set.seed(13875615)
my.sample=rnorm(5000,72,14)
mean(my.sample)
sd(my.sample)
hist(my.sample,col="red",main="My Little Red Histogram")
plot(density(my.sample),lwd=3,main="My 5000 Normal Random Deviates")
qqnorm(my.sample,pch=19)
qqline(my.sample,col="red",lwd=3)
library(moments)
skewness(my.sample)
kurtosis(my.sample)
my.uniforms=runif(1000,0,1)
hist(my.uniforms,col="red")
my.uniforms=runif(10000,0,1)
hist(my.uniforms,col="red")
plot(density(my.uniforms),col="red")
qqnorm(my.uniforms,pch=19)
qqline(my.uniforms,col="red",lwd=3)
skewness(my.uniforms)
kurtosis(my.uniforms)
punif(.4,0,1)
punif(0.4,0,2)
punif(0.4,0,2,lower.tail = FALSE)
curve(dunif(x,0,1),from=0,to=1,lwd=3)
curve(dunif(x,0,1),from=-0.5,to=1.5,lwd=3)
curve(dnorm(x,72,14),from=30,to=120,lwd=3,ylim=c(0,0.05))
for(i in 8:13){
  curve(dnorm(x,72,i),from=30,to=120,lwd=3,add=TRUE)
}
curve(dweibull(x,shape=3,scale = 5),from=0,to=20,lwd=3)
curve(dweibull(x,shape = 3,scale=5),from=0,to=20,lwd=3,ylim=c(0,.5))
for(i in 1:5)
{
  curve(dweibull(x,shape=i,scale=5),from=0,to=20,lwd=3,add=TRUE)
}
curve(dweibull(x,shape=3,scale=5),from=0,to=20,lwd=3,ylim=c(0,.5))
for(i in 1:5)
{
  curve(dweibull(x,shape=3,scale=i),from=0,to=20,lwd=3,add=TRUE)
}
my.data=data.frame()
for(i in 1:1000){
  my.data[i,1]=i
  my.data[i,2]=i^2
}
colnames(my.data)=c("First","Second")
my.data[750,]
head(my.data)
tail(my.data)
my.row=data.frame()
my.row[1,1]=1001
my.row[1,2]=my.row[1,1]^2
colnames(my.row)=c("First","Second")
my.data=rbind(my.data,my.row)
my.data[1001,]
tail(my.data)
plot(my.data,col="red",main="My Little Red Exponential Curve")
