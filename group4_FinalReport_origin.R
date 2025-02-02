#安�?�packages
if (!require(car)) install.packages("car")
if (!require(zoo)) install.packages("zoo")
if (!require(lmtest)) install.packages("lmtest")
if (!require(sandwich)) install.packages("sandwich")
library(MASS)
library(car)
library(zoo)
library(lmtest)
library(sandwich)
#?��?��資�??
data<-read.csv("data.csv",header = TRUE, sep = ",")
#??�述統�?��??
summary(data)
#stepwise
fit1<-lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13,data=data) 
fit2<-step(fit1,direction="both",k=2)
summary(fit2)
#?��?��pvalue??�大�?��?�數X12
fit3<-lm(Y~X1+X4+X6+X7+X9+X13,data=data)  
summary(fit3)
#?��變異?��?��步�??
data_select<-subset(data,select=c(X1,X4,X6,X7,X9,X13))
plot(data_select,col=c("orange","blue"))
#?��變異係數?��?��
correlation.matrix<-cor(data[,c("X1","X4","X6","X7","X9","X13")])
#VIF?��線性檢�?
VIF <- sqrt(vif(fit3))
VIF
#?��??�相??�檢�?
durbinWatsonTest(fit3)
#?��質性檢�?
bptest(fit3, data=data)
plot(fit3)
coeftest(fit3)
coeftest(fit3, vcovHC)