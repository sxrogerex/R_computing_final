#install and load packages
if (!require(car)) install.packages("car")
if (!require(zoo)) install.packages("zoo")
if (!require(lmtest)) install.packages("lmtest")
if (!require(sandwich)) install.packages("sandwich")
if (!require(ggplot2)) install.packages("ggplot2")
library(MASS) #model choosing
library(car)
library(zoo)
library(lmtest) #testing for linear regression
library(sandwich)
library(ggplot2) #visualization

#Load Data
data<-read.csv("data.csv",header = TRUE, sep = ",")

#descriptive statistics
summary(data)

#Visualize Data
#Distribution of each variables
par(mfrow=c(3,5))
hist(data$Y,col=rainbow(15),main="Distribution of Y",xlab ="Y")
hist(data$X1,col=rainbow(15),main="Distribution of X1",xlab ="X1")
hist(data$X2,col=rainbow(15),main="Distribution of X2",xlab ="X2")
hist(data$X3,col=rainbow(15),main="Distribution of X3",xlab ="X3")
hist(data$X4,col=rainbow(15),main="Distribution of X4",xlab ="X4")
hist(data$X5,col=rainbow(15),main="Distribution of X5",xlab ="X5")
hist(data$X6,col=rainbow(15),main="Distribution of X6",xlab ="X6")
hist(data$X7,col=rainbow(15),main="Distribution of X7",xlab ="X7")
hist(data$X8,col=rainbow(15),main="Distribution of X8",xlab ="X8")
hist(data$X9,col=rainbow(15),main="Distribution of X9",xlab ="X9")
hist(data$X10,col=rainbow(15),main="Distribution of X10",xlab ="X10")
hist(data$X11,col=rainbow(15),main="Distribution of X11",xlab ="X11")
hist(data$X12,col=rainbow(15),main="Distribution of X12",xlab ="X12")
hist(data$X13,col=rainbow(15),main="Distribution of X13",xlab ="X13")

#Boxplot of variables
par(mfrow=c(3,5))
boxplot(data$Y, main="Boxplot of Y")
text(y=fivenum(data$Y), labels =round(fivenum(data$Y),2),x=1.3)
boxplot(data$X1, main="Boxplot of X1")
text(y=fivenum(data$X1), labels =round(fivenum(data$X1),2),x=1.3)
boxplot(data$X2, main="Boxplot of X2")
text(y=fivenum(data$X2), labels =round(fivenum(data$X2),2),x=1.3)
boxplot(data$X3, main="Boxplot of X3")
text(y=fivenum(data$X3), labels =round(fivenum(data$X3),2),x=1.3)
boxplot(data$X4, main="Boxplot of X4")
text(y=fivenum(data$X4), labels =round(fivenum(data$X4),2),x=1.3)
boxplot(data$X5, main="Boxplot of X5")
text(y=fivenum(data$X5), labels =round(fivenum(data$X5),2),x=1.3)
boxplot(data$X6, main="Boxplot of X6")
text(y=fivenum(data$X6), labels =round(fivenum(data$X6),2),x=1.3)
boxplot(data$X7, main="Boxplot of X7")
text(y=fivenum(data$X7), labels =round(fivenum(data$X7),2),x=1.3)
boxplot(data$X8, main="Boxplot of X8")
text(y=fivenum(data$X8), labels =round(fivenum(data$X8),2),x=1.3)
boxplot(data$X9, main="Boxplot of X9")
text(y=fivenum(data$X9), labels =round(fivenum(data$X9),2),x=1.3)
boxplot(data$X10, main="Boxplot of X10")
text(y=fivenum(data$X10), labels =round(fivenum(data$X10),2),x=1.3)
boxplot(data$X11, main="Boxplot of X11")
text(y=fivenum(data$X11), labels =round(fivenum(data$X11),2),x=1.3)
boxplot(data$X12, main="Boxplot of X12")
text(y=fivenum(data$X12), labels =round(fivenum(data$X12),2),x=1.3)
boxplot(data$X13, main="Boxplot of X13")
text(y=fivenum(data$X13), labels =round(fivenum(data$X13),2),x=1.3)

#use stepwise to choose model
fit1 <- lm(Y~X1+X2+X3+X4+X5+X6+X7+X8+X9+X10+X11+X12+X13,data=data) 
fit2.1<-step(fit1,direction="both",k=2)
fit2.2<-stepAIC(fit1,direction="both",k=2)
summary(fit2.1)
summary(fit2.2)
#Both step(), stepAIC() choose the same model
# Y ~ X1 + X4 + X6 + X7 + X9 + X12 + X13

#Remove the variable of highest ™¤pvalue: X12, because some variable's p-value is not significant
fit3<-lm(Y ~ X1 + X4 + X6 + X7 + X9 + X13,data=data)  
summary(fit3)
#Remove the variable of highest ™¤pvalue: X13, because some variable's p-value is not significant
fit3<-lm(Y ~ X1 + X4 + X6 + X7 + X9,data=data)  
summary(fit4)
#Remove the variable of highest ™¤pvalue: X6, because some variable's p-value is not significant
fit3<-lm(Y ~ X1 + X4 + X7 + X9,data=data)  
summary(fit4)
#All variables' p-value are significant

#Scatter Diagram
data_select<-subset(data,select=c(X1,X4,X7,X9))
plot(data_select,col=c("orange","blue"))

#Correlation matrix
correlation.matrix<-cor(data[,c("X1","X4","X7","X9")])
correlation.matrix

#Variance Inflation Factor(VIF)
VIF <- sqrt(vif(fit3))
VIF

#Autocorrelation: Durbin-Watson Test
durbinWatsonTest(fit3)

#Breusch-Pagan test
bptest(fit3, data=data)
plot(fit3)
coeftest(fit3)
coeftest(fit3, vcovHC)

#Table of Actual Value & Predict Value & Residuals
t <- data.frame(ActualValue = data$Y, PredictValue = predict(fit3),residuals=residuals(fit3))
View(t)

#MSE
MSE=mean((data$Y - predict(fit3,data,type='response'))^2)
MSE

#Plot of Actual Value & Predict Value
plot(predict(fit3),data$Y,
     xlab="predicted",ylab="actual")
abline(a=0,b=1,col="red")