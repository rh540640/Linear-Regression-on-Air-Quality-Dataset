#Packages Deployed
load.libraries <- c('e1071','dplyr','datasets','VIM','mice')
#if not present identify and download
install.lib <- load.libraries[!load.libraries %in% installed.packages()]
sapply(install.lib, install.packages,repos = "http://cran.us.r-project.org")
#Load the packages
sapply(load.libraries, require, character = TRUE)

#Data Structure of the Dataset
class(airquality)
#Dimension of the Dataset
dim(airquality)
#Data Type of Each Variable in the Dataset
glimpse(airquality)


#This function detects Outliers by quantile capping method
outdetect <- function(c,w=1.5)
{
  h <- w*IQR(c,na.rm = T)
  q <- quantile(c,probs=c(.25, .75),na.rm = T)
  if(length(which(q[1]-h>c))==0)
    cat("There are",sum(q[1]-h>c,na.rm = T),"observations below the 1st quantile\n")
  else
    cat("There are",sum(q[1]-h>c,na.rm = T),"observations below the 1st quantile","on rows",which(q[1]-h>c),"and the values are",boxplot.stats(c)$out,"\n")
  if(length(which(q[2]+h<c))==0)
    cat("There are",sum(q[2]+h<c,na.rm = T),"observations above the 3rd quantile\n")
  else
    cat("There are",sum(q[2]+h<c,na.rm = T),"observations above the 3rd quantile","on rows",which(q[2]+h<c),"and the values are",boxplot.stats(c)$out,"\n")
}

par(mfrow=c(1,2))
boxplot(airquality$Ozone,col = "antiquewhite3",main = "Boxplot of Ozone",outcol="Blue",outpch=19,boxwex=0.7,range = 1.5)
hist(airquality$Ozone,col = "antiquewhite3",main = "Histogram of Ozone", xlab = "Observations",breaks = 15)

outdetect(airquality$Ozone)


par(mfrow=c(1,2))
boxplot(airquality$Solar.R,col = "antiquewhite3",main = "Boxplot of Solar.R",outcol="Blue",outpch=19,boxwex=0.7,range = 1.5)
hist(airquality$Solar.R,col = "antiquewhite3",main = "Histogram of Solar.R", xlab = "Observations",breaks = 15)

outdetect(airquality$Solar.R)

par(mfrow=c(1,2))
boxplot(airquality$Wind,col = "antiquewhite3",main = "Boxplot of Wind",outcol="Blue",outpch=19,boxwex=0.7,range = 1.5)
hist(airquality$Wind,col = "antiquewhite3",main = "Histogram of Wind", xlab = "Observations",breaks = 15)

outdetect(airquality$Wind)

par(mfrow=c(1,2))
boxplot(airquality$Temp,col = "antiquewhite3",main = "Boxplot of Temp",outcol="Blue",outpch=19,boxwex=0.7,range = 1.5)
hist(airquality$Temp,col = "antiquewhite3",main = "Histogram of Temp", xlab = "Observations",breaks = 15)

outdetect(airquality$Temp)

outcap <- function(x)
{
  qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
  caps <- quantile(x, probs=c(.05, .95), na.rm = T)
  H <- 1.5 * IQR(x, na.rm = T)
  sum(x > (qnt[2] + H))
  x[x < (qnt[1] - H)] <- caps[1]
  x[x > (qnt[2] + H)] <- caps[2]
  x<<-x
}

Data <- airquality
outcap(Data$Ozone)
Data$Ozone <- x
outcap(Data$Wind)
Data$Wind <- x
Data[c(9,18,48,62,117),c(1,3)]

#Summary of the Data
summary(Data[-c(5,6)])

#Names of the Variables which contains Missing Values
colnames(Data)[colSums(is.na(Data)) > 0]

PerMiss <- function(x){sum(is.na(x))/length(x)*100}

apply(Data[c(1,2)],2,PerMiss)

apply(Data,1,PerMiss)

library(mice)
md.pattern(Data)

library(VIM)
aggr_plot <-  aggr(Data[c(1,2)], col=c('antiquewhite3','antiquewhite1'), numbers=TRUE, sortVars=TRUE, labels=names(Data[c(1,2)]), cex.axis=.7, gap=3, ylab=c("Histogram of missing data","Pattern"))

marginplot(Data[c(1,2)], col=c('antiquewhite3','antiquewhite1'))

tempData <- mice(Data,m=5,maxit=50,meth='pmm',seed=500)

summary(tempData)

Data <- complete(tempData,1)

summary(airquality[c(1,2)])

summary(Data[c(1,2)])

Viz <- function(x){
  h1 <- hist(x,col="antiquewhite3",main="Histogram",xlab="Variables");
  xfit<-seq(min(x),max(x),length=40) 
  yfit<-dnorm(xfit,mean=mean(x),sd=sd(x)) 
  yfit <- yfit*diff(h1$mids[1:2])*length(x)
  lines(xfit, yfit, col="black", lwd=2)
}

Viz(Data$Ozone)

skewness(Data$Ozone)

Ozone_T <- log(Data$Ozone+1)
Viz(Ozone_T)

skewness(Ozone_T)

par(mfrow=c(1,2))
qqnorm(Data$Ozone,pch=16,main="Before Transformation",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(Data$Ozone, col = 2)
qqnorm(log(Data$Ozone+1),pch=16,main="After Transformation",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(log(Data$Ozone+1), col = 2)

shapiro.test(Ozone_T)

Viz(Data$Solar.R)

skewness(Data$Solar.R)

Solar_T <- (Data$Solar.R)^2
Viz(Solar_T)

skewness(Solar_T)

par(mfrow=c(1,2))
qqnorm(Data$Solar.R,pch=16,main="Before Transformation",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(Data$Solar.R, col = 2)
qqnorm(Solar_T,pch=16,main="After Transformation",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(Solar_T, col = 2)

shapiro.test(Solar_T)

Viz(Data$Wind)

skewness(Data$Wind)  #right positive

qqnorm(Data$Wind,pch=16,main="QQplot for Wind",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(Data$Wind, col = 2)

shapiro.test(Data$Wind)

Viz(Data$Temp)

skewness(Data$Temp)

Temp_T <- Data$Temp^2
Viz(Temp_T)

skewness(Temp_T)

par(mfrow=c(1,2))
qqnorm(Data$Temp,pch=16,main="Before Transformation",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(Data$Temp, col = 2)
qqnorm(Temp_T,pch=16,main="After Transformation",xlab="Sample quantiles of Ozone",ylab="Theoretical quantiles")
qqline(Temp_T, col = 2)

shapiro.test(Temp_T)

Data$Ozone <- Ozone_T
Data$Solar.R <- Solar_T
Data$Temp <- Temp_T

T <- Data[c(5,6)]
Data <- apply(Data[-c(5,6)],2,scale)
Data <- cbind(Data,T)
head(Data,50)

View(Data)
model=lm(Ozone~Solar.R + Wind + Temp, data=Data)

model

summary(model)
