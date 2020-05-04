
# Import libraries
require(forecast)
require(TSPred)
library(readr)
library(tidyr)
library(Metrics)

# Clearing environment and setting working directory
rm(list=ls())
setwd("E:\\Projects\\R\\Crimes in Chicago")

# ---------------------------------------------------------------------------------------

# Preprocessing 
# -------------

# Loading dataset and removing duplicate records
crimes_05to07 = read_csv("Chicago_Crimes_2005_to_2007.csv")
crimes_05to07 = as.data.frame(crimes_05to07)
crimes_05to07 <- unique(crimes_05to07)

crimes_08to11 = read_csv("Chicago_Crimes_2008_to_2011.csv")
crimes_08to11 = as.data.frame(crimes_08to11)
crimes_08to11 <- unique(crimes_08to11)

crimes_12to17 = read_csv("Chicago_Crimes_2012_to_2017.csv")
crimes_12to17 = as.data.frame(crimes_12to17)
crimes_12to17 <- unique(crimes_12to17)

# Loading all data into one dataframe
crimes <- rbind(crimes_05to07, crimes_08to11, crimes_12to17)

# Keeping effective parameters only
keep <- c( "Date", "Primary Type", "Description", "Location Description")
crimesdata <- crimes[keep]

# Removing NULL records
crimesdata <- crimesdata[!is.na(crimesdata$Date),]

# Reformatting time to match time series required format
crimesdata <- separate(data = crimesdata, col = Date, into = c("Date", "Time", "A/P M"), sep = " ")
crimesdata$Date <- as.Date(crimesdata$Date, format="%m/%d/%Y")

# Dividing the dataset into train-test sets
testset <- crimesdata[crimesdata$Date >= "2016-01-01" & crimesdata$Date < "2017-01-01",]
trainset <- crimesdata[crimesdata$Date < "2016-01-01",]

# ---------------------------------------------------------------------------------------

# Time Series Model 
# -----------------

# Creating Time Series model
tab <- table(cut(trainset$Date, 'month'))
instances <- data.frame(Date=format(as.Date(names(tab)), '%m/%Y'), Frequency=as.vector(tab))
data <- ts(instances[,2], start = c(2005,1), frequency = 12)

# Plotting the time series
par(mfrow=c(1,1))
plot(data, xlab="Years", ylab = "Crimes")

plot(diff(data), xlab="Years", ylab = "Crimes")

ARIMAfit <- auto.arima(log10(data), approximation=FALSE, trace=FALSE)

# Predicting the next 6 months
pred <- predict(ARIMAfit, n.ahead = 12)

# Plotting the time series and the prediction
plot((data), xlim=c(2005,2017), xlab = "Year",ylab = "Number of Crimes")
lines(10^(pred$pred),col="blue")
lines(10^(pred$pred+2*pred$se),col="orange")
lines(10^(pred$pred-2*pred$se),col="orange")

testtab <- table(cut(testset$Date, 'month'))
testinstances <- data.frame(Date=format(as.Date(names(testtab)), '%m/%Y'), Frequency=as.vector(testtab))
testdata <- ts(testinstances[,2], start = c(2016,1), frequency = 12)
lines(testdata, col="red")

# Model Accuracy
predi = 10^(pred$pred)
forecast::accuracy(testdata, predi) # MSE 2211705

plotarimapred(log10(data), ARIMAfit, xlim=c(2005,2018), range.percent = 0.2)


# Creating time series for each crime type and exporting the plots into jpeg format
types = as.vector(unique(crimesdata$`Primary Type`))

for (crimetype in types) {
  custom = crimesdata[crimesdata$`Primary Type` == crimetype,]
  tab <- table(cut(custom$Date, 'month'))
  instances <- data.frame(Date=format(as.Date(names(tab)), '%m/%Y'), Frequency=as.vector(tab))
  data <- ts(instances[,2], start = c(2005,1), frequency = 12)
  name = paste(crimetype,'.jpg', sep="")
  jpeg(name)
  par(mfrow=c(1,1))
  plot(data, main = crimetype,font.main=9,cex.main=2, xlab="Years", ylab = crimetype)
  dev.off()
}


# Creating time series for each location and exporting the plots into jpeg format
locationsDes = as.vector(tail(names(sort(table(crimesdata$`Location Description`))), 10))

i=0
for (loc in locationsDes) {
  custom = crimesdata[crimesdata$`Location Description` == loc,]
  tab <- table(cut(custom$Date, 'month'))
  instances <- data.frame(Date=format(as.Date(names(tab)), '%m/%Y'), Frequency=as.vector(tab))
  data <- ts(instances[,2], start = c(2005,1), frequency = 12)
  name = paste(as.character(i),'.jpg', sep="")
  i = i+1
  jpeg(name)
  par(mfrow=c(1,1))
  plot(data, main = loc,font.main=9,cex.main=2, xlab="Years", ylab = loc)
  dev.off()
}