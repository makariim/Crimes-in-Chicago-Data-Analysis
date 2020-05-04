rm(list=ls())
ls() 
setwd("D:/UNI/BD/Project")
getwd()
#------------------------------------------------------------------------------------
library(ggplot2)
library(data.table) 
library(gtools)
#------------------------------------------------------------------------------------
#load Dataset and remove duplicates in batches (my computer crashed when i tried to do it all in one go)
crimes_01to04 = read_csv("Chicago_Crimes_2001_to_2004.csv")
crimes_01to04 = as.data.frame(crimes_01to04)
crimes_01to04<-crimes_01to04[,c("ID","Date","Primary Type","Arrest","Domestic","District")]
crimes_01to04 <- unique(crimes_01to04)

crimes_05to07 = read_csv("Chicago_Crimes_2005_to_2007.csv")
crimes_05to07 = as.data.frame(crimes_05to07)
crimes_05to07<-crimes_05to07[,c("ID","Date","Primary Type","Arrest","Domestic","District")]
crimes_05to07 <- unique(crimes_05to07)

crimes_08to11 = read_csv("Chicago_Crimes_2008_to_2011.csv")
crimes_08to11 = as.data.frame(crimes_08to11)
crimes_08to11<-crimes_08to11[,c("ID","Date","Primary Type","Arrest","Domestic","District")]
crimes_08to11 <- unique(crimes_08to11)

crimes_12to17 = read_csv("Chicago_Crimes_2012_to_2017.csv")
crimes_12to17 = as.data.frame(crimes_12to17)
crimes_12to17<-crimes_12to17[,c("ID","Date","Primary Type","Arrest","Domestic","District")]
crimes_12to17 <- unique(crimes_12to17)
#concatenate all files
df <- rbind(crimes_01to04,crimes_05to07, crimes_08to11, crimes_12to17)
head(df,2L)
#------------------------------------------------------------------------------------
# Preparing Data
df <- df[!is.na(df$Date),]
df <- df[!is.na(df$ID),]
df <- df[!is.na(df$`Primary Type`),]
df <- df[!is.na(df$Arrest),]
df <- df[!is.na(df$District),]
df <- df[!is.na(df$Domestic),]
df <- df[df$District!="",]
df <- df[df$District!=" ",]
df <- df[df$`Primary Type`!="",]
df <- df[df$`Primary Type`!=" ",]
df <- df[df$Date!="",]
df <- df[df$Date!=" ",]
#formating the date and  extracting months for a plot.
df <- separate(data = df, col = Date, into = c("Date", "Time", "A/P M"), sep = " ")
df$Date <- as.Date(df$Date, format="%m/%d/%Y")
df$months <- format(as.Date(df$Date,format="%m/%d/%Y"), format = "%m")
#remove unneeded columns
df<-within(df, rm("Time","ID","Date"))
#------------------------------------------------------------------------------------
#Now PLOT!

#arrange factor levels according to frequency so plot looks nice
df$`Primary Type`<- factor(df$`Primary Type`, levels=names(sort(table(df$`Primary Type`))))
#plot frequency of primary type
ggplot(df) +
  aes(`Primary Type`) +
  geom_bar() +
  scale_fill_hue() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot frequency of crimes in each Distric 
df$District<- factor(df$District, levels=names(sort(table(df$District))))
ggplot(df) +
  aes(District) +
  geom_bar() +
  scale_fill_hue() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot district with crime types
ggplot(df) +
  aes(x = District, fill = `Primary Type`) +
  geom_bar() +
  scale_fill_hue() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot months with crime types
ggplot(df) +
  aes(x = months, fill = `Primary Type`) +
  geom_bar() +
  scale_fill_hue() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

