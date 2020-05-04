rm(list=ls())
ls() 
setwd("D:/UNI/BD/Project")
getwd()
#------------------------------------------------------------------------------------
library(tidyverse)
library(hydroTSM)
library("lsr")
library(ggplot2)
library(data.table) 
library(gtools)
library(plotly)
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
df_corr <- rbind(crimes_01to04,crimes_05to07, crimes_08to11, crimes_12to17)
head(df_corr,2L)
#------------------------------------------------------------------------------------
# Preparing Data
df_corr <- df_corr[!is.na(df_corr$Date),]
df_corr <- df_corr[!is.na(df_corr$ID),]
df_corr <- df_corr[!is.na(df_corr$`Primary Type`),]
df_corr <- df_corr[!is.na(df_corr$Arrest),]
df_corr <- df_corr[!is.na(df_corr$District),]
df_corr <- df_corr[!is.na(df_corr$Domestic),]
df_corr <- df_corr[df_corr$District!="",]
df_corr <- df_corr[df_corr$District!=" ",]
df_corr <- df_corr[df_corr$`Primary Type`!="",]
df_corr <- df_corr[df_corr$`Primary Type`!=" ",]
df_corr <- df_corr[df_corr$Date!="",]
df_corr <- df_corr[df_corr$Date!=" ",]
#formating the date and seperating it into AM/PM, months ,days and seasons.
df_corr <- separate(data = df_corr, col = Date, into = c("Date", "Time", "A/P M"), sep = " ")
df_corr$Date <- as.Date(df_corr$Date, format="%m/%d/%Y")
df_corr$months <- format(as.Date(df_corr$Date,format="%m/%d/%Y"), format = "%m")
df_corr$days <- weekdays(as.Date(df_corr$Date))
df_corr$seasons <- time2season(as.Date(df_corr$Date), out.fmt = "seasons", type="default")
#remove unneeded columns
df_corr<-within(df_corr, rm("Time","ID","Date"))
#------------------------------------------------------------------------------------
#applying Crammer's V on all variable pairs
vars<-colnames(df_corr)
Crammer_all <- function(vars, df){
  # generate all variable pairs (a variable with itself is not included)
  vars_grid <- data.table(combinations(n = length(vars), r = 2, v = vars, repeats.allowed = FALSE))
  do.call(rbind,
          apply(vars_grid, 1, function(x){
            #for each variable pair perform the Chi squared test and print the percentage of expected values that are less than 5
            tmp <- as.character(x)
            vec1 <- unlist(df[,tmp[1]])
            vec2 <- unlist(df[,tmp[2]])
            chi = chisq.test(vec1,vec2)
            data.table(
              variable_x = tmp[1],
              variable_y = tmp[2],
              chi2 = chi$p.value,
              less_five=(sum(chi$expected<5)/length(chi$expected))*100,
              v_cramer = cramersV(vec1,vec2)
            )
          }))
  
}
results <- Crammer_all(vars = vars, df = df_corr)

#plot the "heat map" for the correlations
ggplotly(ggplot(results, aes(variable_x, variable_y)) +
           geom_tile(aes(fill = v_cramer), colour = "black") +
           theme(axis.text.x=element_text(angle=45, hjust=1)) +
           scale_fill_gradient(low = "white", high = "red") +
           theme_bw() + xlab(NULL) + ylab(NULL) +
           theme(axis.text.x=element_text(angle = -90, hjust = 0)) +
           ggtitle("Cramer's V heatmap"))


#Studying relations with high correlation value
#check expected table to see if test failed or not 
#Chi-Square Test Assumptions:
#For larger table, all expected frequencies > 1 and no more than 20% of all cells may have expected frequencies < 5.
#See results variable and check that percentage. Both coming plots pass ^_^

#plot Primary type with Arrests
ggplot(df_corr) +
  aes(fill = Arrest, x = `Primary Type`) +
  geom_bar() +
  scale_fill_hue() + theme(axis.text.x = element_text(angle = 90, hjust = 1))

#plot Primary type with Domestic
ggplot(df_corr) +
  aes(fill = Domestic, x = `Primary Type`) +
  geom_bar() +
  scale_fill_hue() + theme(axis.text.x = element_text(angle = 90, hjust = 1))






