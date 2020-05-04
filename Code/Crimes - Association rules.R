rm(list=ls())
ls() 
setwd("G:/4B/BigData/Project/crimes-in-chicago")
getwd()
#------------------------------------------------------------------------------------
library(arules)
library(arulesViz)
library(hydroTSM)
#------------------------------------------------------------------------------------
################ Read Dataset ################
DatasetName <- "Chicago_Crimes_2012_to_2017"
df <- read.csv(paste(DatasetName,".csv",sep = ""))
head(df,2L)

#------------------------------------------------------------------------------------
################ Data preprocessing ################
# Remove duplicates
df <- unique(df)

# Remove null values
df <- df[!is.na(df$Date),]
df <- df[!is.na(df$Primary.Type),]
df <- df[!is.na(df$District),]

# Remove empty values (after factorizing dataset, we found out it contains values like "", " " )
df <- df[df$District!="",]
df <- df[df$District!=" ",]
df <- df[df$Primary.Type!="",]
df <- df[df$Primary.Type!=" ",]

# Create column for month of crime[1,12] (extracted from Date column)
months <- format(as.Date(df$Date,format="%m/%d/%Y"), format = "%m")
# Create column for day of crime [Saturday, Sunday, ..] (extracted from Date column)
days <- weekdays(as.Date(df$Date))
# Create column for season of crime [Winter, Summer, ..] (extracted from Date column)
seasons <- time2season(as.Date(df$Date), out.fmt = "seasons", type="default")

#------------------------------------------------------------------------------------
################ Association rules between crimes and date-district ################

# Generate association rules between day, month number, district and crime type
# Filter rules with min-support = 0.01, min-confidence = 0.01, min-len = 2 (rule must have more or equal to 2 items)
ar <- apriori(data.frame(days, seasons, months, df$District, df$Primary.Type), parameter = list(support=0.01, confidence=0.01, minlen=2))

# get all possible right-hand-side values which have crime type=....
crimeTypeItems <- grep("^df.Primary.Type=", itemLabels(ar), value = TRUE)

## find only rules with crime type variables in the right-hand-side.
rules <- apriori(data.frame(days, seasons, months, df$District, df$Primary.Type), parameter = list(support=0.01, confidence=0.01, minlen=2), appearance = list(rhs = crimeTypeItems))

# Order generated rules by lift value
liftRules <- sort(rules, by="lift",decreasing = TRUE, na.last=NA)

# Show final generated rule
head(as(liftRules, "data.frame"),10L)