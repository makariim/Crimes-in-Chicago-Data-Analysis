#clean environment and set working directory
rm(list=ls())
setwd("F:\\CUFE\\4\\\\B\\Big Data\\Project")

# import libraries (not all of these were actually used)
library(rattle.data)
library(NbClust)
library(cluster)
library(HSAUR)
library(ggplot2)
library(ggiraph)
library(vegan)
library(ggmap)
library(ggthemes)
library(RColorBrewer)
library(maps)
library(sp)
library(maptools)

# **********************************************************************#
# *********************** Data Preprocessing ***************************#
# **********************************************************************#
#import the data and view structure
dfm <- read.csv("Chicago_Crimes_2012_to_2017.csv", header = TRUE)
str(dfm)

#remove duplicate rows
dfm1 <- dfm[!duplicated(dfm[,c("ID","Case.Number")]),]

#extract the geographical features
dfm1 <- data.frame(dfm1$ID, dfm1$Longitude, dfm1$Latitude, dfm1$Primary.Type)
str(dfm1)

#remove rows with empty and null values
dfm1 <- dfm1[!(dfm1$dfm1.Longitude=="" | dfm1$dfm1.Latitude=="" | is.na(dfm1$dfm1.ID) | is.na(dfm1$dfm1.Longitude) | is.na(dfm1$dfm1.Latitude)), ]

#remove invalid data (outside Chicago!)
dfm1 <- dfm1[dfm1$dfm1.Longitude > -88,]

#Longitude and Latitude histogram
hist(dfm1$dfm1.Longitude, col = "blue3", border = "blue3", xlim = c(-87.5,-88), breaks = 10000, main = "Longitude Histogram", xlab = 'Longitude')
hist(dfm1$dfm1.Latitude, col = "blue3", border = "blue3", xlim = c(41.6, 42.15), breaks = 10000, main = "Latitude Histogram", xlab = 'Latitude')

# **********************************************************************#
# setting up the map
map_bounds <- c(left = -87.9, bottom = 41.6, right = -87.5, top = 42.05)
chicago_map <- get_stamenmap(map_bounds, zoom = 10, maptype = "toner-lite")

# **********************************************************************#
# **************************** Clustering ******************************#
# **********************************************************************#
# Extracting seperate crime types data
table(dfm1$dfm1.Primary.Type)

dfm_homicide <- dfm1[dfm1$dfm1.Primary.Type == "HOMICIDE",]
dfm_robbery <- dfm1[dfm1$dfm1.Primary.Type == "ROBBERY",]
dfm_sexual_assault <- dfm1[dfm1$dfm1.Primary.Type == "CRIM SEXUAL ASSAULT",]

# **********************************************************************#
#determine the optimal number of clusters for all crimes
dfm_clus <- data.frame(dfm1$dfm1.Longitude, dfm1$dfm1.Latitude)
wss <- (nrow(dfm_clus)-1)*sum(apply(dfm_clus,2,var))
for (i in 2:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(dfm_clus, centers=i, 100)$withinss)
}
plot(1:15, wss, type="s", col = "blue3", main = "All crimes", xlab="Number of Clusters", ylab="WithinSS")

#apply kmeans clustering with 3 clusters
set.seed(1234)
km_all <- kmeans(dfm_clus, 3)
km_all$centers
#save cluster for each point
dfm_clus$cluster <- km_all$cluster

# sampling to draw scatter plot
write.csv(dfm_clus,file="data.txt",row.names=F,quote=F)
set.seed(1234)
dfm_sampled <- sample.df("data.txt",n=1000,p=0.1)

# plot clusters on Chicago map
map <- ggmap(chicago_map, extent="device", legend="none") +
  geom_point(data = dfm_sampled, mapping = aes(x = dfm1.dfm1.Longitude, y = dfm1.dfm1.Latitude, colour = cluster),  alpha = 0.05, show.legend = FALSE) +
  ggtitle("All crimes") + theme(plot.title = element_text(hjust = 0.5))
#map
ggsave("./final_all_clusters.png")
#*********************************************************************#
#determine the optimal number of clusters for homicide crimes
dfm_homicide_clus <- data.frame(dfm_homicide$dfm1.Longitude, dfm_homicide$dfm1.Latitude)
wss <- (nrow(dfm_homicide_clus)-1)*sum(apply(dfm_homicide_clus,2,var))
for (i in 2:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(dfm_homicide_clus, centers=i, 100)$withinss)
}
plot(1:15, wss, type="s", col = "blue3", main = "Homicide", xlab="Number of Clusters", ylab="WithinSS")

#apply kmeans clustering with 2 clusters
set.seed(1234)
km_homicide <- kmeans(dfm_homicide_clus, 2)
km_homicide$centers

# plot clusters on Chicago map
map <- ggmap(chicago_map, extent="device", legend="none") +
  geom_point(data = dfm_homicide, mapping = aes(x = dfm1.Longitude, y = dfm1.Latitude, colour = km_homicide$cluster),  alpha = 0.2, show.legend = FALSE) + 
  ggtitle("Homicide") + theme(plot.title = element_text(hjust = 0.5))
#map
ggsave("./final_homicide_clusters.png")
#*********************************************************************#
#determine the optimal number of clusters for robbery crimes
dfm_robbery_clus <- data.frame(dfm_robbery$dfm1.Longitude, dfm_robbery$dfm1.Latitude)
wss <- (nrow(dfm_robbery_clus)-1)*sum(apply(dfm_robbery_clus,2,var))
for (i in 2:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(dfm_robbery_clus, centers=i, 100)$withinss)
}
plot(1:15, wss, type="s", col = "blue3", main = "Robbery", xlab="Number of Clusters", ylab="WithinSS")

#apply kmeans clustering with 2 clusters
set.seed(1234)
km_robbery <- kmeans(dfm_robbery_clus, 2)
km_robbery$centers

# plot clusters on Chicago map
map <- ggmap(chicago_map, extent="device", legend="none") +
  geom_point(data = dfm_robbery, mapping = aes(x = dfm1.Longitude, y = dfm1.Latitude, colour = km_robbery$cluster),  alpha = 0.05, show.legend = FALSE) +
  ggtitle("Robbery") + theme(plot.title = element_text(hjust = 0.5))
#map
ggsave("./final_robbery_clusters.png")
#*********************************************************************#
#determine the optimal number of clusters for sexual assault crimes
dfm_sexual_assault_clus <- data.frame(dfm_sexual_assault$dfm1.Longitude, dfm_sexual_assault$dfm1.Latitude)
wss <- (nrow(dfm_sexual_assault_clus)-1)*sum(apply(dfm_sexual_assault_clus,2,var))
for (i in 2:15) {
  set.seed(1234)
  wss[i] <- sum(kmeans(dfm_sexual_assault_clus, centers=i, 100)$withinss)
}
plot(1:15, wss, type="s", col = "blue3", main = "Sexual Assault", xlab="Number of Clusters", ylab="WithinSS")

#apply kmeans clustering with 2 clusters
set.seed(1234)
km_assault <- kmeans(dfm_sexual_assault_clus, 2)
km_assault$centers

# plot clusters on Chicago map
map <- ggmap(chicago_map, extent="device", legend="none") +
  geom_point(data = dfm_sexual_assault, mapping = aes(x = dfm1.Longitude, y = dfm1.Latitude, colour = km_assault$cluster),  alpha = 0.25, show.legend = FALSE) +
  ggtitle("Sexual assault") + theme(plot.title = element_text(hjust = 0.5))
#map
ggsave("./final_assault_clusters.png")
#*********************************************************************#
# plot cluster centers of all crimes on Chicago map with community areas
# size based on number of points in each cluster
hst <- hist(dfm_clus$cluster)
counts <- hst$counts[hst$counts != 0] / (1.75* 10^4)

x <- km_all$centers[,1]
y <- km_all$centers[,2]
kcenters = data.frame(x, y)

map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + geom_point_interactive(data = kcenters, aes(x,y) , tooltip = c("Avondale","Lafayette Ave","Auburn Gresham"), fill="red", shape=21, alpha=0.5, size=counts) +
  ggtitle("All crimes") + theme(plot.title = element_text(hjust = 0.5))
x <- girafe(ggobj = map)
if( interactive() ) print(x)
# ******************************************************************************** #
# plot cluster centers of homicide crimes on Chicago map with community areas
# size based on number of points in each cluster
hst <- hist(km_homicide$cluster)
counts <- hst$counts[hst$counts != 0] / 100

x <- km_homicide$centers[,1]
y <- km_homicide$centers[,2]
kcenters = data.frame(x, y)

map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + geom_point_interactive(data = kcenters, aes(x,y) , tooltip = c("Central Park Ave","South Side"), fill="red", shape=21, alpha=0.5, size=counts) +
  ggtitle("Homicide") + theme(plot.title = element_text(hjust = 0.5))
x <- girafe(ggobj = map)
if( interactive() ) print(x)
# ******************************************************************************** #
# plot cluster centers of robbery crimes on Chicago map with community areas
# size based on number of points in each cluster
hst <- hist(km_robbery$cluster)
counts <- hst$counts[hst$counts != 0] / 1000

x <- km_robbery$centers[,1]
y <- km_robbery$centers[,2]
kcenters = data.frame(x, y)

map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + geom_point_interactive(data = kcenters, aes(x,y) , tooltip = c("West Town","Princeton Ave"), fill="red", shape=21, alpha=0.5, size=counts) +
  ggtitle("Robbery") + theme(plot.title = element_text(hjust = 0.5))
x <- girafe(ggobj = map)
if( interactive() ) print(x)
# ******************************************************************************** #
# plot cluster centers of sexual assault crimes on Chicago map with community areas
# size based on number of points in each cluster
hst <- hist(km_assault$cluster)
counts <- hst$counts[hst$counts != 0] / 100

x <- km_assault$centers[,1]
y <- km_assault$centers[,2]
kcenters = data.frame(x, y)

map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + geom_point_interactive(data = kcenters, aes(x,y) , tooltip = c("W North Ave","South Side"), fill="red", shape=21, alpha=0.5, size=counts) +
  ggtitle("Sexual Assault") + theme(plot.title = element_text(hjust = 0.5))
x <- girafe(ggobj = map)
if( interactive() ) print(x)
# ******************************************************************************** #
# ****************************** heatmaps plotting ******************************* #
# ******************************************************************************** #
# all crimes heatmap
map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + stat_density2d(data=dfm_clus,  aes(x=dfm1.dfm1.Longitude, y=dfm1.dfm1.Latitude, fill = ..level.., alpha = ..level..), geom = "polygon", bins = 1000)
map <- map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")), name = "Distribution", guide = FALSE)
map <- map +labs(x='Longitude', y='Latitude', title='All crimes')
map <- map + scale_alpha(range = c(0,0.1),guide = FALSE)
map <- map + theme_bw()
map <- map + theme(plot.title = element_text(hjust = 0.5))
map
ggsave("./final_all_heatmap")

# ******************************************************************************** #
# Homicide crime heatmap
map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + stat_density2d(data=dfm_homicide,  aes(x=dfm1.Longitude, y=dfm1.Latitude, fill = ..level.., alpha = ..level..), geom = "polygon", bins = 1000)
map <- map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")), name = "Distribution", guide = FALSE)
map <- map + labs(x='Longitude', y='Latitude', title='Homicide')
map <- map + scale_alpha(range = c(0,0.1),guide = FALSE)
map <- map + theme_bw()
map <- map + theme(plot.title = element_text(hjust = 0.5))
ggsave("./final_homicide_heatmap.png")

# ******************************************************************************** #
# Robbery crime heatmap
map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + stat_density2d(data=dfm_robbery,  aes(x=dfm1.Longitude, y=dfm1.Latitude, fill = ..level.., alpha = ..level..), geom = "polygon", bins = 1000)
map <- map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")), name = "Distribution", guide = FALSE)
map <- map +labs(x='Longitude', y='Latitude', title='Robbery')
map <- map + scale_alpha(range = c(0,0.1),guide = FALSE)
map <- map + theme_bw()
map <- map + theme(plot.title = element_text(hjust = 0.5))
ggsave("./final_robbery_heatmap.png")

# ******************************************************************************** #
# Sexual assault crime heatmap
map <- ggmap(chicago_map, extent="device", legend="none")
map <- map + stat_density2d(data=dfm_sexual_assault,  aes(x=dfm1.Longitude, y=dfm1.Latitude, fill = ..level.., alpha = ..level..), geom = "polygon", bins = 1000)
map <- map + scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral")), name = "Distribution", guide = FALSE)
map <- map +labs(x='Longitude', y='Latitude', title='Sexual Assault')
map <- map + scale_alpha(range = c(0,0.05),guide = FALSE)
map <- map + theme_bw()
map <- map + theme(plot.title = element_text(hjust = 0.5))
map
ggsave("./final_assault_heatmap.png")

# ******************************************************************************** #
# function to sample the dataframe for visualization
sample.df <- function(f,n=10000,split=",",p=0.1){
  con <- file(f,open="rt",)
  on.exit(close(con,type="rt"))
  y <- data.frame()
  #read header
  x <- character(0)
  while(length(x)==0){
    x <- strsplit(readLines(con,n=1),split)[[1]]
  }
  Names <- x
  #read and process data
  repeat{
    x <- tryCatch(read.table(con,nrows=n,sep=split),error = function(e) NULL )
    if(is.null(x)) {break}
    names(x) <- Names
    nn <- nrow(x)
    id <- sample(1:nn,round(nn*p))
    y <- rbind(y,x[id,])
  }
  rownames(y) <- NULL
  return(y)
}
# ******************************************************************************** #
# function to get county by longitude and latitude 
# All points were in cook county so this wasn't very useful
latlong2county <- function(pointsDF) {
  # Prepare SpatialPolygons object with one SpatialPolygon
  # per county
  counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
  IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
  counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                     proj4string=CRS("+proj=longlat +datum=WGS84"))
  # Convert pointsDF to a SpatialPoints object 
  pointsSP <- SpatialPoints(pointsDF, 
                            proj4string=CRS("+proj=longlat +datum=WGS84"))
  
  # Use 'over' to get _indices_ of the Polygons object containing each point 
  indices <- over(pointsSP, counties_sp)
  
  # Return the county names of the Polygons object containing each point
  countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
  countyNames[indices]
}
# ******************************************************************************** #