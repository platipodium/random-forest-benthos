#Praktikum HZG
#random forest

#Pakete installieren
  #install.packages("randomForest")
  #library(randomForest)
install.packages("sp")
library(sp)
install.packages("geoR")
library(geoR)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
install.packages("grid")
library(grid)
install.packages("RColorBrewer")
library(RColorBrewer)
install.packages("devtools")
library(devtools)
install.packages("maptools")
library(maptools)
install.packages("spatstat")
library(spatstat)


#Daten einlesen
mytilus_alldata <- read.table("Mytilus_SNS4_AP.csv",header=T,sep=";",dec=".")
mytilus <- data.frame(mytilus_alldata$Lon,mytilus_alldata$Lat,mytilus_alldata$PA)
#gra_size <-read.table("d50_lon_lat.xyz") #V3=mittlere Sedimentkorngröße

#load("Predictor.rda")

#plotten
  #plot(gra_size$V1,gra_size$V2,cex=0.01)
plot(mytilus$mytilus_alldata.Lon,mytilus$mytilus_alldata.Lat,cex=0.1)


spplot(Predictor,"mgs",formula=log2(mgs)~s1+s2)
grid.text("log2(mgs)",x=unit(0.95,"npc"),y=unit(0.50,"npc"),rot=-90)

spplot(Predictor,"mgs",formula=(-log2(mgs/1000)~s1+s2),col.regions = brewer.pal(11, "Spectral"),
       at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),
       pretty=TRUE) #Krumbein phi scale
grid.text("phi",x=unit(0.95,"npc"),y=unit(0.50,"npc"),rot=-90)


#make spatial points data frame
coordinates(mytilus) <- ~mytilus_alldata.Lon + mytilus_alldata.Lat
proj4string(mytilus)<-CRS("+init=epsg:4326")


##NEW

#load csv file
med <- read.csv("Mytilus_SNS4_AP.csv", sep=";")

#remove all points with no coordinates
med2 = subset(med, !is.na(med$Lat) & !is.na(med$Lon))

#remove all duplicate points
dups <- duplicated(med2[, c("Lat","Lon")])
med3 <- cbind(med2, dups)
med4 <- subset(med3, dups=="FALSE")

#make spatial points data frame
coordinates(med4) <- ~Lon + Lat
proj4string(med4)<-CRS("+init=epsg:4326")

mgs <- read.table("interpolated_d50_NorthSea_1nm.txt",header=T) #contains phi (column val)
coordinates(mgs)<-~lat+lon
proj4string(mgs)<-CRS("+init=epsg:4326")

spplot(mgs,"val",col.regions = brewer.pal(11, "Spectral"),
       at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),
       pretty=TRUE) #Krumbein phi scale

#stack(mgs)
#proj4string(mgs)<-CRS("+init=epsg:4326")
#med.env<-get.environ(points=med4,env=stack(Predictor))


#clip the points to SNS
#med.env2<-med.env[!is.na(med.env$depthg14),]

#create background points
#install.packages("dismo")
#library(dismo)
#set.seed(1)
#bg <- randomPoints(mgs, nrow(med.env2))
#bgdf <- as.data.frame(bg)
#coordinates(bgdf) <- ~x + y
#proj4string(bgdf)<-CRS("+init=epsg:4326")
#bg.env<-get.environ(points=bgdf,env=stack(Predictor))