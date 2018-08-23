#Helmholtz-Zentrum Geesthacht
#Random Forest Model for North Sea Benthos
#Charlotte Schramm, Carsten Lemmen

#Um dieses Dokument reproduzierbar zu machen, verwenden wir die Funktion checkpoint, die alle benötigten Pakete installiert,
#die so verwendet wurden.
#library(checkpoint)
#checkpoint(snapshotDate = "2018-08-22")

#Pakete laden
packages <- c("sp","devtools","grid","RColorBrewer","crecs","dismo","maptools","randomForest")
lapply(packages, require, character=TRUE)


#Daten einlesen
mytilus_alldata <- read.table("Mytilus_SNS4_AP.csv",header=T,sep=";",dec=".")
mytilus <- mytilus_alldata[,c(2,3,6)]

#load("Predictor.rda")


#make spatial points data frame
library(sp)
coordinates(mytilus) <- ~mytilus_alldata.Lon + mytilus_alldata.Lat
proj4string(mytilus)<-CRS("+init=epsg:4326")
#plotten
spplot(mytilus,"mytilus_alldata.PA",cex=0.1,col.regions=c("red","black"))

#spplot mit log2(mgs)
spplot(Predictor,"mgs",formula=log2(mgs)~s1+s2)
library(grid)
grid.text("log2(mgs)",x=unit(0.95,"npc"),y=unit(0.50,"npc"),rot=-90)

#spplot mit phi
spplot(Predictor,"mgs",formula=(-log2(mgs/1000)~s1+s2),col.regions = brewer.pal(11, "Spectral"),
       at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),
       pretty=TRUE) #Krumbein phi scale
grid.text("phi",x=unit(0.95,"npc"),y=unit(0.50,"npc"),rot=-90)




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
coordinates(mgs)<-~lon+lat
proj4string(mgs)<-CRS("+init=epsg:4326")

library(RColorBrewer)
spplot(mgs,"val",col.regions = brewer.pal(11, "Spectral"),
       at=c(-5,-4,-3,-2,-1,0,1,2,3,4,5),
       pretty=TRUE) #Krumbein phi scale


mgs <- data.frame(Predictor, layer=2) #aus Kaelas Skript
Predictor$phi <- (log2(mgs[, "mgs"])) #aus Kaelas Skript

stack(Predictor)
proj4string(Predictor)<-CRS("+init=epsg:4326")
devtools::install_github("janhoo/crecs")
library(crecs)
med.env<-get.environ(points=med4,env=stack(Predictor))

#clip the points to SNS
med.env2<-med.env[!is.na(med.env$depthg14),]

#create background points
#install.packages("dismo")
library(dismo)
set.seed(1)
bg <- randomPoints(Predictor, nrow(med.env2)) #function generate random points that can be used to extract background values
bgdf <- as.data.frame(bg)
coordinates(bgdf) <- ~x + y
proj4string(bgdf)<-CRS("+init=epsg:4326")
bg.env<-get.environ(points=bgdf,env=stack(Predictor))

#combine presence and absence points
med.env2$PA2 <- c(rep(1, nrow(med.env2)))
bg.env$PA2 <- c(rep(0, nrow(bg.env)))
library(maptools)
medPA<- spRbind(med.env2[, 10:ncol(med.env2)], bg.env)

#auch aus Kaelas Skript
#glm
medPA2 <- data.frame(medPA)
library(randomForest)
m1 <-randomForest(as.factor(PA2) ~ depthg14 + phi, data=medPA)
medPA$prediction <-m1$predicted
medPAdf <- data.frame(medPA)

Predictor$prediction <- predict(m1, Predictor)

library(raster)
plot(raster(Predictor, layer=23), col=brewer.pal(11,"Spectral"))
points(med.env2, col="grey25", pch=16, cex=0.5)
points(bgdf, col="darkgrey", bg="grey", pch=21, cex=0.5)

Predictor$cellsize <- area(Predictor)
head(Predictor)

pred <- raster(Predictor, layer=23)
phi <- raster(Predictor, layer=22)
depthg14 <- raster(Predictor, layer=21)


med.env2.utm<-spTransform(med.env2,CRS("+proj=utm +zone=31 +north"))

summary(m1)

#evaluate model

pres<-data.frame(Predictor)[which(Predictor$prediction>0.5),"prediction"]
abs<-as.numeric(as.character(data.frame(Predictor)[which(Predictor$prediction<0.5),"prediction"]))

dismo::evaluate(p=p1,a=a1, model=m1, x=medPA)
summary(m1)

m1


#Data 2000
species20 <- read.table("24d95cd32941c560d37229523351a898146e5837/24d95cd32941c560d37229523351a898146e5837.csv",header=T,sep=",")
species20 <- species20[c(1:6,11)]
summary(species20)
plot(species20$decimalLongitude,species20$decimalLatitude,cex=0.5)
