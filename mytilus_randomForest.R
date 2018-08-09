# cl: hier gehört ein richtiger Header rein, mit Autor/Lizenz/Datum

#Praktikum HZG
#random forest

#Pakete installieren
install.packages("randomForest")
library(randomForest)
install.packages("sp")
library(sp)
install.packages("geoR")
library(geoR)
install.packages("dplyr")
library(dplyr)
install.packages("ggplot2")
library(ggplot2)
# install.packages("grid")
# cl: package ‘grid’ is a base package, and should not be updated
library(grid)

#Daten einlesen
mytilus_alldata <- read.table("Mytilus_SNS4_AP.csv",header=T,sep=";",dec=".")
mytilus <- data.frame(mytilus_alldata$Lon,mytilus_alldata$Lat,mytilus_alldata$PA)
#gra_size <-read.table("d50_lon_lat.xyz") #V3=mittlere Sedimentkorngröße

load("Predictor.rda")

#plotten
  #plot(gra_size$V1,gra_size$V2,cex=0.01)
plot(mytilus$mytilus_alldata.Lon,mytilus$mytilus_alldata.Lat,cex=0.1)

#head(Predictor@data,3) #print first three rows from the slot 'data'
#Predictor_mgs <- Predictor[,-(c(1,3:21))] #entfernt nicht benötigte Daten
#head(Predictor_mgs@data,3)
#class(Predictor_mgs)
#spplot(Predictor_mgs)
sc <- scale_colour_gradient(limits=c(-4.3,5))
spplot(Predictor,"mgs",formula=-log2(mgs/1000)~s1+s2,
       #col.regions = rainbow(10, start = 0, end = .5),
       pretty=TRUE,
       col.regions=bpy.colors(10),
       ) #Krumbein phi scale
grid.text("phi",x=unit(0.95,"npc"),y=unit(0.50,"npc"),rot=-90)
class(sc)




