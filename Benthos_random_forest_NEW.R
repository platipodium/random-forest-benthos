#Prädiktives Mytilus Modell aus Random Forests
#Helmholtz-Zentrum Geesthacht


#Mytilusdaten aufbereiten:
  #Laden der CSV-Datei mit den Mytilus-Daten zum Vorkommen der Miesmuschel
mytilus <- read.csv("Mytilus_SNS4_AP.csv", sep=";")
summary(mytilus) #es gibt 32314 Presence-Daten aber nur 4900 Absence-Daten

  #Punkte ohne Koordinaten und die doppelt vorkommen entfernen
    #remove all points with no coordinates
mytilus2 = subset(mytilus, !is.na(mytilus$Lat) & !is.na(mytilus$Lon))
    #remove all duplicate points
dups <- duplicated(mytilus2[, c("Lat","Lon")])
mytilus3 <- cbind(mytilus2, dups)
mytilus4 <- subset(mytilus3, dups=="FALSE")

  #Mytilus-Daten in SpatialPointsDataFrame überführen und Referenzsystem festlegen
    #make spatial points data frame
library(sp)
coordinates(mytilus4) <- ~Lon + Lat
proj4string(mytilus4)<-CRS("+init=epsg:4326")

  #Mytilus-Daten abbilden
spplot(mytilus4,"PA",cex=0.01,col.regions=c("red","grey"))


#Grainsize-Daten aufbereiten:
  #Daten einlesen und in SpatialPointsDataFrame umwandeln, Referenzsystem setzen
mgs <- read.table("interpolated_d50_NorthSea_1nm.txt",header=T) #contains phi (column val)
mgs1 <- mgs
coordinates(mgs1)<-~lon+lat
proj4string(mgs1)<-CRS("+init=epsg:4326")
  
  #Grainsize-Daten abbilden
library(RColorBrewer)
spplot(mgs1,"val",col.regions = brewer.pal(11, "Spectral"),cex=0.01)

  #SpatialPointsDataFrame in SpatialPixelDataFrame umwandeln
    #Sind die Daten gegridded?
gridded(mgs1) #Ausgabe: FALSE
    #Anschauen der Punktverteilung (Ausschnitt)
mgs_t <-mgs1[1:200,]
plot(mgs_t$lon,mgs_t$lat) #es gibt offenbar fehlende Werte
    #Umwandlung mit der Funktion SpatialPixelDataFrame und abbilden
mgs_pixel <- SpatialPixelsDataFrame(mgs1,data=mgs,tolerance = 0.000100658) #suggested tolerance
spplot(mgs_pixel,"val")
library(grid)
grid.text("phi",x=unit(0.85,"npc"),y=unit(0.5,"npc"),rot=-90)
      #Frage, die bleibt: Weshalb ist Angabe von tolerance nötig, liegt es möglicherweise an
        #den Lücken im sonst scheinbar gegriddedeten Datensatz? Wie erfolgt die Umwandlung
        #von Punkt zu Pixel genau?


#Kombinieren der Muschel- und der Korngrößen-Daten
devtools::install_github("janhoo/crecs")
library(crecs)
mgs_PA <- get.environ(mytilus4,stack(mgs_pixel)) #class SpatialPointsDataFrame

  #Zuschneiden auf den Bereich der Korngrößen-Daten
mgs_PA2 <- mgs_PA[!is.na(mgs_PA$val),]
mgsPA2df <- data.frame(mgs_PA2)

  #Spalte anlegen, in der PA als 1 und 0 definiert ist --> als Faktor definieren
absence <- which(mgs_PA2$PA=="Absence")
presence <- which(mgs_PA2$PA=="Presence")
mgs_PA2[presence,"PA2"] <- 1
mgs_PA2[absence,"PA2"] <- 0
mgs_PA2$PA2 <- as.factor(mgs_PA2$PA2)

  #Karten erzeugen
PA2_plot <- spplot(mgs_PA2,"PA2",cex=0.05,col.regions=c("red","grey"))
val_plot <- spplot(mgs_PA2,"val",cex=0.01)
library(gridExtra)
grid.arrange(val_plot,PA2_plot,ncol=2)


#Random Forest Modell anwenden
mgsPA2_df <- data.frame(mgs_PA2)
library(randomForest)
rf <- randomForest(PA2~val,data=mgsPA2_df)
mgs_PA2$Prediction <- rf$predicted
library(dismo)
mgs_pixel$prediction <- predict(rf,mgs_pixel)

spplot(mgs_pixel,"prediction")
predict <- spplot(mgs_PA2,"Prediction",cex=0.05,col.regions=c("red","grey"))
grid.arrange(PA2_plot,predict,ncol=2)
