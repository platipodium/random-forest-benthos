#Praediktives Mytilus Modell aus Random Forests
#Helmholtz-Zentrum Geesthacht
#author: Charlotte Schramm, Carsten Lemmen
#August 2018

#Mytilusdaten aufbereiten:
  #Laden der CSV-Datei mit den Mytilus-Daten zum Vorkommen der Miesmuschel
mytilus <- read.csv("Mytilus_SNS4_AP.csv", sep=";") #diese Datei hat 37214 Zeilen
#mytilus_table <- read.table("Mytilus_SNS4_AP.csv",header=T,sep=";",dec=".") #diese Datei hat 34368 Zeilen, es gehen Zeilen verloren
summary(mytilus) #es gibt 32314 Presence-Daten aber nur 4900 Absence-Daten

  #Punkte ohne Koordinaten und die doppelt vorkommen entfernen
    #remove all points with no coordinates
mytilus2 = subset(mytilus, !is.na(mytilus$Lat) & !is.na(mytilus$Lon) &
                  grepl("Mytilus", mytilus$Species)& 
                  !grepl("Mytilus galloprovincialis",mytilus$Species)&
                  !grepl("Mytilus edulis galloprovincialis",mytilus$Species)& #nur Daten mit Mytilus (edulis) behalten
                  grepl("Presence", mytilus$PA)) #und nur Presence-Werte behalten
    #remove all duplicate points
dups <- duplicated(mytilus2[, c("Lat","Lon")])
mytilus3 <- cbind(mytilus2, dups)
mytilus4 <- subset(mytilus3, dups=="FALSE") #muss FALSE in Anführungszeichen stehen?--> scheint hier
      #keinen Unterschied zu machen
    #nur Lon, Lat, PA und source-Spalten behalten
mytilus.cleaned=mytilus4[c("Lon","Lat","PA","Source")]
summary(mytilus.cleaned)

  #Mytilus-Daten in SpatialPointsDataFrame ueberfuehren und Referenzsystem festlegen
    #make spatial points data frame
library(sp)
coordinates(mytilus.cleaned) <- ~Lon + Lat
proj4string(mytilus.cleaned)<-CRS("+init=epsg:4326")

  #Mytilus-Daten abbilden
spplot(mytilus.cleaned,"PA",cex=0.01,col.regions=c("red","grey"))


#Grainsize-Daten aufbereiten:
  #Daten einlesen und in SpatialPointsDataFrame umwandeln, Referenzsystem setzen
mgs <- read.table("interpolated_d50_NorthSea_1nm.txt",header=T) #contains phi (column val)
mgs <- mgs[,1:3]
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
      #Frage, die bleibt: Weshalb ist Angabe von tolerance nötig, liegt es moeglicherweise an
        #den Luecken im sonst scheinbar gegriddedeten Datensatz? Wie erfolgt die Umwandlung
        #von Punkt zu Pixel genau?


#Kombinieren der Muschel- und der Korngroessen-Daten
  #devtools::install_github("janhoo/crecs")
library(crecs)
mgs_PA <- get.environ(mytilus.cleaned,stack(mgs_pixel)) #class SpatialPointsDataFrame

  #Zuschneiden auf den Bereich der Korngroessen-Daten
mgs_PA2 <- mgs_PA[!is.na(mgs_PA$val),]

  #Karten erzeugen
PA2_plot <- spplot(mgs_PA2,"PA",cex=0.05,col.regions=c("red","grey"))
val_plot <- spplot(mgs_PA2,"val",cex=0.01)
library(gridExtra)
grid.arrange(val_plot,PA2_plot,ncol=2)
library(raster)

#Es muessen pseudo-absence Daten erzeugt werden
set.seed(1)
library(dismo)
random.absence <- randomPoints(mgs_pixel,n=1396,mytilus.cleaned)
  #Umwandlung in einen Dataframe und Zuweisung der Koordinaten und eines 
    #Koordinaten-Refererenz Systems --> class=SpatialPoints
random.absence <- as.data.frame(random.absence)
coordinates(random.absence) <- ~ x+y
proj4string(random.absence) <- CRS("+init=epsg:4326")
  #Zusammenbringen der Absence-Punkte mit dem Korngroessen-Datensatz
mgs.absence <- get.environ(random.absence,stack(mgs_pixel))

#Kombinieren von Presence und Absence Punkten und schreiben als 0 für presence und 1 für
  #absence
mgs_PA2$PA2 <- 1
mgs.absence$PA2 <- 0
library(maptools)
mgsPA.bind <- spRbind(mgs_PA2[,3:ncol(mgs_PA2)],mgs.absence)
#mgsPA.bind$PA2 <- as.factor(mgsPA.bind$PA2) #wollen nicht Kategorien erhalten -->
    #keine Klassifikation sondern Regression mit Random Forest Modell soll durchgeführt werden
  #Abbilden von presence und absence Punkten
spplot(mgsPA.bind,"PA2",cex=0.01,col.regions=c("black","red"))


#Random Forest Modell anwenden
  #dazu erst in DataFrame umwandeln
mgsPA2df <- data.frame(mgsPA.bind)
library(randomForest)
rf <- randomForest(PA2~val,data=mgsPA2df,ntree=500)
  #die vom Modell vorhergesagten Werte an den Punkten, an denen Presence-Absence-Daten vorhanden sind,
    #wird als extra Spalte dem mgsPA.bind Datensatz angehaengt.
mgsPA.bind$Prediction <- rf$predicted
  #f?r den gesamten Pixeldatensatz, in dem die Korngroessen sind, werden die PA-Werte vorhergesagt
library(dismo)
mgs_pixel$prediction <- predict(rf,mgs_pixel)

spplot(mgs_pixel,"prediction")
predict <- spplot(mgs_PA2,"Prediction",cex=0.05,col.regions=c("red","grey"))
grid.arrange(PA2_plot,predict,ncol=2)


#Vergleich der verschiedene Korngroessen-Datensaetze
  #Zuschneiden auf gemeinsamen Bereich mit der Funktion intersect aus dem raster package
intersec <- intersect(mgs_pixel,Predictor)

#Verwenden der Tiefe als zweiten Pr?diktor
  #Etopo1 Datensatz (https://maps.ngdc.noaa.gov/viewers/wcs-client/)

  #Einlesen der Tiefedaten und Umwandlung in SpatialPointsDataFrame
depth.etopo.df <- read.csv("etopo1_bedrock.xyz",sep="",header=FALSE)
depth.etopo <- depth.etopo.df 
coordinates(depth.etopo) <- ~V1 + V2
proj4string(depth.etopo)<-CRS("+init=epsg:4326")

  #Umwandlung der Tiefendaten in SpatialPixelsDataFrame
depth.pixel <- SpatialPixelsDataFrame(depth.etopo,depth.etopo.df)
spplot(depth.pixel,"V3")

mgsDepthPA <- get.environ(mgsPA.bind,stack(depth.pixel))
mgsDepthPA <- mgsDepthPA[!is.na(mgsDepthPA$V3),]

spplot(mgsDepthPA,"V3",cex=0.1)
rf.withDepth <- randomForest(PA2~ val+V3,mgsDepthPA)
  #Um die Vorhersagewerte zu erhalten, m?ssen die SpatialPixels* Daten von Grainsize und Tiefe
    #beide in einem kombiniert sein.
mgs_pixel$prediction <- predict(rf.withDepth,depth.pixel)
spplot(mgsDepthPA,"prediction",cex=0.1)
