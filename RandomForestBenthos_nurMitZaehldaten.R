#Prädiktives Mytilus Modell aus Random Forests
#Helmholtz-Zentrum Geesthacht


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
                    !grepl("Mytilus edulis galloprovincialis",mytilus$Species)&#nur Daten mit Mytilus (edulis) behalten
                    grepl("Count", mytilus$measurement)) #und nur Zählwerte behalten
#remove all duplicate points
dups <- duplicated(mytilus2[, c("Lat","Lon")])
mytilus3 <- cbind(mytilus2, dups)
mytilus4 <- subset(mytilus3, dups=="FALSE") #muss FALSE in Anführungszeichen stehen?--> scheint hier
#keinen Unterschied zu machen
#nur Lon, Lat, PA und source-Spalten behalten
mytilus.cleaned=mytilus4[c("Lon","Lat","Count","measurement","Source")]
summary(mytilus.cleaned) #Es sind 1533 Zählwerte aus zwei Quellen (DASSH und JNCC)

#Mytilus-Daten in SpatialPointsDataFrame überführen und Referenzsystem festlegen
#make spatial points data frame
library(sp)
coordinates(mytilus.cleaned) <- ~Lon + Lat
proj4string(mytilus.cleaned)<-CRS("+init=epsg:4326")

#Mytilus-Daten abbilden
mytilus.cleaned$Count <- as.numeric(mytilus.cleaned$Count)
spplot(mytilus.cleaned,"Count",cex=0.1)


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
#Frage, die bleibt: Weshalb ist Angabe von tolerance nötig, liegt es möglicherweise an
#den Lücken im sonst scheinbar gegriddedeten Datensatz? Wie erfolgt die Umwandlung
#von Punkt zu Pixel genau?


#Kombinieren der Muschel- und der Korngrößen-Daten
#devtools::install_github("janhoo/crecs")
library(crecs)
mgs_PA <- get.environ(mytilus.cleaned,stack(mgs_pixel)) #class SpatialPointsDataFrame

#Zuschneiden auf den Bereich der Korngrößen-Daten
mgs_PA2 <- mgs_PA[!is.na(mgs_PA$val),]


#Karten erzeugen
PA2_plot <- spplot(mgs_PA2,"Count",cex=0.1)
val_plot <- spplot(mgs_PA2,"val",cex=0.01)
library(gridExtra)
grid.arrange(val_plot,PA2_plot,ncol=2)
summary(mgs_PA2) #an 120 Punkten liegen Zählwerte vor

#Es müssen pseudo-absence Daten erzeugt werden
set.seed(1)
library(dismo)
random.absence <- randomPoints(mgs_pixel,n=120,mytilus.cleaned)
#Umwandlung in einen Dataframe und Zuweisung der Koordinaten und eines 
#Koordinaten-Refererenz Systems --> class=SpatialPoints
random.absence <- as.data.frame(random.absence)
coordinates(random.absence) <- ~ x+y
proj4string(random.absence) <- CRS("+init=epsg:4326")
#Zusammenbringen der Absence-Punkte mit dem Korngrößen-Datensatz
mgs.absence <- get.environ(random.absence,stack(mgs_pixel))

#Kombinieren von Zählwerten und Absence Punkten und den Absence Punkten Null zuordnen
mgs.absence$Count <- 0
  #Es ist nötig, die Zeilennamen von mgs_PA2 durchzunummerieren um die Funktion spRbind 
    #anwenden zu können; die Zeilennummern der beiden Datensätze die verbunden werden sollen
    #dürfen nicht die gleichen Zeilennummern besitzen
mgs_PA2.df <- data.frame(mgs_PA2)
rownames(mgs_PA2.df)<-121:(nrow(mgs_PA2.df)+120)
mgs_PA2 <- mgs_PA2.df
coordinates(mgs_PA2)<-~Lon+Lat
proj4string(mgs_PA2)<-CRS("+init=epsg:4326")

  
library(maptools)
mgsPA.bind <- spRbind(mgs_PA2[,c(1,4:6)],mgs.absence)

#Abbilden von presence und absence Punkten
spplot(mgsPA.bind,"Count",cex=0.01)


#Random Forest Modell anwenden
mgsPA2df <- data.frame(mgsPA.bind)
library(randomForest)
rf <- randomForest(Count~val,data=mgsPA2df,ntree=500)
mgsPA.bind$Prediction <- rf$predicted
library(dismo)
mgs_pixel$prediction <- predict(rf,mgs_pixel)

spplot(mgs_pixel,"prediction")
