#title: "Generalisiertes Lineares Modell für Mytilus"
#author: "Charlotte Schramm"
#copyright: "Helmholtz-Zentrum Geesthacht, 2018"
#date: "Aug 29, 2018"
#license: This report is released  under the CC-by-SA 4.0 license.


library(stats)
#lesen des SpatialPointsDataFrames mit dem Vorkommen von Mytilus
presence_randomabsence_data <- readRDS("presence_randomabsence_data.rds")
summary(presence_randomabsence_data)

#Lesen des SpatialPixelsDataFrame mit den Praediktoren
depth_mgs_pixel <- readRDS("depth_mgs_pixel.rds")
summary(depth_mgs_pixel)
depth_mgs_pixel$V3 <- depth_mgs_pixel$V3*(-1)
depth_mgs_pixel <- subset(depth_mgs_pixel,depth_mgs_pixel$V3>0)
depth_mgs_pixel$log2depth <- log2(depth_mgs_pixel$V3) #logarithmierte Tiefe
depth_plot <- spplot(depth_mgs_pixel,"log2depth",col.regions=blues9,at=seq(from=-9,to=9,by=18/9),main=list("Depth",cex=0.8))
mgs_plot <- spplot(depth_mgs_pixel,"val",col.regions=terrain.colors(21)[1:20],at=seq(from=-4,to=4,by=8/20),main=list("Phi",
                   cex=0.8))

#Lesen des SpatialPointsDataFrame, der Mytilus, Tiefen und phi Daten enthält
pointdata_Mytilus_depth_mgs <- readRDS("pointdata_Mytilus_depth_mgs.rds")
pointdata_Mytilus_depth_mgs$V3 <- pointdata_Mytilus_depth_mgs$V3*(-1)
pointdata_Mytilus_depth_mgs <- subset(pointdata_Mytilus_depth_mgs,pointdata_Mytilus_depth_mgs$V3>0)
pointdata_Mytilus_depth_mgs$log2depth <- log2(pointdata_Mytilus_depth_mgs$V3)

#Funktion glm anwenden
glm_quasibinomial_withlog <- glm(PA2~ val+log2depth,pointdata_Mytilus_depth_mgs,family="quasibinomial") #da Anteilsdaten?
glm_quasibinomial <- glm(PA2~ val+V3,pointdata_Mytilus_depth_mgs,family="quasibinomial")

glm_quasibinomial_mgs <- glm(PA2~ val,pointdata_Mytilus_depth_mgs,family="quasibinomial") #nur phi als Prädiktor

#Vorhersagen
predict_glm_2predictors_withlog <- predict.glm(glm_quasibinomial_withlog,depth_mgs_pixel,type="response")
predict_glm_2predictors <- predict.glm(glm_quasibinomial,depth_mgs_pixel,type="response")
predict_glm_1predictor <- predict.glm(object=glm_quasibinomial_mgs,newdata=depth_mgs_pixel,type="response")
depth_mgs_pixel_df <- data.frame(depth_mgs_pixel)
predicted_2predictors_withlog <- cbind(predict_glm_2predictors_withlog,depth_mgs_pixel_df$lon,depth_mgs_pixel_df$lat)
predicted_2predictors <- cbind(predict_glm_2predictors,depth_mgs_pixel_df$lon,depth_mgs_pixel_df$lat)
predicted_1predictor <- cbind(predict_glm_1predictor,depth_mgs_pixel_df$lon,depth_mgs_pixel_df$lat)


predicted_2predictors <- as.data.frame(predicted_2predictors)
coordinates(predicted_2predictors) <- ~V2+V3
proj4string(predicted_2predictors) <- CRS("+init=epsg:4326")
library(RColorBrewer)
predicted_2predictors_plot <- spplot(predicted_2predictors,"predict_glm_2predictors",col.regions = brewer.pal(9,"OrRd"),
                                     at=seq(from=0,to=1,by=1/9),
                                     main=list(paste("Probability occurence \n(Predictors phi and depth)"),
                                     cex=0.8))

predicted_1predictor <- as.data.frame(predicted_1predictor)
coordinates(predicted_1predictor) <- ~V2+V3
proj4string(predicted_1predictor) <- CRS("+init=epsg:4326")
predicted_1predictor_plot <- spplot(predicted_1predictor,"predict_glm_1predictor",col.regions = brewer.pal(9,"OrRd"),
                                    at=seq(from=0,to=1,by=1/9),main=list("Probability occurence \n(Predictor phi)",cex=0.8))

predicted_2predictors_withlog <- as.data.frame(predicted_2predictors_withlog)
coordinates(predicted_2predictors_withlog) <- ~V2+V3
proj4string(predicted_2predictors_withlog) <- CRS("+init=epsg:4326")
predicted_2predictors_withlog_plot <- spplot(predicted_2predictors_withlog,"predict_glm_2predictors_withlog",
                                             col.regions = brewer.pal(9,"OrRd"),at=seq(from=0,to=1,by=1/9),
                                             main=list("Probability occurence \n(Predictor phi and log2(depth))",cex=0.8))

#Karten plotten
library(gridExtra)
predicted_1predictor_plot
grid.arrange(predicted_1predictor_plot,predicted_2predictors_plot,predicted_2predictors_withlog_plot,ncol=3)
