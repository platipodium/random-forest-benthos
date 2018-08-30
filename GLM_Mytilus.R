#title: "Generalisiertes Lineares Modell für Mytilus"
#author: "Charlotte Schramm"
#copyright: "Helmholtz-Zentrum Geesthacht, 2018"
#date: "Aug 29, 2018"


library(stats)
#lesen des SpatialPointsDataFrames mit dem Vorkommen von Mytilus
presence_randomabsence_data <- readRDS("presence_randomabsence_data.rds")
summary(presence_randomabsence_data)
#Lesen des SpatialPixelsDataFrame mit den Praediktoren
depth_mgs_pixel <- readRDS("depth_mgs_pixel.rds")
summary(depth_mgs_pixel)
depth_plot <- spplot(depth_mgs_pixel,"V3")
mgs_plot <- spplot(depth_mgs_pixel,"val")
#Lesen des SpatialPointsDataFrame, der Mytilus, Tiefen und phi Daten enthält
pointdata_Mytilus_depth_mgs <- readRDS("pointdata_Mytilus_depth_mgs.rds")
#Funktion glm anwenden
glm_quasibinomial <- glm(PA2~ val+V3,pointdata_Mytilus_depth_mgs,family="quasibinomial") #da Anteilsdaten?
glm_quasibinomial_mgs <- glm(PA2~ val,pointdata_Mytilus_depth_mgs,family="quasibinomial") #nur phi als Prädiktor
#Vorhersagen
predict_glm_2predictors <- predict.glm(object=glm_quasibinomial,newdata=depth_mgs_pixel,type="response")
predict_glm_1predictor <- predict.glm(object=glm_quasibinomial_mgs,newdata=depth_mgs_pixel,type="response")
depth_mgs_pixel_df <- data.frame(depth_mgs_pixel)
predicted_2predictors <- cbind(predict_glm_2predictors,depth_mgs_pixel_df$lon,depth_mgs_pixel_df$lat)
predicted_1predictor <- cbind(predict_glm_1predictor,depth_mgs_pixel_df$lon,depth_mgs_pixel_df$lat)

predicted_2predictors <- as.data.frame(predicted_2predictors)
coordinates(predicted_2predictors) <- ~V2+V3
proj4string(predicted_2predictors) <- CRS("+init=epsg:4326")
predicted_2predictors_plot <- spplot(predicted_2predictors,"predict_glm_2predictors")

predicted_1predictor <- as.data.frame(predicted_1predictor)
coordinates(predicted_1predictor) <- ~V2+V3
proj4string(predicted_1predictor) <- CRS("+init=epsg:4326")
predicted_1predictor_plot <- spplot(predicted_1predictor,"predict_glm_1predictor")

library(gridExtra)
par(mfrow=c(2,2))
predicted_1predictor_plot
grid.arrange(depth_plot,mgs_plot,predicted_1predictor_plot,predicted_2predictors_plot,ncol=2)