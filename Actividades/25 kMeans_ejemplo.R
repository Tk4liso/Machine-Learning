# K-means | Agrupación encontrando centros con K-means, ejemplo 
library(mlr)
library(tidyverse)
library(ggplot2)
library(GGally)

#Por si es necesario para linea 24: install.packages("clue")

data(GvHD, package = "mclust")
gvhdTib<-as_tibble(GvHD.control)
gvhdTib

#Escalar los datos
gvhdScaled<-gvhdTib %>% scale()

ggpairs(GvHD.control, 
        upper = list(continuous = "density"), 
        lower = list(continuous = wrap("points", size = 0.5)), 
        diag = list(continuous = "densityDiag")) + 
  theme_bw() 

gvhdTask <- makeClusterTask(data = as.data.frame(gvhdScaled))
listLearners("cluster")$class
kMeans <- makeLearner("cluster.kmeans",par.vals = list(iter.max = 100, nstart = 10))

#Ajustar k (# de clústers)
library(clusterSim)

kMeansParamSpace <- makeParamSet( 
  makeDiscreteParam("centers", values = 3:8), 
  makeDiscreteParam("algorithm", values = c("Hartigan-Wong", "Lloyd", "MacQueen")))

gridSearch <- makeTuneControlGrid() 
kFold <- makeResampleDesc("CV", iters = 10) 

tunedK <- tuneParams(kMeans, task = gvhdTask, 
                     resampling = kFold, 
                     par.set = kMeansParamSpace, 
                     control = gridSearch, 
                     measures = list(db, G1))

# ---- Ejercicio 1 ----
kMeans <- makeLearner("cluster.kmeans",par.vals = list(iter.max = 200, nstart = 10))

tunedK <- tuneParams(kMeans, task = gvhdTask, 
                     resampling = kFold, 
                     par.set = kMeansParamSpace, 
                     control = gridSearch, 
                     measures = list(db, G1))

# - - - - - - - - - - - - - - - - - - - - - - - - -


kMeansTuningData <- generateHyperParsEffectData(tunedK) 
kMeansTuningData$data 

gatheredTuningData <- gather(kMeansTuningData$data,
                             key = "Metric", 
                             value = "Value", 
                             c(-centers, -iteration, -algorithm)) 

ggplot(gatheredTuningData, aes(centers, Value, col = algorithm)) + 
  facet_wrap(~ Metric, scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  theme_bw() 

#Entrenamiento del modelo k-means final y optimizado
tunedKMeans <- setHyperPars(kMeans, par.vals = tunedK$x) 
tunedKMeansModel <- train(tunedKMeans, gvhdTask) 
kMeansModelData <- getLearnerModel(tunedKMeansModel) 
kMeansModelData$iter


gvhdTib <- mutate(gvhdTib, kMeansCluster = as.factor(kMeansModelData$cluster)) 
ggpairs(gvhdTib, aes(col = kMeansCluster), 
        upper = list(continuous = "density")) + 
  theme_bw()


#Usar el modelo para predecir clústeres de nuevos datos
newCell <- tibble(CD4 = 510, 
                  CD8b = 26, 
                  CD3 = 500, 
                  CD8 = 122) %>% 
  scale(center = attr(gvhdScaled, "scaled:center"), scale = attr(gvhdScaled, "scaled:scale")) %>% 
  as_tibble() 

predict(tunedKMeansModel, newdata = newCell)




# ==== Ejercicio 2 ====

ggpairs(GvHD.pos, 
        upper = list(continuous = "density"), 
        lower = list(continuous = wrap("points", size = 0.5)), 
        diag = list(continuous = "densityDiag")) + 
  theme_bw() 

gvhdPosTib <- as_tibble(GvHD.pos) 
gvhdPosScaled <- scale(gvhdPosTib)

str(GvHD.pos)
str(gvhdPosTib)
str(gvhdPosScaled)

gvhdPosTask <- makeClusterTask(data = as.data.frame(gvhdPosScaled))
kMeans_Pos <- makeLearner("cluster.kmeans",par.vals = list(iter.max = 200, nstart = 15)) #Cambié nstart en lugar de iter.max

# - - - - -  - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -  - -  - - - - - 
#Ajustar k (No es necesario ejecutar si se ejecutó antes el código anterior)
library(clusterSim)
kMeansParamSpace <- makeParamSet( 
  makeDiscreteParam("centers", values = 3:8), 
  makeDiscreteParam("algorithm", values = c("Hartigan-Wong", "Lloyd", "MacQueen")))
gridSearch <- makeTuneControlGrid() 
kFold <- makeResampleDesc("CV", iters = 10) 

# - - - - -  - -  - - - - - - - - - - - - - - - - - - - - - - - - - - -  - -  - - - - -

tunedKPos <- tuneParams(kMeans_Pos, task = gvhdPosTask, 
                     resampling = kFold, 
                     par.set = kMeansParamSpace, 
                     control = gridSearch, 
                     measures = list(db, G1))

# ---> 

kMeansTuningData_2 <- generateHyperParsEffectData(tunedK) 
kMeansTuningData_2$data 

gatheredTuningData_2 <- gather(kMeansTuningData_2$data,
                             key = "Metric", 
                             value = "Value", 
                             c(-centers, -iteration, -algorithm)) 

ggplot(gatheredTuningData_2, aes(centers, Value, col = algorithm)) + 
  ggtitle("Ejercicio 2 - GvHD.POS") +
  facet_wrap(~ Metric, scales = "free_y") + 
  geom_line() + 
  geom_point() + 
  theme_bw()


# --->

#Entrenamiento del modelo k-means final y optimizado
tunedKMeans_2 <- setHyperPars(kMeans_Pos, par.vals = tunedKPos$x) 
tunedKMeansModel_2 <- train(tunedKMeans_2, gvhdPosTask) 
kMeansModelData_2 <- getLearnerModel(tunedKMeansModel_2) 
kMeansModelData_2$iter


gvhdPosTib <- mutate(gvhdPosTib, kMeansCluster = as.factor(kMeansModelData_2$cluster)) 
ggpairs(gvhdPosTib, aes(col = kMeansCluster), 
        upper = list(continuous = "density")) + 
  theme_bw()


#Usar el modelo para predecir clústeres de nuevos datos
newCell <- tibble(CD4 = 510, 
                  CD8b = 26, 
                  CD3 = 500, 
                  CD8 = 122) %>% 
  scale(center = attr(gvhdPosScaled, "scaled:center"), scale = attr(gvhdPosScaled, "scaled:scale")) %>% 
  as_tibble() 

predict(tunedKMeansModel_2, newdata = newCell)
suppressWarnings(predict(tunedKMeansModel_2, newdata = newCell))










#xd