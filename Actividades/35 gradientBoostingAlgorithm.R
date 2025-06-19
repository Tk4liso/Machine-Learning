#Gradient boosting algorithm

# ==== Gradient Boosting ====

library(caret)
library(doParallel) 

data <- iris 

#Muestrear aleatoriamente los datos para hacer el conjunto de entrenamiento. Se deja el 10% para la prueba
gbmTrain <- data[sample(nrow(data), round(nrow(data)*0.9),  replace = F),] 

#Crear rejilla de ajuste (grid)
grid <- expand.grid(n.trees = c(1000,1500), interaction.depth=c(1:3), shrinkage=c(0.01,0.05,0.1), 
                    n.minobsinnode=c(20)) 

# This creates the train control. in this example I am using a repeated k-folds cross validation 
# with k= 5 repeated 2 times, allowing parallel. 
ctrl <- trainControl(method = "repeatedcv",number = 5, repeats = 2, allowParallel = T) 

# Register parallel cores 
registerDoParallel(detectCores()-1) 

#build model 
set.seed(124) 
unwantedoutput <- capture.output(GBMModel <- train(Species~.,data = gbmTrain, 
                                                   method = "gbm", trControl = ctrl, tuneGrid = grid)) 

# Note that the "capture.output" function has been used here to avoid pages 
# of output being displayed in the vignette, making it unreadable. 
print(GBMModel) 
confusionMatrix(GBMModel) 
print(GBMModel$resample) #Kappa de Gradient Boosting


# ==== XGBoost ==== 
library(Matrix)
library(xgboost)

head(data)
sum(is.na(data))
str(data)

iris_matrix<-sparse.model.matrix(~ . -Species, data=data)
dim(iris_matrix)
print(iris_matrix)

#Dado que no estamos construyendo un modelo de regresión, la columna de intersección, llena de valores 1, es inútil para este análisis y puede eliminarse de la matriz
iris_matrix <- iris_matrix[, -1] 
print(iris_matrix)
dim(iris_matrix)

#Hacer splits
set.seed(12345)
train_ids <- sample(150, 135) #Partición 90:10 | 135 es el 90% de 150
iris_train <- iris_matrix[train_ids, ] 
iris_test <- iris_matrix[-train_ids, ] 

dim(iris_train)
dim(iris_test)

#Sacar las etiquetas
iris_train_labels <- ifelse(data[train_ids, c("Species")] == "yes", 1, 0) 
iris_test_labels <- ifelse(data[-train_ids, c("Species")] == "yes", 1, 0) 


# - Construir el modelo -
params.xgb <- list(
  objective = "multi:softmax",  # Problema de clasificación multiclase
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1,
  num_class = 3  # Número de clases en la variable objetivo
)


set.seed(555) 
xgb_iris <- xgboost(params = params.xgb,
                    data = iris_train, 
                    label = iris_train_labels, 
                    nrounds = 100, 
                    verbose = 1, 
                    print_every_n = 10) 

#Hacer predicciones
prob_Species <- predict(xgb_iris, iris_test)
pred_Species <- ifelse(prob_Species > 0.50, 1, 0) 
table(pred_Species, iris_test_labels)

library(vcd) 
Kappa(table(pred_Species, iris_test_labels))


# - Sacar kappa de forma alternativa -
library(irr)

contingency_table <- table(pred_Species, iris_test_labels)
contingency_matrix <- as.matrix(contingency_table)
print(contingency_matrix)

#Calcular Kappa usando kappa2()
kappa_result <- kappa2(contingency_matrix)

print(kappa_result)

table(iris_test_labels) #sólo está evaluando sobre un subconjunto del iris donde solo hay una especie


# ----> Corrección de XGBoost
library(Matrix)
library(xgboost)

data <- iris
# Convertir la variable objetivo a numérica (0, 1, 2)
data$Species <- as.numeric(data$Species) - 1  # setosa = 0, versicolor = 1, virginica = 2

# Crear matriz esparsa de características (sin la variable objetivo)
iris_matrix <- sparse.model.matrix(Species ~ . - 1, data = data)

# Hacer splits 90%:10% test
set.seed(12345)
train_ids <- sample(1:nrow(data), size = 0.9 * nrow(data))
iris_train <- iris_matrix[train_ids, ]
iris_test <- iris_matrix[-train_ids, ]

# Sacar las etiquetas
iris_train_labels <- data$Species[train_ids]
iris_test_labels <- data$Species[-train_ids]

# Parámetros del modelo XGBoost multiclase
params.xgb <- list(
  objective = "multi:softmax",  # Clasificación multiclase
  num_class = 3,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

# Entrenar el modelo
set.seed(555)
xgb_iris <- xgboost(
  data = iris_train,
  label = iris_train_labels,
  params = params.xgb,
  nrounds = 100,
  verbose = 1,
  print_every_n = 10
)

# Hacer predicciones
pred_Species <- predict(xgb_iris, iris_test)

# Matriz de confusión
conf_matrix <- table(Predicho = pred_Species, Real = iris_test_labels)
print(conf_matrix)

#Kappa
library(vcd)
print(kappa(conf_matrix)) #Kappa de XGBoost



# ==== Random Forest ==== 
library(randomForest)
library(caret)
library(dplyr)

#Crear splits
set.seed(123)
index <- createDataPartition(data$Species, p=0.90, list=FALSE)
train <- data[index,] 
test <- data[-index,] 

#Entrenar el modelo
rForest <- randomForest(Species ~ ., data=train, maxnodes = 10, ntree = 10) 
print(rForest)
print(rForest$confusion)

library(vcd)
kappa(rForest$confusion)

#Calcular Kappa de forma alternativa
p <- predict(rForest, newdata = test)
actual <- test$Species

#Tabla de contingencia
contingency_table <- table(p, actual)
kappa_RF <- kappa(contingency_table)
kappa_RF #Kappa de Random Forest


plot(rForest)



#xd