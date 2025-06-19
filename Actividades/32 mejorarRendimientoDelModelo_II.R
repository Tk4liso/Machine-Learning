#Mejora del rendimiento del modelo, parte II

credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE) 
credit$default<-ifelse(credit$default=="yes",1,0)

set.seed(123)
train_sample<-sample(1000,900)
credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]

# ----> GBM (Gradient boosting / Generalized Boosting Models)
library(gbm)
set.seed(300)
m_gbm<-gbm(default ~., data=credit_train)
m_gbm

p_gbm<-predict(m_gbm, credit_test, type = "response")
p_gbm_c<-ifelse(p_gbm > 0.50, 1, 0)
table(credit_test$default, p_gbm_c)

library(vcd)
Kappa(table(credit_test$default, p_gbm_c))

# - Mejorar parámetros -
library(caret)

grid_gbm<-expand.grid(
  n.trees=c(100,150,200),
  interaction.depth=c(1,2,3),
  shrinkage=c(0.01,0.1,0.3),
  n.minobsinnode=10)

ctrl<-trainControl(method = "cv", number = 10, selectionFunction = "best")

credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE) 

set.seed(300)
m_gbm_c<-train(default ~ ., data=credit, method="gbm",
               trControl=ctrl, tuneGrid=grid_gbm,
               metric="Kappa",
               verbose=FALSE)

m_gbm_c



# ----> XGBoost
library(Matrix)

credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE) 

credit_matrix<-sparse.model.matrix(~ . -default, data=credit) #Matriz dispersa
dim(credit_matrix)
print(credit_matrix[1:5, 1:15])

#Dado que no estamos construyendo un modelo de regresión, la columna de intersección, llena de valores 1, es inútil para este análisis y puede eliminarse de la matriz
credit_matrix<-credit_matrix[,-1]
print(credit_matrix[1:5, 1:15])

#Dividir la matriz aleatoriamente en conjuntos de entrenemiento y prueba de 90:10
set.seed(12345)
train_ids<-sample(1000,900)
credit_train<-credit_matrix[train_ids,]
credit_test<-credit_matrix[-train_ids,]

dim(credit_train)
dim(credit_test)

#Etiquetas
credit_train_labels <- ifelse(credit[train_ids, c("default")] == "yes", 1, 0) 
credit_test_labels <- ifelse(credit[-train_ids, c("default")] == "yes", 1, 0) 


library(xgboost) 
params.xgb <- list(objective = "binary:logistic",
                   max_depth = 6, 
                   eta = 0.3, 
                   gamma = 0, 
                   colsample_bytree = 1, 
                   min_child_weight = 1, 
                   subsample = 1) 

#Entrenar el modelo
set.seed(555) 
xgb_credit <- xgboost(params = params.xgb, 
                      data = credit_train, 
                      label = credit_train_labels, 
                      nrounds = 100, 
                      verbose = 1, 
                      print_every_n = 10) 

prob_default <- predict(xgb_credit, credit_test) 
pred_default <- ifelse(prob_default > 0.50, 1, 0) 
table(pred_default, credit_test_labels) 

#Kappa
library(vcd) 
Kappa(table(pred_default, credit_test_labels)) 


# - Ajustar hiperparámetros con un grid (rejilla de ajuste) -
grid_xgb <- expand.grid(
  eta = c(0.3, 0.4), 
  max_depth = c(1, 2, 3), 
  colsample_bytree = c(0.6, 0.8), 
  subsample = c(0.50, 0.75, 1.00), 
  nrounds = c(50, 100, 150), 
  gamma = c(0, 1), 
  min_child_weight = 1) 

library(caret) 
ctrl <- trainControl(method = "cv", number = 10,selectionFunction = "best") 
credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE) 

set.seed(300) 
m_xgb <- train(default ~ ., data = credit, method = "xgbTree",
               trControl = ctrl, tuneGrid = grid_xgb, 
               metric = "Kappa", verbosity = 0) 

m_xgb
m_xgb$bestTune

#Encontrar el Kappa más alto
max(m_xgb$results["Kappa"]) 


# ----> Mejorar el XGB
# Ajustar la rejilla con valores más finos
grid_xgb_fine <- expand.grid(
  eta = c(0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5),
  colsample_bytree = c(0.6, 0.7, 0.8),
  subsample = c(0.75, 0.85, 0.9, 1.0),
  nrounds = c(100, 150, 200),
  gamma = c(0, 0.1),
  min_child_weight = c(1, 5, 10)
)

set.seed(300)
m_xgb_fine <- train(default ~ ., data = credit, method = "xgbTree",
                    trControl = ctrl, tuneGrid = grid_xgb_fine, 
                    metric = "Kappa", verbosity = 0)

print(m_xgb_fine)
m_xgb_fine$bestTune
max(m_xgb_fine$results["Kappa"])





# ----> Stacking (Apilamiento de modelos para meta-aprendizaje)

#xd