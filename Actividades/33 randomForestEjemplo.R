#Un ejemplo de random forest 
library(ggplot2)

#Import the dataset
diamond<-diamonds
head(diamond)

# Convert the variables to numerical 
diamond$cut <- as.integer(diamond$cut) 
diamond$color <-as.integer(diamond$color) 
diamond$clarity <- as.integer(diamond$clarity) 
head(diamond)

library(dplyr) 
# Create features and target 
X <- diamond %>% 
  select(carat, depth, table, x, y, z, clarity, cut, color) 
y <- diamond$price


library(randomForest) 
library(caret) 
# Split data into training and test sets 
index <- createDataPartition(y, p=0.75, list=FALSE) 
X_train <- X[ index, ] 
X_test <- X[-index, ] 
y_train <- y[index] 
y_test<-y[-index] 
# Train the model  
regr <- randomForest(x = X_train, y = y_train , maxnodes = 10, ntree = 10) 

# Make prediction 
predictions <- predict(regr, X_test) 
result <- X_test 
result['price'] <- y_test 
result['prediction']<-  predictions 
head(result) 

# Build scatterplot 
ggplot() +
  geom_point( aes(x = X_test$carat, y = y_test, color = 'red', alpha = 0.5) ) +  
  geom_point( aes(x = X_test$carat , y = predictions, color = 'blue',  alpha = 0.5)) + 
  labs(x = "Carat", y = "Price", color = "", alpha = 'Transperency') + 
  scale_color_manual(labels = c( "Predicted", "Real"), values = c("blue", "red")) 


library(Metrics) 
#precision, recall 
print(paste0('MAE: ' , mae(y_test,predictions) )) 
print(paste0('MSE: ' ,caret::postResample(predictions , y_test)['RMSE']^2 )) 
print(paste0('R2: ' ,caret::postResample(predictions , y_test)['Rsquared'] )) 


# ----> Ajuste de los parámetros
N=500 #length(X_train) 
X_train_ = X_train[1:N , ] 
y_train_ = y_train[1:N] 
seed <-7 

metric<-'RMSE' 
customRF <- list(type = "Regression", library = "randomForest", loop = NULL) 
customRF$parameters <- data.frame(parameter = c("maxnodes", "ntree"), 
                                  class = rep("numeric", 2), label = c("maxnodes", "ntree")) 
customRF$grid <- function(x, y, len = NULL, search = "grid") {} 

customRF$fit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
  randomForest(x, y, maxnodes = param$maxnodes, ntree=param$ntree, ...) 
} 
customRF$predict <- function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata) 

customRF$prob <- function(modelFit, newdata, preProc = NULL, submodels = NULL) predict(modelFit, newdata, type = "prob") 
customRF$sort <- function(x) x[order(x[,1]),] 
customRF$levels <- function(x) x$classes 

# Set grid search parameters 
control <- trainControl(method="repeatedcv", number=10, repeats=3, search='grid') 
# Outline the grid of parameters 
tunegrid <- expand.grid(.maxnodes=c(70,80,90,100), .ntree=c(900, 1000, 1100)) 
set.seed(seed) 

# Train the model 
rf_gridsearch <- train(x=X_train_, y=y_train_, method=customRF, metric=metric,
                       tuneGrid=tunegrid, trControl=control)


#Ploteo
plot(rf_gridsearch) 
rf_gridsearch$bestTune 


# ----> Definición y visualización de la importancia de las variables 
varImpPlot(rf_gridsearch$finalModel, main ='Feature importance')




#xd