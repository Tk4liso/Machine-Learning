#Evaluación del rendimiento del modelo, parte II
library(caret)

credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv",stringsAsFactors = TRUE)
str(credit)

random_ids <- order(runif(1000)) 
credit_train <- credit[random_ids[1:500], ] 
credit_validate <- credit[random_ids[501:750], ] 
credit_test <- credit[random_ids[751:1000], ] 

in_train <- createDataPartition(credit$default, p = 0.75, list = FALSE) 
credit_train <- credit[in_train, ] 
credit_test <- credit[-in_train, ] 

set.seed(123)
folds<-createFolds(credit$default, k=10)
str(folds)

credit01_test<-credit[folds$Fold01,]
credit01_train<-credit[-folds$Fold01,]



library(caret)
library(C50)
library(irr)

set.seed(123)
folds<-createFolds(credit$default, k=10)

cv_results <- lapply(folds, function(x) { 
  credit_train <- credit[-x, ] 
  credit_test <- credit[x, ] 
  credit_model <- C5.0(default ~ ., data = credit_train) 
  credit_pred <- predict(credit_model, credit_test) 
  credit_actual <- credit_test$default 
  kappa <- kappa2(data.frame(credit_actual, credit_pred))$value 
  return(kappa) 
}) 

str(cv_results)
mean(unlist(cv_results))
sd(unlist(cv_results))



# ==== Muestreo bootstrap ====
(1 - (1/1000))^1000 
(1 - (1/100000))^100000 
1 / exp(1) 



# ==== ¿Kappa o mejor MCC o curva ROC? ====
table(credit$default)






#xd