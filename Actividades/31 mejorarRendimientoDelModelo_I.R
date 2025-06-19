#Mejora del rendimiento del modelo (Construyendo mejores aprendices), parte I 

library(caret)
modelLookup("C5.0") #Encontrar hiperparametros de ajuste de un modelo

credit<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE)
set.seed(300)
m<-train(default ~., data=credit, method="C5.0")
m
#str(m)

p<-predict(m,credit)
table(p, credit$default)

head(predict(m,credit))
head(predict(m,credit, type="prob")) #obtener las probabilidades estimadas de cada clase

ctrl<-trainControl(method = "cv", number=10, selectionFunction = "oneSE")

grid <- expand.grid(model = "tree",
                    trials = c(1, 5, 10, 15, 20, 25, 30, 35), 
                    winnow = FALSE) 
grid

set.seed(300) 
m <- train(default ~ ., data = credit, method = "C5.0",
           metric = "Kappa", 
           trControl = ctrl, 
           tuneGrid = grid) 
m


# ==== Mejora del rendimiento del modelo con conjuntos ====

# ----> Bagging
library(ipred) 

credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE) 
set.seed(123) 
mybag <- bagging(default ~ ., data = credit, nbagg = 25) 

credit_pred <- predict(mybag, credit) 
table(credit_pred, credit$default) 

library(caret) 
credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv") 
set.seed(300) 
ctrl <- trainControl(method = "cv", number = 10) 
train(default ~ ., data = credit, method = "treebag", 
      trControl = ctrl) 


# ----> Boosting (AdaBoost - Boosting adaptativo)
library(adabag) 

credit <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv", stringsAsFactors = TRUE) 
set.seed(300) 
m_adaboost <- boosting(default ~ ., data = credit) 

p_adaboost <- predict(m_adaboost, credit) 
head(p_adaboost$class) 
p_adaboost$confusion 

# - Con evaluación de rendimiento para datos no vistos -
set.seed(300) 
adaboost_cv <- boosting.cv(default ~ ., data = credit) 
adaboost_cv$confusion 

#Encontrar su kappa
library(vcd) 
Kappa(adaboost_cv$confusion)


# ----> Random Forest
library(randomForest)

set.seed(300) 
rf <- randomForest(default ~ ., data = credit)
rf

#Obtener el kappa de las predicciones out-of-bag
library(vcd) 
Kappa(rf$confusion[1:2,1:2]) 

# - Random Forest con lib(ranger) -
library(ranger) 

set.seed(300) 
m_ranger <- ranger(default ~ ., data = credit) 
m_ranger

#Obtener su kappa
Kappa(m_ranger$confusion.matrix) 






#xd