#Evaluación del rendimiento del modelo, parte I

sms_results<- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 sms_results.csv") 
head(sms_results)

#Mostrar  casos de prueba donde el modelo estimó la probabilidad de spam entre 40 y 60 por ciento
head(subset(sms_results, prob_spam > 0.4 & prob_spam < 0.6))

head(subset(sms_results, actual_type!=predict_type))

#Matríz de confusión
library(gmodels)

table(sms_results$actual_type, sms_results$predict_type)
CrossTable(sms_results$actual_type, sms_results$predict_type) #Lo mismo pero más informativo

# Dado que la precisión es (TP + TN) / (TP + TN + FP + FN)
(152 + 1203) / (152 + 1203 + 4 + 31)
(4 + 31) / (152 + 1203 + 4 + 31) 
1 - 0.9748201 

# ==== Otras medidas de rendimiento ====
library(caret)

#Convertir las columnas a factores
sms_results$actual_type <- factor(sms_results$actual_type, levels = c("ham", "spam"))
sms_results$predict_type <- factor(sms_results$predict_type, levels = c("ham", "spam"))

confusionMatrix(sms_results$predict_type, 
                sms_results$actual_type, positive = "spam") 

pr_a<-0.865+0.109
pr_a

pr_e<-0.868*0.888 + 0.132*0.112
pr_e

k<-(pr_a - pr_e) / (1-pr_e)
k

library(vcd)
Kappa(table(sms_results$actual_type, sms_results$predict_type)) #value = kappa no ponderado

library(irr)
kappa2(sms_results[1:2])

#Coeficiente de correlación de Matthews  | MCC
(152 * 1203 - 4 * 31) / sqrt((152 + 4) * (152 + 31) * (1203 + 4) * (1203 + 31)) 

library(mltools)
mcc(sms_results$actual_type, sms_results$predict_type)

# ifelse() para convertir los valores categóricos ("spam" o "ham") en valores binarios (1 o 0)
cor(ifelse(sms_results$actual_type == "spam", 1,0),
    ifelse(sms_results$predict_type == "spam", 1,0))


#Sensibilidad y especificidad
sens<-152/(152+31)
sens

spec<-1203/(1203+4)
spec

library(caret)
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")
specificity(sms_results$predict_type, sms_results$actual_type, negative = "ham")

#Precisión y recuperación (recall)
library(caret)

prec <- 152 / (152 + 4) 
prec 

rec <- 152 / (152 + 31) 
rec 

posPredValue(sms_results$predict_type, sms_results$actual_type, positive = "spam")
sensitivity(sms_results$predict_type, sms_results$actual_type, positive = "spam")


#La medida F / puntuación F
f <- (2 * prec * rec) / (prec + rec) 
f 

f <- (2 * 152) / (2 * 152 + 4 + 31) 
f 


#Curva ROC (característica operativa del receptor)
library(pROC)

sms_roc<-roc(sms_results$actual_type, sms_results$prob_spam)
plot(sms_roc, main="ROC curve for SMS spam filter", col="blue", lwd=2, legacy.axes=TRUE)

#Comparar con k-NN
sms_results_knn<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 sms_results_knn.csv")
sms_roc_knn <- roc(sms_results$actual_type, sms_results_knn$p_spam) 
plot(sms_roc_knn, col = "red", lwd = 2, add = TRUE) 

#Calcular el AUC (área bajo la curva)
auc(sms_roc)
auc(sms_roc_knn)



# ==== k-folds y bootstraping con Credit Data ====

credit<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\26 credit.csv")
str(credit)

#Convertir variables categóricas a factores
categorical_vars <- c("checking_balance", "credit_history", "purpose", 
                      "savings_balance", "employment_duration", "other_credit", 
                      "housing", "job", "phone", "default")

credit[categorical_vars] <- lapply(credit[categorical_vars], as.factor)


# ----> Bootstrap
library(caret)
library(C50)

set.seed(123) 

#Dividir en 70:30
trainIndex <- createDataPartition(credit$default, p = 0.70, list = FALSE)
train<-credit[trainIndex_70_30, ]
test<-credit[-trainIndex_70_30, ]

train_control_boot <- trainControl(method = "boot", number = 100, classProbs = TRUE, summaryFunction = twoClassSummary) #100 muestras bootstrap

#Modelo C5.0
model_bootstrap <- train(default ~ ., data = train, 
                         method = "C5.0", 
                         trControl = train_control_boot, 
                         metric = "ROC")

print(model_bootstrap)

#Evaluación
prob_bootstrap <- predict(model_bootstrap, newdata = testData_30, type = "prob")[,2]
roc_bootstrap <- roc(testData_30$default, prob_bootstrap, levels = rev(levels(testData_30$default)))



# ----> k-Folds | k=10 usando Árboles
library(caret)
library(C50)

set.seed(123)
train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

#Entrenamiento (70%) y prueba (30%)
trainIndex_70_30 <- createDataPartition(credit$default, p = 0.70, list = FALSE)
trainData_70 <- credit[trainIndex_70_30, ]
testData_30 <- credit[-trainIndex_70_30, ]

#Ajustar el modelo C5.0
model_kFolds <- train(default ~ ., data = trainData_70, 
               method = "C5.0", 
               trControl = train_control,
               metric="ROC")

print(model_kFolds)

#Evaluación
prob_kfold <- predict(model_kFolds, newdata = testData_30, type = "prob")[,2]
roc_kfold <- roc(testData_30$default, prob_kfold, levels = rev(levels(testData_30$default))) # Curva ROC - model_kFolds


# ---> Método de retención para k-Folds | Evaluar model_kFolds dividiendo los datos en entrenamiento, prueba y validación (50:25:25)
set.seed(123)

#Entrenamiento (50%), prueba (25%) y validación (25%)
trainIndex <- createDataPartition(credit$default, p = 0.50, list = FALSE)
trainData <- credit[trainIndex, ]
tempData <- credit[-trainIndex, ]

testIndex <- createDataPartition(tempData$default, p = 0.50, list = FALSE)
testData <- tempData[testIndex, ]
validData <- tempData[-testIndex, ]


train_control <- trainControl(method = "cv", number = 10, classProbs = TRUE, summaryFunction = twoClassSummary)

#Entrenar C5.0 - entrenamiento (50%)
model_kF_retencion <- train(default ~ ., data = trainData, 
               method = "C5.0", 
               trControl = train_control,
               metric="ROC")

#Evaluación en conjunto de prueba (25%)
pred_test <- predict(model_kF_retencion, newdata = testData)
conf_matrix_test <- confusionMatrix(pred_test, testData$default)

#Evaluación en conjunto de validación (25%)
pred_valid <- predict(model_kF_retencion, newdata = validData)
conf_matrix_valid <- confusionMatrix(pred_valid, validData$default)

print(model_kF_retencion)  # Resultados de k-fold CV
print(conf_matrix_test)  # Evaluación en prueba
print(conf_matrix_valid)  # Evaluación en validación

#Predicciones con probabilidades en el conjunto de prueba (25%)
prob_50_25_25 <- predict(model_kF_retencion, newdata = testData, type = "prob")[,2]
roc_50_25_25 <- roc(testData$default, prob_50_25_25, levels = rev(levels(testData$default)))


# ---> Comparar curva ROC de model_kFold vs. model_kF_retencion
library(ggplot2)

#Sólo los k-Folds
ggplot() +
  geom_line(aes(x = 1 - roc_kfold$specificities, y = roc_kfold$sensitivities, color = "K-Fold CV (100%)"), size = 1) +
  geom_line(aes(x = 1 - roc_50_25_25$specificities, y = roc_50_25_25$sensitivities, color = "K-Fold con división 50:25:25"), size = 1) +
  labs(title = "Comparación de Curvas ROC", x = "1 - Especificidad (FPR)", y = "Sensibilidad (TPR)") +
  scale_color_manual(name = "Modelo", values = c("K-Fold CV (100%)" = "blue", "K-Fold con división 50:25:25" = "red")) +
  theme_minimal()


#Los 3 modelos
ggplot() +
  geom_line(aes(x = 1 - roc_kfold$specificities, y = roc_kfold$sensitivities, color = "K-Fold CV (70:30)"), size = 1) +
  geom_line(aes(x = 1 - roc_50_25_25$specificities, y = roc_50_25_25$sensitivities, color = "K-Fold con división 50:25:25"), size = 1) +
  geom_line(aes(x = 1 - roc_bootstrap$specificities, y = roc_bootstrap$sensitivities, color = "Bootstrap (70:30)"), size = 1) +
  labs(title = "Comparación de Curvas ROC", x = "1 - Especificidad (FPR)", y = "Sensibilidad (TPR)") +
  scale_color_manual(name = "Modelo", values = c("K-Fold CV (70:30)" = "blue", 
                                                 "K-Fold con división 50:25:25" = "red", 
                                                 "Bootstrap (70:30)" = "green")) +
  theme_minimal()






#xd