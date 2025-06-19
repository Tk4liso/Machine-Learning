#Reglas de aprendizaje - Identificar hongos venenosos con aprendices de reglas 

mushrooms<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\10 mushrooms.csv", stringsAsFactors = TRUE)
str(mushrooms)

mushrooms$veil_type<-NULL
table(mushrooms$type)

#Entrenamiento
library(OneR)

mushroom_1R<-OneR(type ~ ., data=mushrooms)
mushroom_1R

#Evaluación
mushroom_1R_pred<-predict(mushroom_1R, mushrooms)
table(actual=mushrooms$type, predicted=mushroom_1R_pred)

#Mejorar el rendimiento
library(RWeka)

mushroom_JRip<-JRip(type ~ ., data=mushrooms)
mushroom_JRip
