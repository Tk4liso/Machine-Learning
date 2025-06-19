#Ejemplo árboles de regresión - Estimación de la calidad de los vinos con árboles de regresión y árboles modelo

wine<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\17 whitewines.csv")
str(wine)

hist(wine$quality)
summary(wine)

#Dividir los datos en entrenamiento y prueba 75:25
wine_train<-wine[1:3750,]
wine_test<-wine[3751:4898,]

#Entrenar el modelo
library(rpart)
library(rpart.plot)

m.rpart<-rpart(quality ~ ., data=wine_train)
m.rpart
summary(m.rpart)

rpart.plot(m.rpart, digits = 3)
rpart.plot(m.rpart, digits = 4, fallen.leaves = TRUE, type=3, extra = 101)

#Evaluación del modelo
p.rpart<-predict(m.rpart, wine_test)
summary(p.rpart)
summary(wine_test$quality)

cor(p.rpart, wine_test$quality)

#FUNCIÓN PARA CALCULAR EL MAE ( qué tan lejos, en promedio, estaba su predicción del valor verdadero)
MAE<-function(actual, predicted){
  mean(abs(actual-predicted))
}

MAE(p.rpart, wine_test$quality)

mean(wine_train$quality)      # calificación de calidad media en los datos de entrenamiento
MAE(5.87, wine_test$quality)  # Si predijéramos el valor 5.87 para cada muestra de vino


#Mejorar el modelo
library(Cubist)

m.cubist<-cubist(x=wine_train[-12], y=wine_train$quality)
m.cubist
summary(m.cubist)

#Evaluación del modelo mejorado
p.cubist<-predict(m.cubist, wine_test)
summary(p.cubist)
cor(p.cubist, wine_test$quality)
MAE(wine_test$quality, p.cubist)





#xd