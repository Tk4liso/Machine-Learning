# Problema 1 con k-NN

#El siguiente conjunto de datos proviene del Proyecto 16P5 en Mosteller, F., y Tukey, J. W. 
#(1977). Data Analysis and Regression: A Second Course in Statistics. Addison-Wesley, 
#Reading, MA, pp. 549-551 (indicando su fuente como “Datos utilizados con autorización 
#de Francice van de Walle”).
#El conjunto de datos representa medidas de fertilidad estandarizadas e indicadores
#socioeconómicos para cada una de las 47 provincias francófonas de Suiza, alrededor 
#de 1888. En 1888, Suiza estaba entrando en un período conocido como la transición 
#demográfica; es decir, su fertilidad estaba comenzando a caer desde el alto nivel 
#típico de los países subdesarrollados. El conjunto de datos tiene observaciones 
#sobre seis variables, cada una de las cuales está en porcentaje, es decir, en [0,100].
#Utiliza el algoritmo kNN para encontrar las provincias que tienen medidas de fertilidad similares.

library(class)
library(gmodels)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\2 fertility_measures.csv")
head(data)
str(data)

#Normalizar datos numéricos (la fertilidad mucho mayor que otras variables puede sesgar los resultados)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_n<-as.data.frame(lapply(data[2:7],normalize))

data_train<-data_n[1:32,]
data_test<-data_n[33:47,]

data_train_labels<-data[1:32,1]
data_test_labels<-data[33:47,1]

#Hacemos K-NN (raíz cuadrada de 47=6.8, se redondea a 7)
data_test_pred<-knn(train=data_train, test=data_test, cl=data_train_labels,k=7)

cross_table<-CrossTable(x=data_test_labels, y=data_test_pred, prop.chisq = FALSE)

#Probamos con puntuación Z

data_z<-as.data.frame(scale(data[-1]))
summary(data_z)


#Otras pruebas 
library(ggplot2)
library(GGally)

#Datos sin modificar
ggpairs(data[-1])

#Normalizados (no afecta a los resultados ya que ambos están en una escala de 0 a 100)
ggpairs(data_n)




# ----> Naiv Bayes <----
library(naivebayes)
library(dplyr)
library(ggplot2)
library(psych)
library(e1071)

pairs.panels(data[-1])

#Crear categorías de fertilidad 
data$Fertility_Category <- cut(data$Fertility, 
                               breaks = quantile(data$Fertility, probs = c(0, 1/3, 2/3, 1), na.rm = TRUE), 
                               labels = c("Baja", "Media", "Alta"), 
                               include.lowest = TRUE)
head(data$Fertility_Category)

# Actualizar las etiquetas de entrenamiento y prueba con las nuevas categorías
data_train_labels <- data$Fertility_Category[1:32]
data_test_labels <- data$Fertility_Category[33:47]

# Entrenar modelo Naive Bayes
model <- naiveBayes(data_train, as.factor(data_train_labels))
print(model)

# Realizar predicciones
pred <- predict(model, data_test)

# Evaluar el modelo
confusion_matrix <- table(Predicted = pred, Actual = data_test_labels)
print(confusion_matrix)

# Calcular métricas de desempeño
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
cat("Precisión del modelo:", accuracy, "\n")


# Obtener las provincias clasificadas como "Alta" fertilidad
alta_fertilidad <- data[data$Fertility_Category == "Alta", ]
print(alta_fertilidad)

# Provincias clasificadas como "Alta" por el modelo (predicción)
alta_predichas <- data[33:47, ][pred == "Alta", ]
print(alta_predichas)
# ----> Regresión lineal <----

plot1<-data %>%
  ggplot(aes(Fertility))+
  geom_histogram(fill="turquoise", bins=20)
plot1


library(DataExplorer)
#Correlación de vars con fertilidad
data %>%
  select(Fertility, Agriculture, Examination, Education, Catholic, Infant..Mortality) %>%
  plot_correlation(ggtheme = theme_light(), title = "Correlations with Fertility")


#Ajustar el modelo
model_lm <- lm(Fertility ~ Agriculture + Examination + Education + Catholic + Infant..Mortality, data = data[1:32, ])
summary(model_lm)

predictions_lm <- predict(model_lm, data[33:47, ])

#Calcular residuales (diferencia entre predicho y real)
residuals_lm <- data$Fertility[33:47] - predictions_lm

#Provincias con residuales pequeños
similar_provinces <- data_test[abs(residuals_lm) < mean(abs(residuals_lm)), ]
print(similar_provinces)

#Valores predichos vs reales
plot(data$Fertility[33:47], predictions_lm, xlab = "Fertility Real", ylab = "Fertility Predicho", main = "Real vs Predicho")
abline(0, 1, col = "red")  #Línea de referencia










#