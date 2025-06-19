#Ejemplo de naïve Bayes (2025)

library(foreign)

data<-read.dta("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\9 hsbdemo.dta")
head(data,5)
str(data)
summary(data)

#Crear clasificador Naive Bayes
library(caret)
library(e1071)

set.seed(1)
trainIndex<-createDataPartition(data$prog, p=0.7)$Resample1
train<-data[trainIndex,]
test<-data[-trainIndex,]

NBclassifier<-naiveBayes(prog ~ ., data=train)
NBclassifier$apriori         #Prior
NBclassifier$tables$science  #Posterior
NBclassifier$tables$honors

#Predicciones
trainPred<-predict(NBclassifier, newdata = train, type="class")
trainTable<-table(train$prog, trainPred)
testPred<-predict(NBclassifier, newdata = test, type = "class")
testTable <- table(test$prog, testPred)

trainAcc<-sum(diag(trainTable))/sum(trainTable)
testAcc<-sum(diag(testTable))/sum(testTable)

message("Confusion Matrix for Training Data")
print(trainTable)
message("Confusion Matrix for Test Data") 
print(testTable) 
message("Accuracy") 
print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))


# ==== Vainas de tarea ====

#PROBAR CON 90/10 PARA EL MODELO
set.seed(1)
trainIndex<-createDataPartition(data$prog, p=0.9)$Resample1
train<-data[trainIndex,]
test<-data[-trainIndex,]

NBclassifier<-naiveBayes(prog ~ ., data=train)
NBclassifier$apriori         #Prior
NBclassifier$tables$science  #Posterior
NBclassifier$tables$honors

#Predicciones
trainPred<-predict(NBclassifier, newdata = train, type="class")
trainTable<-table(train$prog, trainPred)
testPred<-predict(NBclassifier, newdata = test, type = "class")
testTable <- table(test$prog, testPred)

trainAcc<-sum(diag(trainTable))/sum(trainTable)
testAcc<-sum(diag(testTable))/sum(testTable)

message("Confusion Matrix for Training Data")
print(trainTable)
message("Confusion Matrix for Test Data") 
print(testTable) 
message("Accuracy") 
print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))


#EJEMPLO

library(tidyverse)

dataIris = iris

# Density plot for each species
dataIris %>%
  gather(Attributes, value, 1:4) %>%
  ggplot(aes(x=value, fill=Species)) +
  geom_density(colour="black", alpha=0.5) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Iris data set",
       subtitle="Density plot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())


# ==== Plots ====

# --> Densidad <--
library(tidyverse)

#Juntar las columnas de puntuaciones
data %>%
  gather(Attributes, value, read:socst) %>%   #Especifica el rango de columnas que quieres usar
  ggplot(aes(x=value, fill=prog)) +  #Se cambia 'science' por 'prog' para mostrar la categorización
  geom_density(colour="black", alpha=0.5) +
  facet_wrap(~Attributes, scales="free_x") +
  labs(x="Values", y="Density",
       title="Academic Data",
       subtitle="Density plot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())

# --> Boxplots <--
data %>%
  gather(Attributes, value, read:socst) %>%   # Especifica el rango de columnas que quieres usar
  ggplot(aes(x=prog, y=value, fill=prog)) +  # Usamos 'prog' para categorización
  geom_boxplot(alpha=0.5, colour="black") +
  facet_wrap(~Attributes, scales="free_y") +  # Usamos 'free_y' para ajustar el eje y según cada atributo
  labs(x="Program", y="Values",
       title="Academic Data",
       subtitle="Boxplot for each attribute") +
  theme_bw() +
  theme(legend.position="bottom",
        legend.title=element_blank())


# --> Matriz de correlación <--
library(ggplot2)
library(GGally)

ggpairs(data[-1]) #Volamos la columna de ID's

# --> De General

#Filtrado: seleccionar solo las filas donde la columna prog tenga el valor de "general"
general_data<-filter(data, prog=="general")

#Selección: elegir las columnas numéricas (de la columna read hasta la columna socst)
general_data<-select(general_data, read:socst)

cor_general <- cor(general_data)
print("Correlation matrix for General:")
print(cor_general)

# --> De Academic
#Filtrado: seleccionar solo las filas donde la columna prog tenga el valor de "academic"
acad_data<-filter(data, prog=="academic")

#Selección: elegir las columnas numéricas 
acad_data<-select(acad_data, read:socst)

cor_acad <- cor(acad_data)
print("Correlation matrix for Academic:")
print(cor_acad)

# --> De Vocation
#Filtrado: seleccionar solo las filas donde prog="vocation"
voc_data<-filter(data, prog=="vocation")

#Selección: elegir las columnas numéricas 
voc_data<-select(voc_data, read:socst)

cor_voc <- cor(voc_data)
print("Correlation matrix for Vocation:")
print(cor_voc)



# --> Scatterplot <--
ggplot(data, aes(x=read, y=math)) + 
  geom_point() +
  labs(title="Scatterplot of Reading vs Math Scores", x="Reading Score", y="Math Score")

# --> Violines xd <--
#Relación entre Variables Categóricas y Numéricas
ggplot(data, aes(x=prog, y=math, fill=prog)) + 
  geom_violin() + 
  labs(title="Violin Plot of Math Scores by Program", x="Program", y="Math Score")

# --> Histograma múltiple (math vs reading)<--
ggplot(data) +
  geom_histogram(aes(x=read, fill="Read"), binwidth=5, alpha=0.5) +
  geom_histogram(aes(x=math, fill="Math"), binwidth=5, alpha=0.5) +
  labs(title="Comparison of Reading and Math Scores", x="Score", y="Frequency") +
  scale_fill_manual(values=c("skyblue", "orange")) +
  theme_minimal()






#xd
