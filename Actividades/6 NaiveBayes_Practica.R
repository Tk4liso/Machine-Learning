#Problema 2 Naive Bayes - Partido Nazi

library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)

data<-read.table("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\6 nazi.txt",sep="\t", header = TRUE)
head(data)
summary(data)
summary(data$R...C...Re..G...M...Co)

#Búsqueda de datos nulos
colSums(is.na(data))

#Separar la columna en varias columnas
data<-data %>% 
  separate(R...C...Re..G...M...Co,c("R","C","Re","G","M","Co"))

head(data)
str(data)
summary(data)

#Convertir los datos a factor e integer, respectivamente
data$R<-as.factor(data$R)
data$C<-as.factor(data$C)
data$Re<-as.factor(data$Re)
data$G<-as.factor(data$G)
data$M<-as.factor(data$M)
data$Co<-as.integer(data$Co)

summary(data)
str(data)
boxplot(data)
boxplot(data[-6])

#Vista gdata#Vista global pero más bonita
ggplot(gather(data), aes(x = key, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")

#Matriz de correlacion
ggpairs(data)

#Histograma de ls variable Co
plot1<-data %>%
  ggplot(aes(Co))+
  geom_histogram(fill="turquoise", bins=20)
plot1

#Normalizar columna Co
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data$Co<-normalize(data$Co)
head(data)
summary(data$Co)

#Discretizar características numéricas
data$Bin1<-data$Co[1:30]
data$Bin2<-data$Co[31:60]
data$Bin3<-data$Co[61:90]
data$Bin4<-data$Co[91:120]
data<-data[-6]

#Dividir en entrenamiento y prueba
data_train<-data[1:90,]
data_test<-data[91:120,]

data_labels<-data[0,]

#Hacer el modelo
library(naivebayes)

data_labels$Bin1<-as.factor(data_labels$Bin1)
data_labels$Bin2<-as.factor(data_labels$Bin2)
data_labels$Bin3<-as.factor(data_labels$Bin3)
data_labels$Bin4<-as.factor(data_labels$Bin4)

nazi_classifier<-naive_bayes(data_train,data_labels)









#