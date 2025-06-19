#4 ejemploII_Knn (Aceptación de préstamos personales)

library(ggplot2)
library(GGally)
library(dplyr)
library(tidyr)

# ==== Manejo de datos (Limpieza, Filtrado y análisis exploratorio) ====

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\4 UniversalBank.csv")
head(data)
str(data)
summary(data)

#Histograma de las variables con más correlación
plot1<-data %>%
  ggplot(aes(Income))+
  geom_histogram(fill="turquoise", bins=20)
plot1

#Matriz de correlación
cor_matrix<-ggpairs(data[-1])
cor_matrix

#Búsqueda de datos nulos
colSums(is.na(data))

#Diagramas de caja para detectar valores atípicos
boxplot(data) #Vista global

#Vista global pero más bonita
ggplot(gather(data), aes(x = key, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")

#Individuales
boxplot(data$Income)
boxplot(data$ZIP.Code)
boxplot(data$Family)
boxplot(data$CCAvg)
boxplot(data$Education)
boxplot(data$Mortgage)
boxplot(data$Personal.Loan)



# ==== Inciso A ====

#Eliminar columna de ID y ZIP code
data_a<-data %>%
  select(-ID,-ZIP.Code) #Exp. la deberíamos volar por su min. de -3 y para evitar redundancia con Age
head(data_a)

#Hacer las variables ficticias para Education (dummy variables)
#Crear columnas ficticias para Education
data_a <- data_a %>%
  mutate(
    Education_1 = ifelse(Education == 1, 1, 0),
    Education_2 = ifelse(Education == 2, 1, 0),
    Education_3 = ifelse(Education == 3, 1, 0)
  ) %>%
  select(-Education) # Eliminar la columna original


#Agregar un cliente nuevo
new_client<-tibble(Age=40,
               Experience=10,
               Income=84,
               Family=2,
               CCAvg=2,
               Education_1=0,
               Education_2=1,
               Education_3=0,
               Mortgage=0,
               Personal.Loan=1,
               Securities.Account=0,
               CD.Account=0,
               Online=1,
               CreditCard=1)

data_a<-rbind(data_a,new_client)
tail(data_a) #Confirmar la adición del cliente

#= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = 
#Normalizar datos numéricos (el área mucho mayor que la suavidad puede sesgar los resultados)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

data_a<-as.data.frame(lapply(data_a,normalize))
head(data_a)
summary(data_a)

ggplot(gather(data_a), aes(x = key, y = value)) +
  geom_boxplot() +
  facet_wrap(~ key, scales = "free")
#= = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = = =

str(data_a)

# = K-NN =
library(class)

#Separar variables predictoras y objetivo
#x=variables predictoras
#y=variable objetivo || #La variable objetivo es Personal.Loan (si aceptó o no el préstamo)

x<-data_a %>%
  select(-Personal.Loan)
y<-data_a$Personal.Loan

#Seleccionar el cliente nuevo (última fila del dataset)
new_client <- x[nrow(x), ]

#Eliminar al cliente nuevo del conjunto de entrenamiento
x_train <- x[-nrow(x), ]
y_train <- y[-nrow(x)]


pred<-knn(train=x_train, test = new_client, cl = y_train, k = 1)
pred #0=no aceptó || 1=aceptó

# ==== Inciso C ====
library(caret)

set.seed(123)
train_index <- createDataPartition(y, p = 0.7, list = FALSE) #Dividir los datos en entrenamiento (70%) y validación (30%)

x_train <- x[train_index, ]  #Datos de entrenamiento
y_train <- y[train_index]    #Variable objetivo de entrenamiento
x_val <- x[-train_index, ]   #Datos de validación
y_val <- y[-train_index]     #Variable objetivo de validación


error <- c()
for (i in 1:15) {
  knn_fit <- knn(train = x_train, test = x_val, cl = y_train, k = i)
  #Calcular el error de clasificación
  error[i] <- 1 - mean(knn_fit == y_val)
}
print(error) #Mejor k -> k=3 o k=5

ggplot(data = data.frame(k = 1:15, error = error), aes(x = k, y = error)) +
  geom_line(color = "blue") +
  geom_point(color = "red") +
  labs(title = "Error de clasificación vs k", x = "k", y = "Error de clasificación")


# ==== Inciso D ====

new_client <- data.frame(
  Age = 40,
  Experience = 10,
  Income = 84,
  Family = 2,
  CCAvg = 2,
  Education_1 = 0,
  Education_2 = 1,
  Education_3 = 0,
  Mortgage = 0,
  Securities.Account = 0,
  CD.Account = 0,
  Online = 1,
  CreditCard = 1
)

prediction <- knn(train = x_train, test = new_client, cl = y_train, k = 3)
pred


# ==== Inciso E ====

set.seed(123)
train_index<-createDataPartition(y, p = 0.5, list = FALSE) #50%: entrenamiento || 50%: validación y pruebas (30+20=50)

#Datos de entrenamiento (50%)
x_train <- x[train_index, ]
y_train <- y[train_index]
remaining_index <- setdiff(1:nrow(x), train_index)

#Datos de validación (30%)
valid_index <- createDataPartition(y[remaining_index], p = 0.6, list = FALSE) #Dividir el 50% restante en validación (60% de ese 50%) y prueba (40% de ese 50%)
x_val <- x[remaining_index[valid_index], ]
y_val <- y[remaining_index[valid_index]]

#Datos de prueba (20%)
x_test <- x[remaining_index[-valid_index], ]
y_test <- y[remaining_index[-valid_index]]



#k-NN - entrenamiento
knn_train <- knn(train = x_train, test = x_train, cl = y_train, k = 5)

#k-NN - validación
knn_val <- knn(train = x_train, test = x_val, cl = y_train, k = 5)

#k-NN - prueba
knn_test <- knn(train = x_train, test = x_test, cl = y_train, k = 5)


conf_train <- table(y_train, knn_train) #Matriz de confusión - entrenamiento
conf_val <- table(y_val, knn_val)       #Matriz de confusión - validación
conf_test <- table(y_test, knn_test)    #Matriz de confusión - prueba

conf_train
conf_val
conf_test





#
