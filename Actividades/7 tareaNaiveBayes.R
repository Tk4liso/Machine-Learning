#Tarea - Naive Bayes (Aceptación de préstamos personales)

library(tidyr)
library(dplyr)
library(ggplot2)
library(GGally)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\7 UniversalBank.csv")
head(data)
str(data)
summary(data)

#Histograma de las variables a considerar
data %>%
  ggplot(aes(Online))+
  geom_histogram(fill="turquoise", bins=20)

data %>%
  ggplot(aes(CreditCard))+
  geom_histogram(fill="turquoise", bins=20)

data %>%
  ggplot(aes(Personal.Loan))+
  geom_histogram(fill="turquoise", bins=20)


#Dividir los datos en entrenamiento (60%) y validación (40%)
data_train<-data[1:3000,]
data_test<-data[3001:5000,]


# ==== Inciso A ====
library(reshape)

#Una tabla dinámica (o tabla de contingencia) es una tabla resumen que 
#organiza y agrupa los datos en función de ciertas variables, permitiendo 
#visualizar el conteo de ocurrencias de combinaciones específicas de valores.

#Crear tabla dinámica con table
d_table <- table(
  Online = data_train$Online,
  CreditCard = data_train$CreditCard,
  Personal.Loan = data_train$Personal.Loan
)

d_table

# ==== Inciso B ====
library(e1071)

#Recordar que Naive Bayes trabaja adecuadamente con datos categóricos
data_train$Personal.Loan <- as.factor(data_train$Personal.Loan)
data_train$CreditCard <- as.factor(data_train$CreditCard)
data_train$Online <- as.factor(data_train$Online)

str(data_train)


new_client <- data.frame(CreditCard = factor(1, levels = levels(data_train$CreditCard)), 
                            Online = factor(1, levels = levels(data_train$Online)))

modelo<-naiveBayes(Personal.Loan ~ CreditCard + Online, data = data_train)

pred<-predict(modelo, new_client, type = "raw")  #type = "raw" devuelve probabilidades
pred


# ==== Inciso C ====
tab_loan_vs_online<-table(Prestamo=data_train$Personal.Loan, Online=data_train$Online)
tab_loan_vs_online

tab_loan_vs_cc<-table(Prestamo=data_train$Personal.Loan, CC=data_train$CreditCard)
tab_loan_vs_cc

# ==== Inciso D ====

#i) P(CC = 1 | Loan = 1)
modelo<-naiveBayes(Personal.Loan ~ CreditCard, data=data_train)
modelo
modelo$tables$CreditCard

#ii) P(Online = 1 | Loan = 1)
modelo<-naiveBayes(Personal.Loan ~ Online, data=data_train)
modelo$tables$Online

#iii) P(Loan = 1) (la proporción de aceptantes de préstamos)
modelo<-naiveBayes(Personal.Loan ~ ., data=data_train)
modelo

#iv) P(CC = 1 | Loan = 0)
#Se resuelve con los comandos anteriores

#v) P(Online = 1 | Loan = 0)
#Se resuelve con los comandos anteriores

#vi) P(Loan = 0)
#Se resuelve con los comandos anteriores


# ==== Inciso E ====

p_CC_given_Loan1 <- 0.288
p_Online_given_Loan1 <- 0.625
p_Loan1 <- 0.103

p_CC_given_Loan0 <- 0.286
p_Online_given_Loan0 <- 0.601
p_Loan0 <- 0.897

numerador <- p_CC_given_Loan1 * p_Online_given_Loan1 * p_Loan1
denominador <- (p_CC_given_Loan0 * p_Online_given_Loan0 * p_Loan0) + numerador

p_Loan1_given_CC1_Online1 <- numerador / denominador
p_Loan1_given_CC1_Online1


# ==== Inciso G ====

modelo<-naiveBayes(Personal.Loan ~ CreditCard + Online, data=data_train)
modelo








#
# ==== Aña ====

library(readr)
library(caTools)
library(tidyverse)
library(data.table)
library(reshape)

#importación de datos
UniversalBank=read_csv('C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\7 UniversalBank.csv')
sum(is.na(UniversalBank))
#Divide los datos en conjuntos de entrenamiento (60%) y validación (40%). 
set.seed(123)
split <- sample.split(UniversalBank$Online & UniversalBank$CreditCard & UniversalBank$`Personal Loan`, SplitRatio = 0.6)
training_set=subset(UniversalBank,split==TRUE)
test_set=subset(UniversalBank,split==FALSE)
training_set
test_set

#a._crear tabla dinamica para los datos de entrenamiento
Bank<-table(training_set$Online,training_set$CreditCard,training_set$`Personal Loan`)
dimnames(Bank) <- list(Online = c("No", "Yes"),
                       CreditCard = c("No", "Yes"),
                       Personal.Loan = c("No", "Yes"))
View(Bank)
data_frame <- data.frame(Online = training_set$Online,
                         CreditCard = training_set$CreditCard,
                         Personal.Loan = training_set$`Personal Loan`)
# Crear tabla dinámica utilizando la función table()
Bank <- table(data_frame)
View(Bank)
Bank<-table(training_set$Online,training_set$CreditCard,training_set$`Personal Loan`)
data_frame <- data.frame(Online = training_set$Online,
                         CreditCard = training_set$CreditCard,
                         Personal.Loan = training_set$`Personal Loan`)
print(Bank)





Bank<-table(training_set$Online,training_set$CreditCard,training_set$`Personal Loan`)
data_frame <- data.frame(Online = training_set$Online,
                         CreditCard = training_set$CreditCard,
                         Personal.Loan = training_set$`Personal Loan`)
View(Bank)
Bank <- table(Online = training_set$Online,
              CreditCard = training_set$CreditCard,
              Personal.Loan = training_set$Personal.Loan)
View(Bank)
Bank<-table(training_set$Online,training_set$CreditCard,training_set$Personal.Loan)
View(Bank)
probabilidad_aceptacion <- Bank[2, 2, 2] / sum(Bank[2, , 2])
print(probabilidad_aceptacion)
View(Bank)
prob_loan_accepted <- Bank["1", "1", "1"] / sum(Bank)
prob_loan_accepted
prob_loan_accepted <- Bank["1", "1", "1"] / sum(Bank[1, 1, ])
prob_loan_accepted
prob_loan_accepted <- Bank["1", "1", "1"] / sum(Bank)
total_customers <- sum(Bank[1, 1, ])
prob_loan_accepted <- freq_accepted_loan / total_customers
freq_accepted_loan <- Bank[1, 1, 1]
total_customers <- sum(Bank[1, 1, ])
prob_loan_accepted <- freq_accepted_loan / total_customers
prob_loan_accepted
probabilidad_aceptacion <- Bank[1, 1, 1] / sum(Bank[1, , 1])
print(probabilidad_aceptacion)
Bank_Online <- table(training_set$Online, 
                     training_set$'Personal.Loan')
print(Bank_Online)
View(Bank)
View(Bank_online)
View(Bank_Online)
Bank_Online <- table(Online=training_set$Online, 
                     Personal.Loan =training_set$'Personal.Loan')
print(Bank_Online)
View(Bank_Online)
View(Bank_Online)
q()

library(readr)
library(caTools)
library(tidyverse)
library(e1071)

UniversalBank=read_csv('UniversalBank.csv')
sum(is.na(UniversalBank))
set.seed(123)
split <- sample.split(UniversalBank$Online & UniversalBank$CreditCard & UniversalBank$Personal.Loan, SplitRatio = 0.6)
training_set=subset(UniversalBank,split==TRUE)
test_set=subset(UniversalBank,split==FALSE)
training_set
test_set
Bank <- table(Online = training_set$Online,
              CreditCard = training_set$CreditCard,
              Personal.Loan = training_set$Personal.Loan)
View(Bank)
pres_aceptado <- Bank["1", "1", "1"] / sum(Bank)
pres_aceptado
Bank_Online <- table(Online=training_set$Online, 
                     Personal.Loan =training_set$'Personal Loan')
print(Bank_Online)
Bank_CC <- table(CreditCard = training_set$CreditCard, 
                 Personal.Loan =training_set$'Personal Loan')
print(Bank_CC)
Bank
#i P(CC = 1 | Loan = 1) - Proporción de titulares de tarjetas de crédito entre los aceptantes de préstamos
p_CC<- Bank_CC[2, 2] / sum(Bank_CC[,2 ])
p_CC
#ii P(Online = 1 | Loan = 1)
p_O<- Bank_Online[2, 2] / sum(Bank_Online[,2 ])
p_O
#iii P(Loan = 1) (la proporción de aceptantes de préstamos)
p_1<- Bank_Online[1,1 ] / sum(Bank_Online[1,])
p_1
#iv. P(CC = 1 | Loan = 0)
p_2<- Bank_CC[2, 2] / sum(Bank_CC)
p_2
#v. P(Online = 1 | Loan = 0)
p_3 <- Bank_Online[2, 2] / sum(Bank_Online[,1])
p_3
Bank_online[2,2]
Bank_Online[2,2]
View(Bank_online)
View(Bank_Online)
Bank_Online[,1]
p_3 <- Bank_Online[2, 2] / (Bank_Online[,1])
p_3
Bank_Online[,]
Bank_Online[1,1]
Bank_Online[1,2]
Bank_Online[1,3]
Bank_Online[2,2]
Bank_Online[2,1]
p_3 <- Bank_Online[2, 1] / (Bank_Online[,1])
p_3
p_3 <- Bank_Online[2, 1] / sum(Bank_Online[,1])
p_3
#v. P(Online = 1 | Loan = 0)
p_3 <- Bank_Online[2, 1] / sum(Bank_Online[,1])
p_3
#v. P(Online = 1 | Loan = 0)
p_3 <- Bank_Online[2, 1] / sum(Bank_Online[,1])
p_3
Bank_Online[1,1 ]
Bank_Online[1, ]
Bank_online
Bank_Online
View(Bank_Online)
Bank_Online[1, 1]
Bank_Online[2, 2]
Bank_Online[, 1]
Bank_Online[2, 2]
Bank_Online[,2]
Bank_Online[1,]
Bank_Online[,]
Bank_Online[1,1]
Bank_Online[1,2]
Bank_Online[2,]
Bank_Online[2,2]
Bank_Online[2,1]
Bank_Online[,2]
Bank_Online[,1]
#iii P(Loan = 1) (la proporción de aceptantes de préstamos)
p_1<- Bank_Online[,2 ] / sum(Bank_Online[,1])
p_1
p_1<- Bank_Online[2, ] / sum(Bank_Online[,1])
p_1
#iii P(Loan = 1) (la proporción de aceptantes de préstamos)
p_1<- Bank_Online[, 2] / sum(Bank_Online[,1])
p_1
suma_0_y_1 <- p_1[1] + p_1[2]
print(suma_0_y_1)
Bank_Online
Bank_Online[2,2]
Bank_CC[2,2]
sum(Bank_CC)
sum(Bank_Online[,1])
p_1<- sum(Bank_Online[, 2]) / sum(Bank_Online[,1])
p_1
p_2<- Bank_CC[2, 2] / sum(Bank_CC)
p_2
p_2<- Bank_CC[2, 2] / (Bank_CC)
p_2
Bank_CC
Bank_CC[1,]
Bank_CC[1,1]
Bank_CC[1,2]
Bank_CC[1,1]
#iv. P(CC = 1 | Loan = 0)
p_2<- Bank_CC[1, 2] / (Bank_CC[1,1])
p_2
p_2<- Bank_CC[1, 2] / sum(Bank_CC[1,1])
p_2
Bank_CC
Bank_CC[1,2]
Bank_CC[2,2]
Bank_CC[,1]
Bank_CC[2,1]
p_2<- Bank_CC[2, 1] / sum(Bank_CC[1,1])
p_2
Bank_CC[1,1]
Bank_CC[,1]
p_2<- Bank_CC[2, 1] / sum(Bank_CC[,1])
p_2
#iv. P(CC = 1 | Loan = 0)
p_2<- Bank_CC[2, 1] / sum(Bank_CC[,1])
p_2
Bank_CC[1,1 ]
sum(Bank_CC[1,])
#vi. P(Loan = 0)
p_4<- Bank_CC[1,1 ] / sum(Bank_CC[1,])
p_4
model <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = training_set)
new_data <- data.frame(Online = 1, CreditCard = 1)
prob_prediction <- predict(model, newdata = new_data, type = "raw")
print(prob_prediction)
modelo <- naive_bayes(Personal.Loan ~ ., data = Uni, usekernel = TRUE)
#probabilidades<-predict(modelo,datos=Uni,type"prob")
clasificación<-ifelse(probabilidades[,1]<0.5,1,0)
Uni$Clasificacion<-clasificacion 
model <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = training_set)
new_data <- data.frame(Online = 1, CreditCard = 1)
prob_prediction <- predict(model, newdata = new_data, type = "raw")
print(prob_prediction)
model <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = training_set)
p_bayes <- data.frame(Online = 1, CreditCard = 1)
prob_prediction <- predict(model, newdata = p_bayes, type = "raw")
print(prob_prediction)
model <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = training_set)
p_bayes <- data.frame(Online = 1, CreditCard = 1)
prob_prediction <- predict(model, newdata = p_bayes, type = "raw")
print(prob_prediction[1, 1])
model <- naiveBayes(Personal.Loan ~ Online + CreditCard, data = training_set)
p_bayes <- data.frame(Online = 1, CreditCard = 1)
prob_prediction <- predict(model, newdata = p_bayes, type = "raw")
print(prob_prediction[[1, 1]])
(prob_prediction[[1, 1]])*(10)