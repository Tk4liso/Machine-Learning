#Regresión lineal - Predicción de los costos de las reclamaciones de seguros de automóviles mediante regresión lineal 

insurance<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\13 autoinsurance.csv", stringsAsFactors = TRUE)
str(insurance)
summary(insurance$expenses)
hist(insurance$expenses)

table(insurance$geo_area) 
table(insurance$vehicle_type) 

#Matriz de correlación para las cuatro variables numéricas no binarias
cor(insurance[c("age", "est_value", "miles_driven", "expenses")]) 

#Matriz de diagrama de dispersión
pairs(insurance[c("age","est_value","miles_driven","expenses")], pch=".")

library(psych)
#Matriz de dispersión mejorada
pairs.panels(insurance[c("age", "est_value", "miles_driven","expenses")], pch = ".") 



#Entrenar el modelo con los datos

#Versión larga
ins_model<-lm(expenses ~ age + geo_area + vehicle_type + est_value + miles_driven +  college_grad_ind + speeding_ticket_ind +  hard_braking_ind + late_driving_ind + clean_driving_ind,
              data=insurance)

#Versión corta
ins_model<-lm(expenses ~ ., data=insurance)
options(scipen = 999)
ins_model

#Evaluación del rendimiento del modelo
summary(ins_model)


#Incorporación de relaciones no lineal (mejorar modelo)
insurance$age2<-insurance$age^2
ins_model2 <- lm(expenses ~ . + hard_braking_ind:late_driving_ind, data = insurance) 
summary(ins_model2)


#Predicciones
insurance$pred <- predict(ins_model2, insurance) 
cor(insurance$pred, insurance$expenses) 

plot(insurance$pred, insurance$expenses) 
abline(a = 0, b = 1, col = "red", lwd = 3, lty = 2)


predict(ins_model2, 
        data.frame(age = 30, age2 = 30^2, geo_area = "rural",  
                       vehicle_type = "truck", est_value = 25000, 
                       miles_driven = 14000, college_grad_ind = 0,  
                       speeding_ticket_ind = 0, hard_braking_ind = 0, 
                       late_driving_ind = 0, clean_driving_ind = 1)) 

predict(ins_model2, 
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                       vehicle_type = "truck", est_value = 25000, 
                       miles_driven = 14000, college_grad_ind = 0, 
                       speeding_ticket_ind = 0, hard_braking_ind = 0, 
                       late_driving_ind = 0, clean_driving_ind = 0)) 

predict(ins_model2, 
        data.frame(age = 30, age2 = 30^2, geo_area = "rural", 
                       vehicle_type = "truck", est_value = 25000, 
                       miles_driven = 14000, college_grad_ind = 0, 
                       speeding_ticket_ind = 0, hard_braking_ind = 0, 
                       late_driving_ind = 0, clean_driving_ind = 0))

2435.384 - 1247.903 



#xd