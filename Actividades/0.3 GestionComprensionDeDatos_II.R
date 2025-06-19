# GestionComprensionDeDatos_II

#Factores
subject_name<-c("John Doe","Jane Doe", "Steve Graves")
temperature<-c(98.1,98.6,101.4)
flu_status<-c(FALSE,FALSE,TRUE)

temperature[2]
temperature[2:3]
temperature[-2]
temperature[c(TRUE,TRUE,FALSE)]

gender<-factor(c("MALE","FEMALE","MALE"))
gender

blood<-factor(c("O","AB","A"), levels = c("A","B","AB","O"))
blood

symptoms<-factor(c("SEVERE","MILD","MODERATE"), levels=c("MILD","MODERATE","SEVERE"), ordered=TRUE)
symptoms
symptoms>"MODERATE"

subject_name[1]
temperature[1]
flu_status[1]
gender[1]
blood[1]
symptoms[1]


#Listas

subject1<-list(fullname=subject_name[1],
               temperature=temperature[1],
               flu_status=flu_status[1],
               gender=gender[1],
               blood=blood[1],
               symptoms=symptoms[1])

subject1
subject1[2]
subject1[[2]]
subject1$temperature

subject1[c("temperature","flu_status")]

#Data Frames
#Crear un Data Frame
pt_data<-data.frame(subject_name,temperature, flu_status, gender, blood, symptoms, stringsAsFactors = FALSE)
pt_data
pt_data$subject_name
pt_data[c("temperature","flu_status")]

pt_data[1,2]
pt_data[c(1,3),c(2,4)]
pt_data[,1]
pt_data[1,]
pt_data[,]

pt_data[c(1,3),c("temperature","gender")]
pt_data[-2,c(-1,-3,-5,-6)]

pt_data$temp_c<-(pt_data$temperature-32)*(5/9)
pt_data[c("temperature","temp_c")]


#Matrices
m <- matrix(c(1, 2, 3, 4), nrow = 2) 
m 
m <- matrix(c(1, 2, 3, 4), ncol = 2)
m

m <- matrix(c(1, 2, 3, 4, 5, 6), nrow = 2)
m
m <- matrix(c(1, 2, 3, 4, 5, 6), ncol = 2)
m

m[1, ] 
m[, 1] 



#Administración de datos (Guardar, cargar y eliminar estructuras de datos de R)
save(x, y, z, file = "mydata.RData")
load("mydata.RData")

ls() # La función de listado ls() devuelve un vector de todas las estructuras de datos que se encuentran actualmente en la memoria

#Borrar objetos
rm(m, subject1) 
rm(list = ls())

#Leer un CSV
pt_data <- read.csv("pt_data.csv", stringsAsFactors = FALSE) 

mydata <- read.csv("mydata.csv", stringsAsFactors = FALSE, header = FALSE) 

#Guardar un Data Frame como CSV
write.csv(pt_data, file = "pt_data.csv", row.names = FALSE)



#Exploración y comprensión de los datos
usedcars<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0.3 usedcars.csv", stringsAsFactors=FALSE)
str(usedcars)
summary(usedcars$year)
summary(usedcars[c("price", "mileage")]) 


#Medición de la tendencia central: media y mediana
(36000 + 44000 + 56000) / 3
mean(c(36000, 44000, 56000)) 
median(c(36000, 44000, 56000)) 


#Medición de la dispersión: cuartiles y resumen de cinco números
range(usedcars$price)          #La función range() devuelve tanto el valor mínimo como el máximo
diff(range(usedcars$price))    #La combinación de range() con la función de diferencia diff() te permite calcular la estadística de rango con una sola línea de código

IQR(usedcars$price)            #La diferencia entre Q1 y Q3 se conoce como rango intercuartil (RIC) y se puede calcular con la función IQR()

quantile(usedcars$price)       # identificar cuantiles para un conjunto de valores
quantile(usedcars$price, probs = c(0.01, 0.99))  #obtener cuantiles arbitrarios
quantile(usedcars$price, seq(from = 0, to = 1, by = 0.20)) 


#Diagramas de caja
par(mfrow = c(1, 2)) # un renglón, dos columnas
boxplot(usedcars$price, main = "Boxplot of Used Car Prices", ylab = "Price ($)") 
boxplot(usedcars$mileage, main = "Boxplot of Used Car Mileage", ylab = "Odometer (mi.)") 


#Histogramas
par(mfrow = c(1, 2)) 
hist(usedcars$price, main = "Histogram of Used Car Prices", xlab="Price ($)")
hist(usedcars$mileage, main = "Histogram of Used Car Mileage", xlab = "Odometer (mi.)") 


#Varianza y desviación típica
var(usedcars$price) 
sd(usedcars$price) 
var(usedcars$mileage)
sd(usedcars$mileage) 


#Exploración de variables categóricas
table(usedcars$year) 
table(usedcars$model) 
table(usedcars$color) 

model_table <- table(usedcars$model) 
prop.table(model_table)

color_table <- table(usedcars$color) 
color_pct <- prop.table(color_table) * 100 
round(color_pct, digits = 1) 


#Diagramas de dispersión (scatterplots) 
par(mfrow = c(1, 1))
plot(x = usedcars$mileage, y = usedcars$price, 
     main = "Scatterplot of Price vs. Mileage", 
     xlab = "Used Car Odometer (mi.)", 
     ylab = "Used Car Price ($)") 


#Examen de relaciones: tabulaciones cruzadas de dos vías
library(gmodels)

usedcars$conservative<-usedcars$color %in% c("Black", "Gray", "Silver", "White")
table(usedcars$conservative) 
CrossTable(x = usedcars$model, y = usedcars$conservative)









#