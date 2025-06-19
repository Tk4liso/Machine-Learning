#Pronóstico de datos numéricos: Métodos de regresión I 

# ==== Regresión lineal simple ====
launch<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\11 challenger.csv")
b<-cov(launch$temperature, launch$distress_ct) / var(launch$temperature)
b
a<-mean(launch$distress_ct) - b * mean(launch$temperature)
a

#Correlaciones
r <- cov(launch$temperature, launch$distress_ct)/ (sd(launch$temperature) * sd(launch$distress_ct)) 
r
cor(launch$temperature, launch$distress_ct)

# ==== Regresión lineal múltiple ====

#Fórmula de regresión básica que devuelve un vector de coeficientes beta estimados
reg <- function(y, x) { 
  x <- as.matrix(x) 
  x <- cbind(Intercept = 1, x) 
  b <- solve(t(x) %*% x) %*% t(x) %*% y 
  colnames(b) <- "estimate" 
  print(b) 
} 

str(launch)
reg(y=launch$distress_ct, x=launch[2])

#Este modelo predice el número de eventos de desgaste de la junta tórica utilizando la 
#temperatura, la presión de verificación de campo y el número de identificación del lanzamiento
reg(y=launch$distress_ct, x=launch[2:4])







#xd