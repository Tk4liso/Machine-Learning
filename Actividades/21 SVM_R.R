#Máquinas de soporte vectorial con R 

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
# Primero, se genera un conjunto de datos simple y se entrena un SVM lineal.                                  #
# Después, se visualiza la frontera de decisión con una línea recta.                                          #
# Luego, se carga otro conjunto de datos más complicado.                                                      #
# Finalmente, se usa un SVM con kernel radial para encontrar una frontera de decisión curva y se grafica.     #
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #


# ==== SVM lineal ====
library(e1071)

#1. Generar datos simulados
set.seed(10111)
x=matrix(rnorm(40),20,2)
y=rep(c(-1,1),c(10,10))
x[y==1,] = x[y==1,]+1
plot(x, col=y+3, pch=19)

#2. Entrenar un SVM lineal
dat=data.frame(x,y=as.factor(y))
svmfit<-svm(y ~ ., data=dat, kernel="linear", cost=10, scale=FALSE)
print(svmfit)

#3. Visualizar la frontera de decisión
make.grid=function(x,n=75){
  grange=apply(x,2,range)
  x1 = seq(from = grange[1,1], to = grange[2,1],length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1=x1,X2=x2)
}

xgrid = make.grid(x)
xgrid[1:10,]

ygrid=predict(svmfit,xgrid)
plot(xgrid, col=c("red","blue")[as.numeric(ygrid)], pch=20, cex=0.2)
points(x, col=y+3, pch=19)
points(x[svmfit$index,], pch=5, cex=2)

#4. Graficar la ecuación de la recta de decisión

#Extraer los coeficientes lineales cercanos al límite
beta=drop(t(svmfit$coefs) %*% x[svmfit$index,])
beta0=svmfit$rho

plot(xgrid, col = c("red", "blue")[as.numeric(ygrid)], pch = 20, cex = .2)
points(x, col = y + 3, pch = 19) 
points(x[svmfit$index,], pch = 5, cex = 2)
abline(beta0 / beta[2], -beta[1] / beta[2]) 
abline((beta0 - 1) / beta[2], -beta[1] / beta[2], lty = 2) 
abline((beta0 + 1) / beta[2], -beta[1] / beta[2], lty = 2) 


# ==== Clasificador SVM no lineal ====
load(file = "C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\21 ESL.mixture.rda")
names(ESL.mixture) #muestra los nombres de las variables dentro del conjunto de datos

rm(x,y)  #Borra x e y anteriores para evitar conflictos.
attach(ESL.mixture) #hace que los objetos dentro de ESL.mixture puedan usarse directamente sin necesidad de escribir ESL.mixture$nombre_variable
plot(x, col = y+1)  #sumando 1 a 'y' para evitar el color 0 (no válido en R)

#Entrenar el SVM con kernel radial (que puede hacer fronteras curvas)
dat = data.frame(y = factor(y), x)
fit = svm(factor(y) ~ ., data=dat, scale = FALSE, kernel="radial", cost=5) #scale=FALSE → No estandariza las variables (por defecto en R, sí lo hace)

#Visualizar la frontera de decisión
xgrid=expand.grid(X1 = px1, X2 = px2)  #Crea una cuadrícula de valores en el espacio de entrada
ygrid=predict(fit,xgrid)

plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2) 
points(x, col = y + 1, pch = 19) #points() agrega puntos a un gráfico ya existente | pch = 19 los hace más visibles.

#Trazar contornos del modelo
func = predict(fit, xgrid, decision.values = TRUE) 
func = attributes(func)$decision 
xgrid = expand.grid(X1 = px1, X2 = px2) 
ygrid = predict(fit, xgrid) 
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = .2) 
points(x, col = y + 1, pch = 19) 
contour(px1, px2, matrix(func, 69, 99), level = 0, add = TRUE) 
contour(px1, px2, matrix(func, 69, 99), level = 0.5, add = TRUE, col = "blue", lwd = 2) 


# ==== Trabajo extra ====
library(e1071)

set.seed(1)
x=matrix (rnorm (200*2) , ncol =2) 
x[1:100 ,]=x[1:100 ,]+2             #Desplazar los primeros 100 puntos arriba/derecha
x[101:150 ,]= x[101:150 ,] -2       #Desplazar los siguientes 50 puntos abajo/izquierda
y=c(rep (1 ,150) ,rep (2 ,50) ) 
dat=data.frame(x=x,y=as.factor (y)) 
plot(x, col =y) 

new_df = data.frame(y = factor(y), x)
fit2 = svm(y ~ ., data=new_df, scale = FALSE, kernel="radial", cost=5)

#Crear una cuadrícula de valores para visualizar la frontera de decisión
make.grid = function(x, n = 75) {
  grange = apply(x, 2, range)
  x1 = seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 = seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid = make.grid(x)
ygrid = predict(fit2, xgrid)

#Graficar la clasificación con la cuadrícula de colores
plot(xgrid, col = as.numeric(ygrid), pch = 20, cex = 0.2)
points(x, col = y, pch = 19)  # Agregar puntos originales

#Obtener la función de decisión y graficar la frontera
func = predict(fit2, xgrid, decision.values = TRUE)
func = attributes(func)$decision

#Dibujar la frontera de decisión
contour(seq(min(x[,1]), max(x[,1]), length = 75), 
        seq(min(x[,2]), max(x[,2]), length = 75), 
        matrix(func, 75, 75), level = 0, add = TRUE, col = "blue", lwd = 2)







#xd