# ==== Introducción a R ====

#Crear matrices
x<-matrix(1:9, nrow = 3, ncol = 3)
x
x[,1]     #X [,j] se refiere a la columna j,
x[1,]     #X [i,] se refiere a la i-ésima fila de la matriz X
x[1,1]    #X[i,j] se refiere al elemento de la i-ésima fila, j-ésima columna, respectivamente
x[2,3]
x[3,3]

#Arrays
#Estructura: myarray <- array (vector, dimensions, dimnames)

dim1<-c("A1","A2")
dim2<-c("B1","B2","B3")
dim3<-c("C1","C2")
dim1
dim2
dim3

#arreglo tridimensional (2 × 3 × 2)
z<-array(1:12,c(2,3,2), dimnames = list(dim1,dim2,dim3))
z


# ==== Análisis de Datos ====

myCSV<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0 c31.csv")
str(myCSV)
View(myCSV)

myTable<-read.table("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0 c311.txt",sep="\t", header = TRUE)
str(myTable)

library(readxl)
myODBC<-read_excel("C:/Users/Tacos/OneDrive/Documentos/Universidad/9. Noveno Semestre/Máquinas de aprendizaje/Actividades/0 c311Lot1.xls", sheet="LOT")
str(myODBC)

library("xlsx")
myxlsx<-"C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0 c311Lot.xlsx"
myxlsxdata<-read.xlsx(myxlsx,1)


data<-read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/wine-quality/winequality-white.csv", sep=";")
summary(data)

data=read.table(file="http://www.sthda.com/upload/decathlon.txt",
                header = T, row.names = 1, sep="\t")
print(data)


#Estructuras de control:
#    if y else: Prueba una condición
#    while: Ejecuta un ciclo cuando una condición es verdadera
#    for: Repite un número fijo de veces
#    repeat: Ejecuta un ciclo continuamente (debe salir del ciclo salir)
#    next: Opción para omitir una iteración de un ciclo
#    break: Rompe la ejecución de un ciclo

#if
x<-runif(1,0,20) #Genera un # randonm uniforme
x

if (x>10) {
  print(x)  
} else{
  print("x is less than 10")
}

#for
x<-c("Juan","Beni","Jorge","David")
x

for (i in 2:3) {
  print(x[i])
}

for (i in seq_along(x)) { #La función seq_along() genera un número entero basado en la longitud del objeto.
  print(x[i])
}

#while
count=0
while (count<10) {
  print(count)
  count=count+1
}


#Funciones de ciclo

#apply(): Evalúa una función en una sección de un arreglo y devuelve los resultados en un arreglo.
#lapply(): Recorre una lista y evalúa cada elemento o aplica la función a cada elemento.
#sapply(): Una aplicación fácil de usar de lapply() que devuelve un vector, matriz o
#arreglo.
#tapply): Usualmente se usa sobre un subconjunto de un conjunto de datos

#apply
apply(cars,2,mean) #encontrar la velocidad promedio y la distancia promedio de los autos
head(cars)

str(apply)

apply(cars,2,quantile)

#lapply
str(lapply)
str(cars)

lap<-lapply(cars,mean)
lap
str(lap)

#sapply
#La principal diferencia entre sapply() y lapply() es el resultado de salida. El resultado de sapply() puede ser un vector o una matriz, mientras que el resultado de lapply() es una lista.
sap<-sapply(cars, mean)
sap
str(sap)

#tapply
str(tapply)
tapply(mtcars$mpg, mtcars$cyl, mean)
tapply(mtcars$hp, mtcars$am, mean)



#cut: dividir las variables continuas para colocarlas en diferentes contenedores.
Orange
c1<-cut(Orange$age,breaks=4)
table(c1)

seq(100,2000,by=300)
c2<-cut(Orange$age,breaks=seq(100,2000,by=300))
table(c2)

#split: Esta función divide el conjunto de datos en grupos definidos por el argumento.
str(split)
c3<-split(Orange,Orange$age)
c3


#Funciones propias
myFunc<-function(){
  print("Mi primera funcion")
}
myFunc()


myFun<-function(num){
  for (i in seq_len(num)) {
    cat("My Function:",i,"\n")
  }
}
myFun(2)
# ==== Filtrar y resumir datos ====
library(sqldf)
sqldf("SELECT * FROM mtcars WHERE am=1 AND vs=1")
subset(mtcars, am==1 & vs==1)

identical(
  sqldf("SELECT * FROM mtcars WHERE am=1 AND vs=1", row.names=TRUE),
  subset(mtcars, am==1 & vs==1)
)

subset(mtcars, am==1 & vs==1, select=hp:wt)

#Eliminar datos de manera eficiente
library(hflights)
system.time(sqldf("SELECT * FROM hflights WHERE Dest=='BNA'", row.names = TRUE))
system.time(subset(hflights, Dest=='BNA'))

library(dplyr)
system.time(filter(hflights, Dest=='BNA'))
str(select(filter(hflights, Dest=='BNA'), DepTime:ArrTime))

#Los nombres de las filas no se conservan en dplyr, por lo que si los necesitas, vale la pena copiar los nombres en variables
#explícitas antes de pasarlas a dplyr o directamente a data.table

mtcars$rownames<-rownames(mtcars)
select(filter(mtcars,hp>300), c(rownames,hp))

library(data.table)
hflights_dt<-data.table(hflights)
hflights_dt[, rownames := rownames(hflights)]
system.time(hflights_dt[Dest=='BNA'])

str(hflights_dt[Dest=='BNA', list(DepTime, ArrTime)])
hflights_dt[Dest=='BNA', c('DepTime','ArrTime'), with=FALSE]

#Agregación
aggregate(hflights$Diverted, by=list(hflights$DayOfWeek), FUN=mean)
with(hflights, aggregate(Diverted, by=list(DayOfWeek), FUN=mean))
aggregate(Diverted ~ DayOfWeek, data=hflights, FUN=mean)

tapply(hflights$Diverted, hflights$DayOfWeek, mean)

library(plyr)
ddply(hflights, .(DayOfWeek), function(x) mean(x$Diverted))
ddply(hflights, .(DayOfWeek), summarise, Diverted=mean(Diverted))

library(dplyr)
hflights_DayOfWeek<-group_by(hflights,DayOfWeek)
str(attributes(hflights_DayOfWeek))

dplyr::summarise(hflights_DayOfWeek,mean(Diverted))
hflights_dt[, mean(Diverted), by=DayOfWeek]
setkey(hflights_dt, 'DayOfWeek')
hflights_dt[, mean(Diverted), by=DayOfWeek]

DPLYR_ALL<-function(){
  hflights_DayOfWeek<-group_by(hflights,DayOfWeek)
  dplyr::summarise(hflights_DayOfWeek,mean(Diverted))
}
hflights_dt_nokey<-data.table(hflights)
key(hflights_dt_nokey)

dplyr::summarise(group_by(hflights_dt, DayOfWeek), mean(Diverted))


#Resumir
ddply(hflights, .(DayOfWeek), summarise, n=length(Diverted))
ddply(hflights, .(DayOfWeek), nrow)
table(hflights$DayOfWeek)
count(hflights, 'DayOfWeek')
dplyr::summarise(hflights_DayOfWeek, n())
attr(hflights_DayOfWeek,'group_sizes')
hflights_dt[,.N, by=list(DayOfWeek)]




#xd