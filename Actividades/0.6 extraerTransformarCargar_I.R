#Extraer, transformar y analizar 

bike<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0.6 bike_sharing_data.csv")
str(bike)
summary(bike)

#Importación de datos desde bases de datos relacionales 
library(RODBC)
connection <- odbcConnect(dsn = "ourDB", uid = "Paul", pwd = "R4BI") 
query <- "SELECT * FROM marketing"
bike_rodbc <- sqlQuery(connection, query)
close(connection)

#Filtrado de filas
library(dplyr)
extracted_rows<-filter(bike,registered==0, season==1|season==2)
dim(extracted_rows)

using_membership<-filter(bike,registered==0,season %in% c(1,2))
identical(extracted_rows, using_membership)

#selección de columnas de datos
extracted_columns<-select(extracted_rows, season, casual)

#Agregar una columna calculada a partir de datos existentes
add_revenue<-mutate(extracted_columns, revenue=casual*5)
add_revenue

#Agregar datos en grupos 
grouped<-group_by(add_revenue,season)
report<-summarise(grouped,sum(casual),sum(revenue))
report

#Escritura de datos en un CSV (guardar el data frame)
write.csv(report,"revenue_report.csv",row.names = FALSE)

#Escritura de datos en un archivo de texto delimitado por tabulaciones
write.table(report,"revenue_report.txt",row.names = FALSE, sep="\t")






#xd