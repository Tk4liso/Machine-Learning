#Ejemplo Reglas de asociación - Identificación de productos de consumo frecuente con reglas de asociación 
library(arules)

groceries <- read.transactions("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\22 groceries.csv", sep = ",")
summary(groceries)

head(toLongFormat(groceries, decode=FALSE), n=7)
inspect(groceries[1:5]) #Inspeccionar las 5 primeras transacciones
itemFrequency(groceries[,1:3])
itemFrequencyPlot(groceries, support=0.1)
itemFrequencyPlot(groceries,topN=20)

image(groceries[1:5])
image(sample(groceries,100))

apriori(groceries)

groceryrules<-apriori(groceries,parameter = list(support=0.006, confidence=0.25, minlen=2))
groceryrules
summary(groceryrules)
inspect(groceryrules[1:3])

#Mejorar el modelo
inspect(sort(groceryrules, by="lift")[1:5]) #las cinco mejores reglas según la estadística de elevación 

#Tomando subconjuntos de reglas de asociación
berryrules<-subset(groceryrules, items %in% "berries") # encontrar cualquier regla que incluya bayas
inspect(berryrules)

write(groceryrules, file = "groceryrules.csv", sep=".", quote=TRUE, row.names=FALSE)
groceryrules_df <- as(groceryrules, "data.frame")
str(groceryrules_df)

#Algoritmo Eclat
groceryitemsets_eclat<-eclat(groceries, support=0.006)
inspect(groceryitemsets_eclat[1:5])

groceryrules_eclat<-ruleInduction(groceryitemsets_eclat, confidence=0.25) #Generar reglas a partir de los conjuntos de elementos
groceryrules_eclat

inspect(groceryrules_eclat[1:5])











#xd