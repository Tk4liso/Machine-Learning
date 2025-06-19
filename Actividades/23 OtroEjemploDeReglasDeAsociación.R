#Otro ejemplo de reglas de asociación - Combinaciones de cursos (pero con reglas de asociación)
#Como usamos el mismo dataset que en la actividad 15 retomo el mismo EDA
#NOTAS: hay otro algoritmo de reglas llamado FP-Growth

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\23 Coursetopics.csv")

# ==== EDA (análisis exploratorio de datos) ====
library(ggplot2)
library(GGally)

sum(is.na(data))

head(data)
str(data)
summary(data)

ggpairs(data)
cor(data)

#Distribución de los cursos tomados
counts<-colSums(data)
ggplot(data.frame(Course = names(counts), Count = counts), 
       aes(x = reorder(Course, -Count), y = Count)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() + # Poner los cursos en vertical
  theme_minimal() +
  labs(title = "Distribución de cursos tomados", x = "Cursos", y = "Número de estudiantes")

#Matriz de co-ocurrencia (Para estudiar qué cursos aparecen con mayor frecuencia juntos en un mismo estudiante)
co_occurrence <- t(data) %*% as.matrix(data)
co_occurrence

# ==== Tratamiento de los datos ====

#Convertir el (1,0) --> (Yes,No)
data[]<-lapply(data, function(x) as.factor(ifelse(x==1,"Yes","No")))
head(data)


# ==== Reglas a priori ====
library(arules)

#Convertir los datos a formato de transacciones
data_trans <- as(data, "transactions")
head(data_trans)

rules <- apriori(data_trans,parameter = list(supp = 0.05, conf = 0.6, minlen = 2))

summary(rules)

inspect(sort(rules, by = "confidence")[1:10])  #Las 10 reglas más relevantes ordenadas por confianza
inspect(sort(rules, by = "lift")[1:10])        #Las 10 reglas con mayor lift
inspect(sort(rules, by = "support")[1:10])     #Las 10 reglas más frecuentes

#Filtrado
rules_filtered <- subset(rules, lift > 1.2 & confidence > 0.7)
inspect(rules_filtered[1:10])

rules_yes <- subset(rules, rhs %pin% "=Yes") #Mantener las reglas donde el RHS tenga al menos un curso con "Yes"
rules_no_redundant <- subset(rules_yes, !all(lhs %pin% "=No")) #Eliminar reglas redundantes (donde todos los de lhs son "No")
length(rules_no_redundant)

#inspect(sort(rules_no_redundant, by = "lift")[1:10])
#inspect(sort(rules_no_redundant, by = "support")[1:10])

#rules_final <- subset(rules_no_redundant, confidence > 0.7)
#inspect(rules_final[1:10])

# ---> Mejorar las reglas <---

itemLabels(data_trans)
rules_v2 <- apriori(data_trans, 
                    parameter = list(support = 0.7, confidence = 0.6, minlen = 2),
                    appearance = list(rhs = c("Intro=Yes", "DataMining=Yes","Survey=Yes","Cat.Data=Yes","Regression=Yes","Forecast=Yes","DOE=Yes","SW=Yes")))

summary(rules_v2)
inspect(rules_v2)



#Ploteos
library(arulesViz)

#Visualizar las reglas de asociación (gráfico de red)
plot(rules, method = "graph", engine = "htmlwidget")

#Gráfico de dispersión (soporte vs confianza)
plot(rules, method = "scatterplot", measure = c("support", "confidence"), shading = "lift")


# ==== Árboles (C.50) ====
library(C50)

#Usar por si marca error:
#data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\15 Coursetopics.csv")
data[]<-lapply(data, function(x) as.factor(ifelse(x==1,"Yes","No")))

models_c50 <- list()

# Generar un árbol para cada curso
for (course in colnames(data)) {
  formula <- as.formula(paste(course, "~ ."))
  models_c50[[course]] <- C5.0(formula, data = data)
}

# Evaluar y mostrar la precisión de los árboles
evaluate_c50_model <- function(model, data, target) {
  pred <- predict(model, data)
  confusion_matrix <- table(Predicho = pred, Real = data[[target]])
  
  # Calcular precisión
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  cat("Evaluación del árbol para", target, ":\n")
  print(confusion_matrix)
  cat("Precisión:", round(accuracy * 100, 2), "%\n")
  cat("-----------------------------------\n")
}

evaluate_c50_model(models_c50$Intro, data, "Intro")
evaluate_c50_model(models_c50$DataMining, data, "DataMining")
evaluate_c50_model(models_c50$Survey, data, "Survey")
evaluate_c50_model(models_c50$Cat.Data, data, "Cat.Data")
evaluate_c50_model(models_c50$Regression, data, "Regression")
evaluate_c50_model(models_c50$Forecast, data, "Forecast")
evaluate_c50_model(models_c50$DOE, data, "DOE")
evaluate_c50_model(models_c50$SW, data, "SW")

# Visualizar árboles de decisión
plot(models_c50$Intro)
plot(models_c50$DataMining)
plot(models_c50$Survey)
plot(models_c50$Cat.Data)
plot(models_c50$Regression)
plot(models_c50$Forecast)
plot(models_c50$DOE)
plot(models_c50$SW)







#xd