#Tarea de reglas - Identificación de combinaciones de cursos

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\15 Coursetopics.csv")

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


# ==== Reglas con RIPPER ====
library(RWeka)

#Convertir el (1,0) --> (Yes,No)
data[]<-lapply(data, function(x) as.factor(ifelse(x==1,"Yes","No")))
head(data)

mRip_intro <- JRip(Intro ~ ., data = data)
mRip_intro

mRip_dataMining <- JRip(DataMining ~ ., data = data)
mRip_dataMining

mRip_Survey <- JRip(Survey ~ ., data = data)
mRip_Survey

mRip_Cat.Data <- JRip(Cat.Data ~ ., data = data)
mRip_Cat.Data

mRip_Regression <- JRip(Regression ~ ., data = data)
mRip_Regression

mRip_Forecast <- JRip(Forecast ~ ., data = data)
mRip_Forecast

mRip_DOE <- JRip(DOE ~ ., data = data)
mRip_DOE

mRip_SW <- JRip(SW ~ ., data = data)
mRip_SW


#Evaluar el modelo
evaluate_model <- function(model, data, target) {
  pred <- predict(model, data)
  confusion_matrix <- table(Predicho = pred, Real = data[[target]])
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix) #Calcular precisión
  
  cat("Evaluación para", target, ":\n")
  print(confusion_matrix)
  cat("Precisión:", round(accuracy * 100, 2), "%\n")
  cat("-----------------------------------\n")
}

evaluate_model(mRip_intro, data, "Intro")
evaluate_model(mRip_dataMining, data, "DataMining")
evaluate_model(mRip_Survey, data, "Survey")
evaluate_model(mRip_Cat.Data, data, "Cat.Data")
evaluate_model(mRip_Regression, data, "Regression")
evaluate_model(mRip_Forecast, data, "Forecast")
evaluate_model(mRip_DOE, data, "DOE")
evaluate_model(mRip_SW, data, "SW")


#Graficar patrones clave
library(ggplot2)
library(reshape2)
library(igraph)

#Convertir a formato largo para ggplot2
co_occurrence_melt <- melt(co_occurrence)

#Mapa de calor
ggplot(co_occurrence_melt, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient(low = "white", high = "red") +
  theme_minimal() +
  labs(title = "Mapa de Calor de Co-ocurrencia de Cursos",
       x = "Curso 1", y = "Curso 2") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Grafo de red

diag(co_occurrence) <- 0 #Eliminar bucles (diagonal) | Evitar autoconexiones

graph_courses <- graph_from_adjacency_matrix(co_occurrence, mode = "undirected", weighted = TRUE)

#Asignar colores según el peso de las conexiones
E(graph_courses)$color <- ifelse(E(graph_courses)$weight > 20, "red",
                                 ifelse(E(graph_courses)$weight > 10, "orange", "gray"))

#Escalar el grosor de las líneas según la frecuencia
E(graph_courses)$width <- E(graph_courses)$weight / 3

plot(graph_courses,
     vertex.label = V(graph_courses)$name,
     vertex.label.color = "black",
     edge.width = E(graph_courses)$width,
     edge.color = E(graph_courses)$color,
     vertex.color = "skyblue",
     vertex.size = 25,
     main = "Red de Relaciones entre Cursos")



# ==== Árboles (C.50) ====
library(C50)

#Usar por si marca error:
#data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\15 Coursetopics.csv")
#data[]<-lapply(data, function(x) as.factor(ifelse(x==1,"Yes","No")))

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


# ==== Árboles (Rpart) ====
library(rpart)
library(rpart.plot)

#Usar por si marca error:
#data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\15 Coursetopics.csv")
#data[]<-lapply(data, function(x) as.factor(ifelse(x==1,"Yes","No")))

models_rpart <- list()

for (course in colnames(data)) {
  formula <- as.formula(paste(course, "~ ."))
  models_rpart[[course]] <- rpart(formula, data = data, method = "class", control = rpart.control(cp = 0.01))
}

evaluate_rpart_model <- function(model, data, target) {
  pred <- predict(model, data, type = "class")
  confusion_matrix <- table(Predicho = pred, Real = data[[target]])
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  
  cat("Evaluación del árbol (rpart) para", target, ":\n")
  print(confusion_matrix)
  cat("Precisión:", round(accuracy * 100, 2), "%\n")
  cat("-----------------------------------\n")
}

evaluate_rpart_model(models_rpart$Intro, data, "Intro")
evaluate_rpart_model(models_rpart$DataMining, data, "DataMining")
evaluate_rpart_model(models_rpart$Survey, data, "Survey")
evaluate_rpart_model(models_rpart$Cat.Data, data, "Cat.Data")
evaluate_rpart_model(models_rpart$Regression, data, "Regression")
evaluate_rpart_model(models_rpart$Forecast, data, "Forecast")
evaluate_rpart_model(models_rpart$DOE, data, "DOE")
evaluate_rpart_model(models_rpart$SW, data, "SW")

rpart.plot(models_rpart$Intro)










#xd