#Tarea árboles de decisión - Subastas competitivas en eBay.com
library(ggplot2)
library(GGally)
library(caret)

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\12 eBayAuctions.csv")
head(data)
str(data)
summary(data)

#Variables categóricas
barplot(table(data$Category), main="Frecuencia de Categorías", col="blue", las=2)                    #¿Cuál es la categoría más popular?
barplot(table(data$endDay), main="Frecuencia de Subastas por Día de Finalización", col="red", las=2) #¿En qué días terminan más subastas?


#Seleccionar solo variables numéricas
numeric_data <- data[, sapply(data, is.numeric)]
ggpairs(numeric_data)


#Procesamiento de datos
data$Duration <- as.factor(data$Duration)

#0 = No competitiva, 1 = Competitiva
data$Competitive. <- as.factor(data$Competitive.)

data$Category <- as.factor(data$Category)
data$currency <- as.factor(data$currency)
data$endDay <- as.factor(data$endDay)

#Dividir los datos en 60%:40%
set.seed(1234)
trainIndex <- createDataPartition(data$Competitive., p = 0.6, list = FALSE)
train <- data[trainIndex, ]
test <- data[-trainIndex, ]

# ==== Inciso A ====
library(rpart)
library(rpart.plot)
library(rattle)

#Competitive = 1 : Competitiva
#Competitive = 0 : No competitiva

#method = "class" : Clasificación
#cp = 0 : Sin poda automática para podarlo manualmente después
tree<-rpart(Competitive. ~ ., data=train,
            method = "class",
            control = rpart.control(minbucket = 50, maxdepth = 7,cp=0))

tree
summary(tree)

#Graficar árbol sin podar
rpart.plot(tree, type = 3, extra = 101, fallen.leaves = TRUE, cex = 0.7)


#Mejor árbol podado

printcp(tree) #Tabla de complejidad del árbol no podado

optimal_cp <- tree$cptable[which.min(tree$cptable[, "xerror"]), "CP"] #Obtener mejor CP

#El mejor CP era de 0.000, pero veamos que tal el segundo mejor
pruned_tree1 <- prune(tree, cp = optimal_cp) #El CP óptimo (sin cambios = 0)
pruned_tree2 <- prune(tree, cp = 0.036765) #Probar con un CP mayor

#Graficar el árbol podado

#type = 3 → Muestra reglas en nodos internos y clases en hojas.
#extra = 101 → Muestra la clase, porcentaje de la clase dominante y el conteo de observaciones.
#fallen.leaves = TRUE → Mantiene los nodos terminales alineados abajo.
#cex = 0.7 → Reduce el tamaño del texto para mayor legibilidad.

rpart.plot(pruned_tree1, type = 3, extra = 101, fallen.leaves = TRUE, cex = 0.7)
rpart.plot(pruned_tree2, type = 3, extra = 101, fallen.leaves = TRUE, cex = 0.7)


#Mostrar reglas del árbol
rpart.rules(pruned_tree1)
rpart.rules(pruned_tree2)


# ==== Inciso D ====

#Convertimos la variable Category en variables dummy
library(caret)

dummies <- model.matrix(~ Category - 1, data = data)
data_dummy <- cbind(data, dummies)
data_dummy$Category <- NULL
head(data_dummy)

#Dividir los datos en 60%:40%
set.seed(1234)
trainIndex <- createDataPartition(data$Competitive., p = 0.6, list = FALSE)
train_dummy <- data[trainIndex, ]
test_dummy <- data[-trainIndex, ]

new_tree<-rpart(Competitive. ~ . - sellerRating - ClosePrice - Duration,
                data=train_dummy, 
                method = "class",
                control = rpart.control(minbucket = 50, maxdepth = 7,cp=0.03676471))

new_tree
summary(new_tree)

#Hallar el mejor CP para la poda
printcp(new_tree)
new_optimal_cp<-new_tree$cptable[which.min(new_tree$cptable[,"xerror"]),"CP"]

#Podar el árbol 
pruned_new_tree <- prune(new_tree, cp = new_optimal_cp)

rpart.plot(pruned_new_tree, type = 3, extra = 101, fallen.leaves = TRUE, cex = 0.7)

rpart.rules(pruned_new_tree)

#Mejorar el plot
rpart.plot(pruned_new_tree, 
           type = 3,                                             # Mostrar reglas dentro de los nodos
           extra = 104,                                          # Mostrar la clase y el número de observaciones
           under = TRUE,                                         # Información adicional debajo de los nodos
           tweak = 1.2,                                          # Ajustar tamaño del árbol
           cex = 0.9,                                            # Ajustar tamaño del texto
           faclen = 5,                                           # Acortar etiquetas de factores
           box.palette = c("red", "orange", "yellow", "green"),  # Mejor contraste de colores
           shadow.col = NULL,                                    # Eliminar sombras
           split.cex = 1.1,                                      # Tamaño del texto en las divisiones
           split.prefix = "¿", split.suffix = "?",               # Agregar signos de pregunta en los nodos de división
           split.box.col = "gray90",                             # Color más claro para nodos de decisión
           fallen.leaves = TRUE)                                 # Mantener alineados los nodos terminales



# ==== Inciso E ====
library(ggplot2)

printcp(pruned_new_tree) #Tabla de complejidad del árbol
summary(pruned_new_tree)


new_df <- train_dummy
new_df$CategoryNum <- as.numeric(new_df$Category) #Convertir `Category` en un factor numérico para el eje Y

#Crear el scatter plot | Category vs OpenPrice
ggplot(new_df, aes(x = OpenPrice, y = CategoryNum, color = as.factor(Competitive.))) +
  geom_point(alpha = 0.6, size = 3) +  # Puntos semi-transparentes para mejor visibilidad
  scale_color_manual(values = c("red", "blue"), labels = c("No Competitiva", "Competitiva")) +
  labs(title = "Diagrama de Dispersión: OpenPrice vs. Categoría",
       x = "Precio de Apertura (OpenPrice)",
       y = "Categoría (Codificada Numéricamente)",
       color = "Tipo de Subasta") +
  theme_minimal() +
  # Dibujar las líneas de decisión basadas en el árbol
  geom_vline(xintercept = 1.8, linetype = "dashed", color = "black", linewidth = 1) +  # Línea de OpenPrice >= 1.8
  geom_hline(yintercept = c(6.5), linetype = "dotted", color = "black", linewidth = 1) # Línea de corte en categorías según el árbol


#VERSIÓN 2 DEL PLOT - endDay vs OpenPrice

#endDay = Sat, Sun, Thu, Wed (Menos competitivo) : 3,4,5
#endDay = Fri, Mon, Tue (Más competitivo) : 1,2,6

new_df$endDayNum <- as.numeric(new_df$endDay)

ggplot(new_df, aes(x = OpenPrice, y = endDayNum, color = as.factor(Competitive.))) +
  geom_point(alpha = 0.6, size = 3) +  # Puntos semi-transparentes para mejor visibilidad
  scale_color_manual(values = c("red", "blue"), labels = c("No Competitiva", "Competitiva")) +
  labs(title = "Diagrama de Dispersión: OpenPrice vs. endDay",
       x = "Precio de Apertura (OpenPrice)",
       y = "Día de Cierre de la Subasta (Codificado Numéricamente)",
       color = "Tipo de Subasta") +
  theme_minimal() +
  geom_vline(xintercept = 1.8, linetype = "dashed", color = "black", linewidth = 1) + #Línea de decisión en OpenPrice según el árbol
  geom_hline(yintercept = 4.5, linetype = "dotted", color = "black", linewidth = 1)   #Línea de separación en endDay según el árbol


# ==== Inciso F ====
library(caret)

test_dummy$Competitive. <- as.factor(as.character(test_dummy$Competitive.))

pred <- predict(pruned_new_tree, newdata = test_dummy, type = "class")
confusionMatrix(pred, test_dummy$Competitive.) #Matríz de confusión

# --> Curva de elevación <--
library(gains)

test_dummy$Competitive. <- as.numeric(as.character(test_dummy$Competitive.))

#Obtener probabilidades de predicción para la clase "1"
prob <- predict(pruned_new_tree, newdata = test_dummy, type = "prob")[,2]

gain <- gains(test_dummy$Competitive., prob) #Crear la curva de elevación

plot(c(0, gain$cume.pct.of.total * sum(test_dummy$Competitive.)) ~ 
       c(0, gain$cume.obs), type = "l",
     xlab = "Número de casos",
     ylab = "Subastas competitivas acumuladas",
     main = "Curva de Elevación",
     col = "blue", lwd = 2)

abline(0, sum(test_dummy$Competitive.) / nrow(test_dummy), col = "red", lty = 2) #Agregar línea base (modelo aleatorio)












#xd