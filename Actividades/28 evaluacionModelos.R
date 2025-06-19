#Proyecto - Evaluación de un modelo de machine learning
#Taisen Romero Bañuelos (202055209) 20/03/2025

#1) Sensation-Seeking, Risk-Taking, and Problematic FinancialBehaviors of College Students

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\28 overdrawn.csv")
head(data)


# ==== Análisis exploratorio de los datos (EDA) ====
library(ggplot2)

# ---> Limpieza de datos
sum(is.na(data))
colSums(is.na(data))
data[!complete.cases(data), ]
str(data) #14 observaciones no deberían afectar tanto al total (450)
data <- na.omit(data)
sum(is.na(data))


head(data)
str(data)
summary(data)

data<-subset(data, select = -X) #Eliminar X porque sólo es un ID
head(data)
sum(is.na(data))


# ---> Ahora si, el EDA
plot(data)

boxplot(data)
boxplot(data$DaysDrink)

hist(data$Age)
hist(data$Sex) # male=0 | female=1
hist(data$DaysDrink)
hist(data$Overdrawn) # Saldo negativo: no=0 | yes=1

# Matríz de correlación
library(ggplot2)
library(GGally)
library(tidyverse)

cor_data <- data %>% select(Age, Sex, DaysDrink, Overdrawn)
cor_matrix <- cor(cor_data, use = "complete.obs")
cor_matrix

ggpairs(data)


#Hostogramas pero bonito
#Edad
ggplot(data, aes(x = Age)) +
  geom_histogram(fill = "steelblue", bins = 10, color = "black") +
  labs(title = "Distribución de la edad", x = "Edad", y = "Frecuencia") +
  theme_minimal()

#DaysDrink - Distribución
ggplot(data, aes(x = DaysDrink)) +
  geom_histogram(fill = "darkred", bins = 15, color = "black") +
  labs(title = "Distribución de los días de consumo de alcohol", x = "Días de Consumo", y = "Frecuencia") +
  theme_minimal()

#DaysDrink por Sexo - Boxplot
ggplot(data, aes(x = as.factor(Sex), y = DaysDrink, fill = as.factor(Sex))) +
  geom_boxplot() +
  labs(title = "Días de consumo de alcohol por sexo", x = "Sexo (0=M, 1=F)", y = "Días de consumo") +
  theme_minimal() +
  scale_fill_manual(values = c("lightblue", "pink"))

#Distribución de edad por sobregiro
ggplot(data, aes(x = Age, fill = as.factor(Overdrawn))) +
  geom_density(alpha = 0.5) +
  labs(title = "Distribución de edad por sobregiro", x = "Edad", fill = "Overdrawn") +
  theme_minimal()


#Proporción de sobregiro por sexo
library(dplyr)

prop_sobregiro <- data %>%
  group_by(Sex) %>%
  summarise(Proporcion = mean(as.numeric(as.character(Overdrawn)) == 1))

ggplot(prop_sobregiro, aes(x = as.factor(Sex), y = Proporcion, fill = as.factor(Sex))) +
  geom_col() +
  geom_text(aes(label = scales::percent(Proporcion, accuracy = 0.1)), 
            vjust = -0.5, size = 4) +
  labs(title = "Proporción de sobregiros por sexo",
       x = "Sexo (0 = Hombre, 1 = Mujer)", y = "Proporción de sobregiro") +
  scale_fill_manual(values = c("lightblue", "pink")) +
  theme_minimal()



# - Transformar a variables categóricas según la recomendación del PDF -
library(dplyr)

data <- data %>%
  mutate(DaysDrink_Categ = case_when(
    DaysDrink < 7 ~ "Pocos",
    DaysDrink >= 7 & DaysDrink < 14 ~ "Moderado",
    DaysDrink >= 14 ~ "Alto"
  ))

head(data)
table(data$DaysDrink_Categ)

table(data$Overdrawn)
prop.table(table(data$Overdrawn))

#Calcular proporcion de nivel de consumo de alcohol por sexo
prop_consumo_sexo <- data %>%
  group_by(Sex, DaysDrink_Categ) %>%
  summarise(n = n()) %>%
  mutate(proporcion = n / sum(n))  #prop. de cada sexo

prop_consumo_sexo

ggplot(prop_consumo_sexo, aes(x = DaysDrink_Categ, y = proporcion, fill = as.factor(Sex))) +
  geom_col(position = "dodge") +
  labs(title = "Proporción de niveles de consumo por sexo",
       x = "Nivel de consumo", y = "Proporción",
       fill = "Sexo (0 = Hombre, 1 = Mujer)") +
  scale_fill_manual(values = c("lightblue", "pink")) +
  theme_minimal()


# - Prueba de Chi-cuadrado (buscar asociaciones estadísticamente significativas con Overdrawn)

#Age
chi_table_age <- table(data$Age, data$Overdrawn)
chisq.test(chi_table_age)

#Sex
chi_table_sex <- table(data$Sex, data$Overdrawn)
chisq.test(chi_table_sex)

#DaysDrink_Categ
chi_table_drink <- table(data$DaysDrink_Categ, data$Overdrawn)
chisq.test(chi_table_drink)


#Dist. DaysDrink - en general
ggplot(data, aes(x = DaysDrink_Categ, fill = DaysDrink_Categ)) +
  geom_bar() +
  labs(title = "Distribución del consumo de alcohol", x = "Nivel de consumo", y = "Frecuencia") +
  theme_minimal()



# - Proporciones de variables vs sobregiro -

#Edad vs Overdrawn
ggplot(data, aes(x = Age, fill = as.factor(Overdrawn))) +
  geom_histogram(position = "dodge", bins = 10, color = "black") +
  labs(title = "Distribución de edad por sobregiro", x = "Edad", y = "Frecuencia") +
  scale_fill_manual(values = c("gray", "red"), name = "Overdrawn", labels = c("No", "Sí")) +
  theme_minimal()

#DaysDrink_Categ vs Overdrawn
ggplot(data, aes(x = DaysDrink_Categ, fill = as.factor(Overdrawn))) +
  geom_bar(position = "fill") +  # Fill muestra proporciones
  labs(title = "Sobregiro según nivel de consumo de alcohol", x = "Nivel de Consumo", y = "Proporción") +
  scale_fill_manual(values = c("gray", "red"), name = "Overdrawn", labels = c("No", "Sí")) +
  theme_minimal()

#Sexo vs Overdrawn
ggplot(data, aes(x = as.factor(Sex), fill = as.factor(Overdrawn))) +
  geom_bar(position = "fill") +
  labs(title = "Proporción de sobregiro por sexo", x = "Sexo (0=M, 1=F)", y = "Proporción") +
  scale_fill_manual(values = c("gray", "red"), name = "Overdrawn", labels = c("No", "Sí")) +
  theme_minimal()

#Consumo & Sexo vs Overdrawn
ggplot(data, aes(x = DaysDrink_Categ, fill = as.factor(Overdrawn))) +
  geom_bar(position = "fill") +
  facet_wrap(~ Sex, labeller = labeller(Sex = c("0" = "Hombre", "1" = "Mujer"))) +
  labs(title = "Sobregiro según consumo y sexo", x = "Consumo", y = "Proporción") +
  theme_minimal()



# ==== Creación y entrenamiento de modelos ====
library(dplyr)
library(caret)
library(rpart)
library(rpart.plot)

#Eliminar DaysDrink no categ.
data<-subset(data, select = -DaysDrink)
head(data)

#Convertir a factor
data$Overdrawn <- factor(
  ifelse(data$Overdrawn == 1, "Sí", "No"),
  levels = c("No", "Sí"))

data$Sex <- factor(data$Sex, levels = c(0, 1), labels = c("Hombre", "Mujer"))
data$DaysDrink_Categ <- factor(data$DaysDrink_Categ, levels = c("Pocos", "Moderado", "Alto"))


str(data)
sum(is.na(data))


# ---> Entrenamiento

# - - - - - Árbol de decisión 70:30 - - - - - 
set.seed(123)
train_idx <- createDataPartition(data$Overdrawn, p = 0.7, list = FALSE)
train_1 <- data[train_idx, ]
test_1 <- data[-train_idx, ]

tree_70 <- rpart(Overdrawn ~ ., data = train_1, method = "class", 
                 control = rpart.control(cp = 0.0001, minsplit = 5))

summary(tree_70)
tree_70$variable.importance
rpart.plot(tree_70, type = 4, extra = 104, box.palette = "GnBu", shadow.col = "gray", nn = TRUE, cex = 0.6, fallen.leaves = TRUE, branch.lty = 1)


# - Poda (OPCIONAL) -
printcp(tree_70)  # Tabla de complejidad
plotcp(tree_70)   # Gráfica: identifica el cp óptimo

opt_cp <- tree_70$cptable[which.min(tree_70$cptable[, "xerror"]), "CP"] #Podar con el cp óptimo
pruned_tree <- prune(tree_70, cp = opt_cp)
rpart.plot(pruned_tree, type = 4, extra = 104, box.palette = "GnBu", shadow.col = "gray", nn = TRUE, cex = 0.6, fallen.leaves = TRUE, branch.lty = 1)



# - - - - - Árbol de decisión 50:25:25 - - - - - 
set.seed(456)
n <- nrow(data)
idx <- sample(1:n, n)
train_idx <- idx[1:round(0.5 * n)]
test_idx  <- idx[(round(0.5 * n) + 1):(round(0.75 * n))]
eval_idx  <- idx[(round(0.75 * n) + 1):n]

train_2 <- data[train_idx, ]
test_2  <- data[test_idx, ]
eval_2  <- data[eval_idx, ]

tree_50 <- rpart(Overdrawn ~ ., data = train_2, method = "class", 
                 control = rpart.control(cp = 0.0001, minsplit = 5))

summary(tree_50)
tree_50$variable.importance

rpart.plot(tree_50, type = 4, extra = 104, box.palette = "GnBu", shadow.col = "gray", cex = 0.6)



# - - - - - Árbol de decisión - Bootstrap (100 iteraciones) - - - - - 
#Reutilizamos la partición de datos del modelo 1

train_control_boot <- trainControl(method = "boot", number = 100, classProbs = TRUE, summaryFunction = twoClassSummary,savePredictions = TRUE)

#Modelo
model_bootstrap <- train(
  Overdrawn ~ ., data = train_1,
  method = "rpart",
  trControl = train_control_boot,
  metric = "ROC")

model_bootstrap
summary(model_bootstrap)

prob_bootstrap <- predict(model_bootstrap, newdata = test_1, type = "prob")[, "Sí"]
pred_bootstrap <- predict(model_bootstrap, newdata = test_1)



# ==== Evaluación ====
library(pROC)
library(MLmetrics)
library(irr)

#Evaluar los modelos con splits normales
eval_model <- function(model, test_data, model_name = "") {
  #Predicciones
  probs <- predict(model, newdata = test_data, type = "prob")[, "Sí"]
  preds <- predict(model, newdata = test_data, type = "class")
  
  #Kappa
  kappa_df <- data.frame(obs = test_data$Overdrawn, pred = preds)
  kappa <- kappa2(kappa_df)$value
  
  # ROC AUC
  roc_obj <- roc(test_data$Overdrawn, probs, levels = c("No", "Sí"))
  auc_val <- auc(roc_obj)
  
  cat("──", model_name, "──\n")
  cat("Kappa:", round(kappa, 3), "\n")
  cat("AUC:", round(auc_val, 3), "\n\n")
  
  #Plotear curva ROC
  plot(roc_obj, main = paste("ROC -", model_name), col = "blue")
}


# - - - - Arbol 70:30 - - - - 
eval_model(tree_70, test_1, model_name = "Árbol 70/30")


# - - - - Arbol 50:25:25 - - - - 
eval_model(tree_50, eval_2, model_name = "Árbol 50/25/25")


# - - - - Bootstrap - - - - 
#ROC AUC
roc_boot <- roc(test_1$Overdrawn, prob_bootstrap, levels = c("No", "Sí"))
plot(roc_boot, main = "ROC - Árbol con bootstrap", col = "blue")

#Kappa
kappa_boot <- kappa2(data.frame(obs = test_1$Overdrawn, pred = pred_bootstrap))$value

cat("── Árbol con Bootstrap ──\n")
cat("Kappa:", round(kappa_boot, 3), "\n")
cat("AUC:", round(auc(roc_boot), 3), "\n")



#Vainas comparativas

resultados_modelos <- data.frame(
  Modelo = c("Árbol 70:30", "Árbol 50:25:25", "Árbol Bootstrap"),
  Kappa = c(0.264, 0.062, 0),
  AUC = c(0.78, 0.582, 0.5))

resultados_modelos

resultados_largos <- pivot_longer(resultados_modelos, cols = c(Kappa, AUC),
                                  names_to = "Métrica", values_to = "Valor")

ggplot(resultados_largos, aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0, 1) +
  labs(title = "Comparación de desempeño entre modelos",
       x = "Modelo", y = "Valor de la métrica") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal()


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
#El mejor modelo fue el 70:30, así que haré regresión logística para comparar
#Para predecir si un estudiante ha sobregirado su cuenta (Overdrawn = "Sí"/"No"), es mejor usar regresión logística.

#Usaré el mismo split 70:30 que usé para el Modelo 1
str(train_1)
str(test_1)

model_logit <- glm(Overdrawn ~ .,
                  data = train_1,
                  family = "binomial")

prob_logit <- predict(model_logit, newdata = test_1, type = "response")
pred_logit <- factor(ifelse(prob_logit > 0.5, "Sí", "No"), levels = c("No", "Sí")) #Ajustar el umbral de corte según kappa (0.5 -> k=0. Todas las pred. fueron "No") | Probar con 0.3
pred_logit

#Kappa
kappa_logit <- kappa2(data.frame(obs = test_1$Overdrawn, pred = pred_logit))$value

#AUC (ROC)
roc_logit <- roc(test_1$Overdrawn, prob_logit, levels = c("No", "Sí"))
auc_logit <- auc(roc_logit)

plot(roc_logit, main = "ROC - Regresión Logística (70:30)", col = "darkgreen")

cat("── Regresión Logística (70:30) ──\n")
cat("Kappa:", round(kappa_logit, 3), "\n")
cat("AUC:", round(auc_logit, 3), "\n")

summary(model_logit)


# ---> Probar con más umbrales
umbrales <- seq(0.1, 0.9, by = 0.05)
kappas <- sapply(umbrales, function(thresh) {
  pred_bin <- factor(ifelse(prob_logit > thresh, "Sí", "No"), levels = c("No", "Sí"))
  kappa2(data.frame(obs = test_1$Overdrawn, pred = pred_bin))$value
})
plot(umbrales, kappas, type = "b", col = "darkred", pch = 16,
     xlab = "Umbral de corte", ylab = "Kappa", main = "Kappa vs Umbral")


# ---> Modelo con umbral de corte óptimo (aprox. 0.18)
corte_optimo <- 0.18 #Lo pongo en un objeto para que sea más explícito que este es el bueno

pred_logit_opt <- factor(ifelse(prob_logit > corte_optimo, "Sí", "No"), levels = c("No", "Sí"))

kappa_opt <- kappa2(data.frame(obs = test_1$Overdrawn, pred = pred_logit_opt))$value

roc_log_opt <- roc(test_1$Overdrawn, prob_logit, levels = c("No", "Sí")) #Se usa el prob_logit del modelo anterior porque ROC evalúa el desempeño del modelo a través de todos los posibles umbrales (de 0 a 1)
auc_opt <- auc(roc_log_opt)
plot(roc_log_opt, main = "ROC - Reg. Logística (70:30) - Umbral de corte óptimo", col = "darkgreen")


cat("── Regresión Logística (umbral óptimo ≈ 0.18) ──\n")
cat("Kappa:", round(kappa_opt, 3), "\n")
cat("AUC:", round(auc_opt, 3), "\n")


# ---> Comparaciones finales

resultados_modelos <- data.frame(
  Modelo = c("Árbol 70:30", "Árbol 50:25:25", "Árbol Bootstrap", "Reg. Logística (umbral óptimo)"),
  Kappa = c(0.264, 0.062, 0, 0.228),
  AUC = c(0.78, 0.582, 0.5, 0.789)
)
resultados_modelos


resultados_largos <- pivot_longer(resultados_modelos, cols = c(Kappa, AUC),
                                  names_to = "Métrica", values_to = "Valor")

ggplot(resultados_largos, aes(x = Modelo, y = Valor, fill = Métrica)) +
  geom_bar(stat = "identity", position = "dodge") +
  ylim(0, 1) +
  labs(title = "Comparación de desempeño entre modelos",
       x = "Modelo", y = "Valor de la métrica") +
  scale_fill_brewer(palette = "Set1") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 15, hjust = 1))







# ==== PIPELINES MODULARES | Pruebas ====

#Crear pipelines (modulares) para las diferentes divisiones de datos (para que en futuras tareas reutilice el código)
split_70_30 <- function(df, target, prop = 0.7, seed = 42) {
  set.seed(42)
  
  #target debe ser una cadena de texto | Es la var. a predecir
  idx <- createDataPartition(df[[target]], p = prop, list = FALSE)
  
  train <- df[idx, ]
  test <- df[-idx, ]
  
  return(list(train = train, test = test))
}


split_50_25_25 <- function(df) {
  set.seed(42)
  n <- nrow(df)
  idx <- sample(seq_len(n))
  
  n_train <- floor(0.5 * n)
  n_val <- floor(0.25 * n)
  
  train <- df[idx[1:n_train], ]
  val   <- df[idx[(n_train + 1):(n_train + n_val)], ]
  test  <- df[idx[(n_train + n_val + 1):n], ]
  
  return(list(train = train, val = val, test = test))
}


split_bootstrap <- function(df, n = 100) {
  set.seed(42)
  boot_samples <- vector("list", n)
  
  for (i in 1:n) {
    idx <- sample(seq_len(nrow(df)), replace = TRUE)
    boot_samples[[i]] <- df[idx, ]
  }
  
  return(boot_samples)  # cada elemento es un dataset bootstrap
}







#xd