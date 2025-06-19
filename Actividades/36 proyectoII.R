#24/04/2025

#Proyecto II 
#3. Loan approval dataset (loan_data.csv)
#Alumno: Taisen Romero Bañuelos (202055209).


# ==== Knowledge Discovery in Databases (KDD) ====
#1. Data Selection
#2. Data Cleaning and Preprocessing
#3. Data Transformation and Reduction
#4. Data Mining
#5. Evaluation and Interpretation of Results

#Nota: luego del paso 3 pasar al EDA para un mejor flujo.

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\36 loan_data.csv")
head(data)
#View(data)
str(data)
summary(data) 
# |
# |--> Hay o posiblemente hay outliners: person_age; person_emp_exp; person_income;
# |--> loan_status tiene una media de 0.222, lo que indica que sólo el 22% de los casos son aprobados, por lo que está desbalanceado (habría que confirmar con un conteo)
# |--> No hay NA's
# |--> loan_percent_income: alcanza un 30, lo que puede implicar errores o extremos válidos pero inusuales.
# |
# |--> Codificación one-hot, label encoding o as.factor (person_gender, education, loan_intent, home_ownership)
# |--> Convertir a 0 (No) / 1 (Yes) (previous_loan_defaults_on_file)
# |--> Escalar o normalizar. Tentativo ya que posiblemente use XGBoost y Random Forest. (loan_percent_income, int_rate, credit_score)


# ----> Conversión de variables categóricas a factor
data$person_gender <- as.factor(data$person_gender)
data$person_education <- as.factor(data$person_education)
data$person_home_ownership <- as.factor(data$person_home_ownership)
data$loan_intent <- as.factor(data$loan_intent)

data$previous_loan_defaults_on_file <- ifelse(data$previous_loan_defaults_on_file == "Yes", 1, 0)
data$previous_loan_defaults_on_file <- as.factor(data$previous_loan_defaults_on_file)

data$loan_status <- as.factor(data$loan_status)
data$loan_status <- factor(data$loan_status, levels = c(0,1), labels = c("Rechazado", "Aprobado"))

str(data)


# ----> Tratamiento de outliers
library(ggplot2)
library(tidyverse)

#Seleccionar solo las variables numéricas
numeric_vars <- data %>%
  select(where(is.numeric))

boxplot(numeric_vars)
boxplot(numeric_vars$loan_amnt)  #Muchos valores cercanos al tope (35,000) son frecuentes, pero no necesariamente incorrectos (puede ser el máximo permitido por política).
boxplot(numeric_vars$cb_person_cred_hist_length)

#Convertir a formato largo
datos_largos <- numeric_vars %>%
  pivot_longer(cols = everything(), names_to = "variable", values_to = "valor")

ggplot(datos_largos, aes(x = "", y = valor)) +
  geom_boxplot(fill = "lightblue", outlier.color = "red", outlier.size = 1.5) +
  facet_wrap(~ variable, scales = "free") +
  theme_minimal() +
  labs(x = "", y = "Valor", title = "Boxplots de variables numéricas") +
  theme(strip.text = element_text(face = "bold"),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())


# -> Filtro/recorte basado en valores realistas. No es necesario para modelos de árboles pero podemos beneficiarnos de ello.
# Este elimina los que no pasan el filtro | | data <- subset(data,person_age <= 100 & person_emp_exp <= 80 & person_income < 1e6 & loan_percent_income <= 1.0 & cb_person_cred_hist_length <= 25)

data$person_age <- pmin(data$person_age, 100)
data$person_emp_exp <- pmin(data$person_emp_exp, 80)
data$person_income <- pmin(data$person_income, 1e6)
data$loan_percent_income <- pmin(data$loan_percent_income, 1.0)
data$cb_person_cred_hist_length <- pmin(data$cb_person_cred_hist_length, 25)

sum(is.na(data))
str(data)


# - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - -
#TRATAMIENTO DE DATOS PARA UN MODELO DE REGRESIÓN (se usará para el stacking)
# - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - -

# - Los outliers observados y el desbalance pueden afectar sobre todo al modelo de regresión, así que hagamos DF específico para regresión -
data_reg <- data

# -> One-hot encoding para regresión <-
library(caret)
data_reg$loan_status <- as.factor(data_reg$loan_status)

#Guardar variable objetivo
loan_status_target <- data_reg$loan_status

#Crear variables dummy (excluye loan_status automáticamente)
dummies <- dummyVars(loan_status ~ ., data = data_reg) 
data_reg <- data.frame(predict(dummies, newdata = data_reg))

data_reg$loan_status <- loan_status_target # Voler a incluir la variable objetivo

sum(is.na(data_reg))
str(data_reg)

# -> Normalización (z-score) <-
predictors <- data_reg[, setdiff(names(data_reg), "loan_status")]
predictors_scaled <- as.data.frame(scale(predictors))
data_reg_z <- cbind(predictors_scaled, loan_status = data_reg$loan_status)

sum(is.na(data_reg_z))
str(data_reg_z)

boxplot(data_reg_z)
summary(data_reg_z) #Todavía hay outliers, los máximos se alejan mucho de la media


# -> Escalado (min-max) para ver si trata mejor los outliers <-

min_max_scaler <- function(x) {
  return((x - min(x)) / (max(x) - min(x)))
}

#Aplicar a todas las columnas predictoras
predictors <- data_reg[, setdiff(names(data_reg), "loan_status")]
predictors_scaled <- as.data.frame(lapply(predictors, min_max_scaler))

# Unir con la variable objetivo
data_reg <- cbind(predictors_scaled, loan_status = data_reg$loan_status)

sum(is.na(data_reg))
str(data_reg)
boxplot(data_reg)
summary(data_reg)


# - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - -
# - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - - - - - - - - - -  - - - - - - - - - - - - - -



# ==== Análisis exploratorio de datos (EDA) ====
#Estructura/preguntas de investigación:
#A) Estadísticas básicas (descriptivas):
#   - Distribuciones (edad, ingresos, score crediticio...)
#   - Medidas de tendencia central y dispersión (mean, std, min, max...)
#   - Balanceo de clases (proporción de loan_status)
#   - ¿Qué % de ingreso se pide en préstamo (loan_percent_income)?
#
#B) Ploteos:
#   - Histogramas para variables numéricas
#   - Boxplots (detectar outliners)
#   - Matriz de correlación para variables numéricas
#   - Gráficas de pares (pairplot) para identificar clusters o relaciones
#   - Gráficos de densidad o violin plot (comparar score de aprobados vs rechazados)
#
#C) Exploración multivariada:
#   - ¿Cómo influye la interacción entre score de crédito y % de ingreso en el préstamo?
#   - ¿Se aprueban más préstamos con ciertos propósitos (loan_intent)?
#   - ¿Qué combinaciones de educación y home_ownership tienden a correlacionarse con aprobación?
#
#D) Fuera de situación:
#   - ¿Cómo influye la experiencia laboral y la educación en el estado financiero?



# ----> Histogramas
library(ggplot2)
library(tidyverse)

summary(data)
str(data)

numeric_vars <- data %>%
  select(where(is.numeric))
str(numeric_vars)

hist(numeric_vars$person_age)
hist(numeric_vars$person_emp_exp)

#Histogramas en plot facetado
numeric_long <- numeric_vars %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Valor")

ggplot(numeric_long, aes(x = Valor)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "white") +
  facet_wrap(~ Variable, scales = "free", ncol = 3) +
  labs(title = "Distribuciones de variables numéricas",
       x = "Valor", y = "Frecuencia") +
  theme_minimal()

#Distribución del Score Crediticio por Estado del Préstamo
ggplot(data, aes(x = credit_score, fill = loan_status)) +
  geom_histogram(position = "identity", alpha = 0.5, bins = 30) +
  labs(title = "Distribución del score crediticio por estado del préstamo",
       x = "Credit Score", y = "Frecuencia") +
  theme_minimal()


#Boxplot por clase objetivo
ggplot(data, aes(x = loan_status, y = loan_percent_income, fill = loan_status)) +
  geom_boxplot() +
  labs(title = "Porcentaje del ingreso vs estado del préstamo",
       x = "Loan Status", y = "Loan % Income") +
  theme_minimal()


# loan intent
ggplot(data, aes(x = loan_intent, fill = loan_status)) +
  geom_bar(position = "fill") +
  labs(title = "Propósito del préstamo según aprobación",
       x = "Loan Intent", y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# educación
ggplot(data, aes(x = person_education, fill = loan_status)) +
  geom_bar(position = "fill") +
  labs(title = "Nivel educativo según aprobación",
       x = "Nivel educativo", y = "Proporción") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


#Matriz de correlación
library(GGally)
library(corrplot)

#ggpairs(data)
#ggpairs(numeric_vars)

cor(numeric_vars)
corrplot(cor(numeric_vars), method = "color", type = "upper", tl.cex = 0.8)

#Mostrar la matriz en consola, redondeada a 3 decimales
matrix_cor <- cor(numeric_vars)
matrix_cor
print(round(matrix_cor, 3))



# ==== Modelado ====
library(caret)
library(randomForest)
library(xgboost)

#Configurar control de entrenamiento para k-Fold CV
control_cv <- trainControl(
  method = "cv",              
  number = 10,                
  classProbs = TRUE,          #Calcular probabilidades
  summaryFunction = defaultSummary,
  savePredictions = "final"   
)

# ----> Random Forest

#Entrenamiento
set.seed(123)
modelo_rf_cv <- train(
  loan_status ~ ., 
  data = data,
  method = "rf",
  trControl = control_cv,
  metric = "Kappa",              #Métrica principal
  importance = TRUE,             #Calcular importancia de variables
  tuneLength = 1,
  ntree = 200
)


# ----> XGBoost

#Preparar los datos
X <- model.matrix(loan_status ~ . -1, data = data)  #Matriz de predictores (sin intercepto)
y <- data$loan_status                              #Variable objetivo (factor)

data_xgb <- data.frame(X)
data_xgb$loan_status <- y

#Entrenamiento
set.seed(123)
modelo_xgb_cv <- train(
  loan_status ~ ., 
  data = data_xgb,
  method = "xgbTree",
  trControl = control_cv,
  metric = "Kappa"
)


# ----> Comparación de los modelos
resamples_list <- resamples(list(RandomForest = modelo_rf_cv, XGBoost = modelo_xgb_cv))
summary(resamples_list)

#Visualizar variabilidad de Kappa entre folds
bwplot(resamples_list, metric = "Kappa")



# ==== Mejoras de rendimiento 1 ====
library(caret)
library(randomForest)
library(xgboost)
library(dplyr)

#Crear la matriz de predictores completa ANTES de particionar
X_total <- model.matrix(loan_status ~ . -1, data = data)
data_xgb_total <- data.frame(X_total)
data_xgb_total$loan_status <- data$loan_status

# 1. Dividir los datos (50% nivel 1, 25% nivel 2, 25% prueba final)
set.seed(123)

train_lvl1_idx <- createDataPartition(data$loan_status, p = 0.5, list = FALSE)
data_train_lvl1 <- data[train_lvl1_idx, ]
rest_data <- data[-train_lvl1_idx, ]

train_lvl2_idx <- createDataPartition(rest_data$loan_status, p = 0.5, list = FALSE)
data_train_lvl2 <- rest_data[train_lvl2_idx, ]
data_test_final <- rest_data[-train_lvl2_idx, ]

# También dividir data_xgb_total (datos con variables dummy)
data_xgb_train_lvl1 <- data_xgb_total[train_lvl1_idx, ]
rest_data_xgb <- data_xgb_total[-train_lvl1_idx, ]
data_xgb_train_lvl2 <- rest_data_xgb[train_lvl2_idx, ]
data_xgb_test_final <- rest_data_xgb[-train_lvl2_idx, ]

#Dividir también para data_reg
data_reg_train_lvl1 <- data_reg[train_lvl1_idx, ]
data_reg_rest <- data_reg[-train_lvl1_idx, ]
data_reg_train_lvl2 <- data_reg_rest[train_lvl2_idx, ]
data_reg_test_final <- data_reg_rest[-train_lvl2_idx, ]

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 2. Entrenar modelos de nivel 1 (M1: RF, M2: XGB, M3: Regresión)

control_simple <- trainControl(method = "none")

# ----> Random Forest
set.seed(123)
modelo_rf_lvl1 <- train(
  loan_status ~ ., data = data_train_lvl1,
  method = "rf",
  trControl = control_simple,
  tuneLength = 1,
  ntree = 200,
  metric = "Kappa"
)

# ----> XGBoost (usando data_xgb_train_lvl1)
set.seed(123)
modelo_xgb_lvl1 <- train(
  loan_status ~ ., 
  data = data_xgb_train_lvl1,
  method = "xgbTree",
  trControl = control_simple,
  metric = "Kappa",
  tuneGrid = expand.grid(
    nrounds = 100,
    max_depth = 6,
    eta = 0.3,
    gamma = 0,
    colsample_bytree = 1,
    min_child_weight = 1,
    subsample = 1
  )
)

# ----> Regresión logística (data_reg)
set.seed(123)
modelo_regresion_lvl1 <- train(
  loan_status ~ ., data = data_reg_train_lvl1,
  method = "glm",
  family = "binomial",
  trControl = control_simple
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 3. Generar predicciones sobre conjunto de entrenamiento de nivel 2

p1_rf <- predict(modelo_rf_lvl1, newdata = data_train_lvl2, type = "prob")[, "Aprobado"]
p2_xgb <- predict(modelo_xgb_lvl1, newdata = data_xgb_train_lvl2, type = "prob")[, "Aprobado"]
p3_reg <- predict(modelo_regresion_lvl1, newdata = data_reg_train_lvl2, type = "prob")[, "Aprobado"]

data_meta_train <- data.frame(
  p1_rf = p1_rf,
  p2_xgb = p2_xgb,
  p3_reg = p3_reg,
  loan_status = data_train_lvl2$loan_status
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# 4. Crear predicciones también para conjunto de prueba final

p1_rf_test <- predict(modelo_rf_lvl1, newdata = data_test_final, type = "prob")[, "Aprobado"]
p2_xgb_test <- predict(modelo_xgb_lvl1, newdata = data_xgb_test_final, type = "prob")[, "Aprobado"]
p3_reg_test <- predict(modelo_regresion_lvl1, newdata = data_reg_test_final, type = "prob")[, "Aprobado"]

data_meta_test <- data.frame(
  p1_rf = p1_rf_test,
  p2_xgb = p2_xgb_test,
  p3_reg = p3_reg_test,
  loan_status = data_test_final$loan_status
)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 5. ENTRENAR EL META-MODELO
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

#Hay que aplicar k-Fold cv para comparar con los modelos del inicio
set.seed(123)
control_cv_meta <- trainControl(
  method = "cv",
  number = 10,
  classProbs = TRUE,
  summaryFunction = defaultSummary, 
  savePredictions = "final"
)

meta_model_cv <- train(
  loan_status ~ ., 
  data = data_meta_train, 
  method = "glm", 
  family = "binomial", 
  trControl = control_cv_meta,
  metric = "Kappa"
)

# 6. Predecir y evaluar el meta-modelo
predicciones_meta <- predict(meta_model, newdata = data_meta_test)
conf_matrix_meta <- confusionMatrix(predicciones_meta, data_meta_test$loan_status)

print(conf_matrix_meta)

# 7. Comparar
accuracy_rf <- modelo_rf_cv$results$Accuracy[which.max(modelo_rf_cv$results$Kappa)]
kappa_rf <- modelo_rf_cv$results$Kappa[which.max(modelo_rf_cv$results$Kappa)]

accuracy_xgb <- modelo_xgb_cv$results$Accuracy[which.max(modelo_xgb_cv$results$Kappa)]
kappa_xgb <- modelo_xgb_cv$results$Kappa[which.max(modelo_xgb_cv$results$Kappa)]

accuracy_meta <- conf_matrix_meta$overall["Accuracy"]
kappa_meta <- conf_matrix_meta$overall["Kappa"]

tabla_comparativa <- data.frame(
  Modelo = c("Random Forest", "XGBoost", "Stacking (Meta-Modelo)"),
  Accuracy = c(accuracy_rf, accuracy_xgb, accuracy_meta),
  Kappa = c(kappa_rf, kappa_xgb, kappa_meta)
)

print(tabla_comparativa)



# ==== Mejoras de rendimiento 2 ====

# ----> Aplicar PCA a data_xgb_train_lvl1 (sin incluir loan_status)
library(caret)

#Separar X e y
X_train <- data_xgb_train_lvl1[, setdiff(names(data_xgb_train_lvl1), "loan_status")]
y_train <- data_xgb_train_lvl1$loan_status

#Aplicar PCA conservando 95% de varianza
pre_pca <- preProcess(X_train, method = "pca", thresh = 0.95)
X_train_pca <- predict(pre_pca, X_train)

#Agregar de nuevo la var. target
data_xgb_pca_train <- cbind(X_train_pca, loan_status = y_train)
print(dim(data_xgb_pca_train))



# ----> Mejorar el modelo inicial de XGB con un grid

#Rejilla de hiperparámetros
grid_xgb_fine <- expand.grid(
  eta = c(0.05, 0.1, 0.3),           #learning rate
  max_depth = c(3, 4),               #profundidad del árbol
  colsample_bytree = c(0.7, 0.8),    #columnas aleatorias por árbol
  subsample = c(0.8, 0.9),           #muestras aleatorias por árbol
  nrounds = c(100, 150),             #número de iteraciones
  gamma = c(0, 0.1),                 #regularización por número mínimo de pérdidas
  min_child_weight = c(1, 5)         #mínimo peso en hijos
)

#Control de validación cruzada
ctrl_xgb_fine <- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = "final"
)


set.seed(300)
m_xgb_fine <- train(
  loan_status ~ ., 
  data = data_xgb_pca_train,    #Usar sólo el 50% de los datos | 1er intento: data | 2do: data_xgb_train_lvl1 | 3er: data_xgb_pca_train
  method = "xgbTree",
  trControl = ctrl_xgb_fine,
  tuneGrid = grid_xgb_fine,
  metric = "Kappa",
  verbosity = 0
)


print(m_xgb_fine)
m_xgb_fine$bestTune   #Mejores hiperparámetros encontrados
max(m_xgb_fine$results$Kappa)  #Mejor valor de Kappa alcanzado



# ==== Mejoras de rendimiento 3 ====
library(splitstackshape)

#Estratificar los datos según 'loan_status'
set.seed(123) 
data_stratified <- stratified(data, group = "loan_status", size = 0.1, replace = FALSE)
table(data_stratified$loan_status) #Están desbalanceadas las clases

#Ajustar el tamaño de la muestra estratificada para balancear las clases
data_stratified <- stratified(data, group = "loan_status", size = c("Aprobado" = 10000, "Rechazado" = 10000), replace = FALSE)
table(data_stratified$loan_status)

#Grid
grid_xgb_fine <- expand.grid(
  eta = c(0.01, 0.05),               # learning rate
  max_depth = c(4, 6),               # profundidad del árbol
  colsample_bytree = c(0.7, 0.8),    # columnas aleatorias por árbol
  subsample = c(0.7, 0.8),           # muestras aleatorias por árbol
  nrounds = c(150, 200),             # número de iteraciones
  gamma = c(0, 0.1, 0.2),            # regularización por número mínimo de pérdidas
  min_child_weight = c(1,5)          # mínimo peso en hijos
)

#Cv - control
ctrl_xgb_fine <- trainControl(
  method = "cv", 
  number = 5, 
  classProbs = TRUE,
  summaryFunction = defaultSummary,
  savePredictions = "final"
)

#Entrenamiento con grid
set.seed(300)
m_xgb_fine <- train(
  loan_status ~ ., 
  data = data_stratified,   
  method = "xgbTree",
  trControl = ctrl_xgb_fine,
  tuneGrid = grid_xgb_fine,
  metric = "Kappa",
  verbosity = 0
)

#Mejores hiperparámetros y Kappa
print(m_xgb_fine)
m_xgb_fine$bestTune   # Mejores hiperparámetros encontrados
max(m_xgb_fine$results$Kappa)  # Mejor valor de Kappa alcanzado





#xd