# Proyceto 1 -Tarea de regresión 
#Taisen Romero Bañuelos (202055209) 03/03/2025

# ==== ASCII ====

#˚　　　　✦　　　.　　. 　.˚　.　　　　　 . ✦　　　 　˚　　　　 . ★⋆. ࿐࿔ 
#... 　　˚　　 　　*　　 　　✦　　　.　　.　　　✦　˚ 　　　　.˚　.˚　　　　✦　　　.　　. 　.˚　.
#⠀⠀⠀⠀⠀⢀⣴⣿⣿⣿⣦⠀
#⠀⠀⠀⠀⣰⣿⡟⢻⣿⡟⢻⣧
#⠀⠀⠀⣰⣿⣿⣇⣸⣿⣇⣸⣿
#⠀⠀⣴⣿⣿⣿⣿⠟⢻⣿⣿⣿
#⣠⣾⣿⣿⣿⣿⣿⣤⣼⣿⣿⠇
#⢿⡿⢿⣿⣿⣿⣿⣿⣿⣿⡿⠀
#⠀⠀⠈⠿⠿⠋⠙⢿⣿⡿⠁⠀


# ==== Predicción de tarifas aéreas en nuevas rutas ====

data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\19 Airfares.csv")

head(data)
str(data)
summary(data)
sum(is.na(data))

#Limpieza de datos
data$S_CODE <- gsub("\\*", "UNK", data$S_CODE)
data$E_CODE <- gsub("\\*", "UNK", data$E_CODE)

#Contar valores "UNK"
table(data$S_CODE == "UNK")
table(data$E_CODE == "UNK")

#Vars categóricas a factor
data$VACATION <- as.factor(data$VACATION)
data$SW <- as.factor(data$SW)
data$SLOT <- as.factor(data$SLOT)
data$GATE <- as.factor(data$GATE)
str(data)

# --> Graficar distribución (histograma) de variables numéricas <--
library(ggplot2)

#Tarifas (FARE)
ggplot(data, aes(x = FARE)) +
  geom_histogram(bins = 30, fill = "skyblue", color = "black") +
  labs(title = "Distribución de Tarifas (FARE)", x = "Tarifa Promedio (USD)", y = "Frecuencia")

#DISTANCE
ggplot(data, aes(x = DISTANCE)) +
  geom_histogram(bins = 30, fill = "lightgreen", color = "black") +
  labs(title = "Distribución de Distancias", x = "Distancia (millas)", y = "Frecuencia")

#Número de pasajeros (PAX)
ggplot(data, aes(x = PAX)) +
  geom_histogram(bins = 30, fill = "orange", color = "black") +
  labs(title = "Distribución de Pasajeros (PAX) por Ruta", x = "Número de Pasajeros", y = "Frecuencia")

#Competitividad
ggplot(data, aes(x = HI)) +
  geom_histogram(bins = 30, fill = "purple", color = "black") +
  labs(title = "Distribución de Pasajeros (PAX) por Ruta", x = "Concentración del índice de Herfindahal", y = "Frecuencia")



# --> Correlación entre variables numéricas <--
library(corrplot)

num_vars <- data[, sapply(data, is.numeric)]
cor_matrix<-cor(num_vars)
cor_matrix
corrplot(cor_matrix, method = "color", type = "upper", tl.cex = 0.8, title = "Matriz de Correlación", mar = c(0, 0, 1, 0))  #tl.cex: es para el tamaño de las etiquetas


# --> Boxplots <--
boxplot(data$HI)
boxplot(data$FARE, main = "Detección de Outliers en HI", ylab = "Indice de Herfindahal", col = "lightgray")
boxplot.stats(data$HI)$out #Mostrar los datos atípicos

boxplot(data$FARE)
boxplot(data$FARE, main = "Detección de Outliers en Tarifas", ylab = "Tarifa Promedio (USD)", col = "lightgray")
boxplot.stats(data$FARE)$out #Mostrar los datos atípicos

boxplot(data$PAX)
boxplot(data$FARE, main = "Detección de Outliers en PAX", ylab = "Número de pasajeros", col = "lightgray")
boxplot.stats(data$PAX)$out #Mostrar los datos atípicos

summary(data)


#Tarifas según la presencia de Southwest Airlines
ggplot(data, aes(x = SW, y = FARE, fill = SW)) +
  geom_boxplot() +
  labs(title = "Impacto de Southwest Airlines en las Tarifas", x = "Presencia de Southwest (Sí/No)", y = "Tarifa Promedio (USD)") +
  scale_fill_manual(values = c("Yes" = "lightblue", "No" = "lightcoral"))

#Tarifas según restricciones de SLOT
ggplot(data, aes(x = SLOT, y = FARE, fill = SLOT)) +
  geom_boxplot() +
  labs(title = "Impacto de Restricciones de SLOT en las Tarifas", x = "Restricción de Slot (Libre/Controlado)", y = "Tarifa Promedio (USD)") +
  scale_fill_manual(values = c("Free" = "lightgreen", "Controlled" = "tomato"))

#Tarifas según restricciones de GATE
ggplot(data, aes(x = GATE, y = FARE, fill = GATE)) +
  geom_boxplot() +
  labs(title = "Impacto de Restricciones de GATE en las Tarifas", x = "Restricción de Puerta (Libre/Controlado)", y = "Tarifa Promedio (USD)") +
  scale_fill_manual(values = c("Free" = "lightblue", "Constrained" = "purple"))


# ---- Inciso A ----
#La matriz de correlación ya se hizo arriba (linea 57) así que se harán los diagramas de dispersión.
#Se empezará con el ploteo de las variables que tenían más correlación con FARE

library(ggplot2)

#FARE vs DISTANCE
ggplot(data, aes(x = DISTANCE, y = FARE)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Diagrama de Dispersión: FARE vs DISTANCE",
       x = "DISTANCE (Millas)", y = "Tarifa Promedio (FARE)")

#FARE vs PAX
ggplot(data, aes(x = PAX, y = FARE)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Diagrama de Dispersión: FARE vs PAX",
       x = "Número de Pasajeros (PAX)", y = "Tarifa Promedio (FARE)")

#FARE vs COUPON
ggplot(data, aes(x = COUPON, y = FARE)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Diagrama de Dispersión: FARE vs COUPON",
       x = "Número de Cupones (COUPON)", y = "Tarifa Promedio (FARE)")

#FARE vs HI
ggplot(data, aes(x = HI, y = FARE)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  labs(title = "Diagrama de Dispersión: FARE vs HI",
       x = "índice de Herfindahal", y = "Tarifa Promedio (FARE)")



#Diagrama de dispersión facetado (FARE vs Vars. numéricas)
ggplot(data = reshape2::melt(data, id.vars = "FARE", 
                             measure.vars = c("COUPON", "NEW", "HI", "S_INCOME","E_INCOME", "S_POP", "E_POP", "DISTANCE", "PAX")),aes(x = value, y = FARE)) +
  geom_point(alpha = 0.5, color = "blue") +
  geom_smooth(method = "lm", col = "red", se = FALSE) +
  facet_wrap(~variable, scales = "free_x") +
  labs(title = "Diagramas de dispersión de FARE vs Predictores numéricos",x = "Valor del predictor", y = "Tarifa Promedio (FARE)")



# ---- Inciso B ----
library(dplyr)
library(ggplot2)

str(data)

mean_fare_by_city <- data %>%
  group_by(S_CITY, E_CITY) %>%
  summarise(Fare_Mean = mean(FARE, na.rm = TRUE)) %>%
  arrange(desc(Fare_Mean))

#Mostrar las primeras filas para ver si hay diferencias significativas
print(head(mean_fare_by_city, 10))  #Las 10 rutas más caras
print(tail(mean_fare_by_city, 10))  #Las 10 rutas más baratas

#Gráfico de puntos ordenado (Dot Plot)
ggplot(mean_fare_by_city, aes(x = reorder(S_CITY, Fare_Mean), y = Fare_Mean)) +
  geom_point(color = "red", size = 3) +
  coord_flip() +
  labs(title = "Distribución de Tarifa Promedio por Ciudad de Origen",
       x = "Ciudad de Origen", y = "Tarifa Promedio (USD)") +
  theme_minimal()


#Ordenar las ciudades por tarifa promedio
fare_by_city <- mean_fare_by_city %>% arrange(Fare_Mean)

#Gráfico de barras
ggplot(fare_by_city, aes(x = reorder(S_CITY, Fare_Mean), y = Fare_Mean)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  coord_flip() +  # Para que las barras sean horizontales y más legibles
  labs(title = "Tarifa Promedio por Ciudad de Origen",
       x = "Ciudad de Origen", y = "Tarifa Promedio (USD)") +
  theme_minimal()

# Clasificar ciudades
data <- data %>%
  mutate(CITY_TYPE = case_when(
    # Hubs Nacionales e Internacionales (ahora con distancia mínima de 800 millas)
    (S_POP > 5000000 | E_POP > 5000000) & DISTANCE > 800 ~ "Hub Nacional/Internacional",
    
    # Aeropuertos Nacionales/Regionales (ciudades medianas con vuelos largos)
    S_POP > 1000000 & S_POP <= 5000000 & DISTANCE > 500 ~ "Aeropuerto Nacional/Regional",
    
    # Aeropuertos Low-Cost (competencia alta o presencia de Southwest)
    (SW == "Yes" | HI < 4000) ~ "Aeropuerto Low-Cost",
    
    # Aeropuertos de Conectividad (concentración moderada y vuelos medios)
    HI >= 3000 & HI <= 6000 & DISTANCE < 1500 ~ "Aeropuerto Secundario/Conectividad",
    
    # Aeropuertos de Tráfico Local (ajustado para incluir más aeropuertos de "Otros")
    DISTANCE < 600 & HI > 6000 ~ "Aeropuerto de Tráfico Local",
    
    # Nueva Categoría: Aeropuerto Mediano con Rutas Regionales
    S_POP > 500000 & S_POP <= 2000000 & DISTANCE >= 250 & DISTANCE <= 1000 ~ "Aeropuerto Mediano Regional",
    
    # Si no cumple ninguna condición, se clasifica como "Otros"
    TRUE ~ "Otros"
  ))

#Distribución de la clasificación
table(data$CITY_TYPE)

otros_cities <- data %>%
  filter(CITY_TYPE == "Otros") %>%
  select(S_CITY, E_CITY, FARE, DISTANCE, S_POP, HI, SW) %>%
  head(20)
print(otros_cities)



# ---> Ver que nos muestra un árbol de decisión
library(rpart)
library(rpart.plot)

#Crear variable objetivo basada en nuestra clasificación actual
data$CITY_TYPE <- as.factor(data$CITY_TYPE)

#sin S_CITY y E_CITY porque son texto
predictors <- data[, c("DISTANCE", "S_POP", "HI", "SW")]

#Entrenamiento
tree_model <- rpart(CITY_TYPE ~ DISTANCE + S_POP + HI + SW, data = data, method = "class")
summary(tree_model)

rpart.plot(tree_model, type = 2, extra = 104, box.palette = "RdBu", branch.lty = 3, cex = 0.4)

# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #
#DECARGAR ÁRBOL

#Para ver todo bien ordenado
png("arbol_decision1.png", width = 2000, height = 1500, res = 200)  #Tamaño y resolución alta
rpart.plot(tree_model, type = 2, extra = 104, box.palette = "RdBu", branch.lty = 3, cex = 0.5)
dev.off()

#Para poder ver los criterios
png("arbol_decision2.png", width = 2000, height = 1500, res = 200)  #Tamaño y resolución alta
rpart.plot(tree_model, type = 2, extra = 104, box.palette = "RdBu", branch.lty = 3, cex = 0.7)
dev.off()  # Cierra el archivo PNG
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # #

#Predicción
data$CITY_TYPE_PRED <- predict(tree_model, data, type = "class")

#Evaluación - manual vs predicción
table(data$CITY_TYPE, data$CITY_TYPE_PRED)

# <---


categ_vars <- c("CITY_TYPE", "VACATION", "SW", "SLOT", "GATE")

#Porcentaje de vuelos en cada categoría
for (var in categ_vars) {
  cat("\nDistribución de", var, ":\n")
  print(round(prop.table(table(data[[var]])) * 100, 2))
}

#Tarifa promedio por categoría
fare_by_category <- list()
for (var in categ_vars) {
  fare_by_category[[var]] <- aggregate(FARE ~ get(var), data, mean)
  colnames(fare_by_category[[var]]) <- c(var, "Mean_FARE")
}

for (var in categ_vars) {
  cat("\nTarifa Promedio según", var, ":\n")
  print(fare_by_category[[var]])
}

# Calcular la diferencia de tarifas para cada variable categórica
fare_diff <- list()
for (var in categ_vars) {
  min_fare <- min(fare_by_category[[var]]$Mean_FARE)
  max_fare <- max(fare_by_category[[var]]$Mean_FARE)
  diff <- max_fare - min_fare
  fare_diff[[var]] <- diff
}
print(fare_diff)



# ---- Inciso C ----
library(caret)
library(dplyr)

categ_vars <- c("CITY_TYPE", "VACATION", "SW", "SLOT", "GATE")
data[categ_vars] <- lapply(data[categ_vars], as.factor)

#One-hot encoding solo a las variables categóricas seleccionadas
data_dummies <- dummyVars(" ~ .", data = data[, c(categ_vars, "FARE")], fullRank = TRUE)
data_transformed <- predict(data_dummies, newdata = data) %>%
  as.data.frame()

#Crear nuevo DF añadiendo vars. numéricas excluyendo vars. que no sirven
num_vars <- setdiff(names(data), c("S_CODE", "S_CITY", "E_CODE", "E_CITY","CITY_TYPE_PRED", categ_vars))
data_transformed[num_vars] <- data[num_vars]


set.seed(123)
train_index <- createDataPartition(data_transformed$FARE, p = 0.8, list = FALSE)

train <- data_transformed[train_index, ]
test <- data_transformed[-train_index, ]

# Regresión por pasos
reg_lin <- lm(FARE ~ ., data = train)
model_step <- step(reg_lin, direction = "both", trace = 1)
summary(model_step)

names(coef(reg_lin)) #Variables originales
names(coef(model_step)) #Vars que quedaron 
cat("Variables eliminadas:", setdiff(names(coef(reg_lin)), names(coef(model_step))), "\n")

# ---> iii)
library(leaps)

#Búsqueda exhaustiva de predictores
ex_search <- regsubsets(FARE ~ ., data = train, nvmax = ncol(train) - 1)

#Identificar el mejor modelo según BIC (criterio de información de Bayes)
summary_ex <- summary(ex_search)
bic_values <- summary_ex$bic
best_model_index <- which.min(bic_values)

best_model_vars <- coef(ex_search, best_model_index)
best_model_vars

# <---

# ---> iv)

#Pred de regresión por pasos
test$FARE_PRED_STEP <- predict(model_step, newdata = test)

#Pred de búsqueda exhaustiva
best_model_vars_names <- setdiff(names(best_model_vars), "(Intercept)")
model_ex <- lm(FARE ~ ., data = train[, c("FARE", best_model_vars_names), drop = FALSE])
test$FARE_PRED_EX <- predict(model_ex, newdata = test[, best_model_vars_names, drop = FALSE])

rmse_step <- sqrt(mean((test$FARE - test$FARE_PRED_STEP)^2))
mae_step <- mean(abs(test$FARE - test$FARE_PRED_STEP))

rmse_ex <- sqrt(mean((test$FARE - test$FARE_PRED_EX)^2))
mae_ex <- mean(abs(test$FARE - test$FARE_PRED_EX))

print(paste("Regresión por pasos - RMSE:", round(rmse_step, 2), "| MAE:", round(mae_step, 2)))
print(paste("Búsqueda exhaustiva - RMSE:", round(rmse_ex, 2), "| MAE:", round(mae_ex, 2)))


# <---

# ---> v)

nueva_ruta <- data.frame(
  CITY_TYPE.Otros = 0,  #Asumiendo que no es de tipo "Otros"
  VACATION.Yes = 0,  #"No" en binario
  SW.Yes = 0,  
  SLOT.Free = 1,  #"Free" en binario
  GATE.Free = 1,  
  HI = 4442.141,
  E_INCOME = 27664,
  S_POP = 4557004,
  E_POP = 3195503,
  DISTANCE = 1976,
  PAX = 12782
)

tarifa_pred <- predict(model_ex, newdata = nueva_ruta)
tarifa_pred
print(paste("Tarifa promedio predicha:", round(tarifa_pred, 2), "USD"))

# <---

# ---> vi)
#Lo mismo pero con SW == 1 == Yes
nueva_ruta_SW <- nueva_ruta
nueva_ruta_SW$SW.Yes <- 1  

tarifa_predicha_SW <- predict(model_ex, newdata = nueva_ruta_SW)
tarifa_predicha_SW
print(paste("Tarifa promedio con Southwest:", round(tarifa_predicha_SW, 2), "USD"))

#Calcular reducción en la tarifa
reduccion_tarifa <- tarifa_pred - tarifa_predicha_SW
reduccion_tarifa
print(paste("Reducción estimada en la tarifa:", round(reduccion_tarifa, 2), "USD"))


# <---

# ---> viii)
library(leaps)

#Vars. disponibles antes de operar vuelos
vars_disp <- c("DISTANCE", "S_POP", "E_POP", "S_INCOME", "E_INCOME","SLOT.Free", "GATE.Free", "SW.Yes", "VACATION.Yes")

train_reducido <- train[, c("FARE", vars_disp)]

ex_search_reducido <- regsubsets(FARE ~ ., data = train_reducido, nvmax = length(vars_disp))

#Identificar el mejor modelo según BIC
summary_ex_reducido <- summary(ex_search_reducido)
bic_values_reducido <- summary_ex_reducido$bic
best_model_index_reducido <- which.min(bic_values_reducido)

#Obtener predictores seleccionados en el mejor modelo
best_model_vars_reducido <- coef(ex_search_reducido, best_model_index_reducido)
best_model_vars_reducido

# <---

# ---> ix)

nueva_ruta_reducida <- data.frame(
  DISTANCE = 1976,
  E_INCOME = 27664,
  SLOT.Free = 1,  #"Free"
  GATE.Free = 1,  
  SW.Yes = 0,  #"No"
  VACATION.Yes = 0  
)

#Extraer nombre de las vars. seleccionadas
best_model_vars_names_reducido <- setdiff(names(best_model_vars_reducido), "(Intercept)")

model_ex_reducido <- lm(FARE ~ ., data = train_reducido[, c("FARE", best_model_vars_names_reducido), drop = FALSE])

#Predecir la tarifa en la nueva ruta
tarifa_pred_reducida <- predict(model_ex_reducido, newdata = nueva_ruta_reducida)

print(paste("Tarifa promedio predicha con modelo reducido:", round(tarifa_pred_reducida, 2), "USD"))


# <---

# ---> x)

#Predicciones con el modelo original (iii)
test$FARE_PRED_EX <- predict(model_ex, newdata = test[, best_model_vars_names, drop = FALSE])

#Predicciones con el modelo reducido (viii)
test$FARE_PRED_EX_REDUCIDO <- predict(model_ex_reducido, newdata = test[, best_model_vars_names_reducido, drop = FALSE])

rmse_ex <- sqrt(mean((test$FARE - test$FARE_PRED_EX)^2))
mae_ex <- mean(abs(test$FARE - test$FARE_PRED_EX))

rmse_ex_reducido <- sqrt(mean((test$FARE - test$FARE_PRED_EX_REDUCIDO)^2))
mae_ex_reducido <- mean(abs(test$FARE - test$FARE_PRED_EX_REDUCIDO))

print(paste("Modelo original (iii) - RMSE:", round(rmse_ex, 2), "| MAE:", round(mae_ex, 2)))
print(paste("Modelo reducido (viii) - RMSE:", round(rmse_ex_reducido, 2), "| MAE:", round(mae_ex_reducido, 2)))


# <---





# ==== Situación financiera de los bancos ====

data_bank<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\19 Banks.csv")
head(data_bank)
str(data_bank)
summary(data_bank)


# ---- Inciso A ----

#Convertir Financial.Condition a factor (1 = débil, 0 = fuerte)
data_bank$Financial.Condition <- factor(data_bank$Financial.Condition, levels = c(0,1))

model_bank<-glm(Financial.Condition ~ TotLns.Lses.Assets + TotExp.Assets, data = data_bank, family = binomial)
summary(model_bank)

coeficientes <- coef(model_bank)
coeficientes
beta_0 <- coeficientes[1] #Intercept
beta_1 <- coeficientes[2] #TotLns.Lses.Assets
beta_2 <- coeficientes[3] #TotExp.Assets 

#i)
cat("Logit(π) =", beta_0, "+", beta_1, "* TotLns.Lses.Assets +", beta_2, "* TotExp.Assets \n")

#ii)
cat("Odds(π) = exp(", beta_0, "+", beta_1, "* TotLns.Lses.Assets +", beta_2, "* TotExp.Assets )\n")

#iii)
cat("P(Débil) = 1 / (1 + exp(-(", beta_0, "+", beta_1, "* TotLns.Lses.Assets +", beta_2, "* TotExp.Assets )))\n")



# ---- Inciso B ----

new_bank <- data.frame(TotLns.Lses.Assets = 0.6, TotExp.Assets = 0.11)

#Calcular el logit
logit_val <- coef(model_bank)[1] + coef(model_bank)[2] * new_bank$TotLns.Lses.Assets + coef(model_bank)[3] * new_bank$TotExp.Assets
cat("Logit:", round(logit_val, 4), "\n")

#Calcular odds
odds<- exp(logit_val)
cat("Odds:", round(odds, 4), "\n")

#Probabilidad de ser financieramente débil
weak_prob<- 1 / (1 + exp(-logit_val))
cat("Probabilidad de ser débil:", round(weak_prob, 4), "\n")

#Clasificación del banco (corte en 0.5)
clasificacion <- ifelse(weak_prob >= 0.5, "Débil", "Fuerte")
clasificacion


# ---- Inciso C ----
library(pROC)

#El nuevo umbral de probabilidad (p) es 0.6, así que:
new_prob<-0.6
nuevo_umbral_logit <- log(new_prob / (1 - new_prob))
cat("Umbral equivalente en logit:", round(nuevo_umbral_logit, 4), "\n")



probabilidades_predict <- predict(model_bank, type = "response")
roc_obj <- roc(data_bank$Financial.Condition, probabilidades_predict)

#Encontrar el mejor umbral usando el índice de Youden
mejor_umbral_prob <- coords(roc_obj, "best", ret = "threshold")
mejor_umbral_prob

#Convertir el umbral de probabilidad a logit
mejor_umbral_logit <- log(mejor_umbral_prob / (1 - mejor_umbral_prob))
mejor_umbral_logit


# ---- Inciso D ----

#Cambio en la probabilidad de ser débil para un aumento de unidad (delta X = 0.1)
cambio <- 0.1
logit_change <- beta_1 * cambio
prob_change <- 1 / (1 + exp(-logit_change))

cat("Cambio en la probabilidad por un incremento de 0.1 en TotLns.Lses.Assets:", round(prob_change, 4), "\n")


# ---- Inciso E ----

#Suecuencia de umbrales
umbrales <- seq(0.3, 0.8, by = 0.05)

resultados <- data.frame(Umbral = umbrales, Sensibilidad = NA, Especificidad = NA)

probs_E <- predict(model_bank, type = "response")

#Calcular sensibilidad y especificidad para cada umbral
for (i in 1:length(umbrales)) {
  umbral <- umbrales[i]
  
  predicciones <- ifelse(probs_E >= umbral, "Débil", "Fuerte")
  
  matriz_conf <- table(Predicho = predicciones, Real = data_bank$Financial.Condition)
  
  sensibilidad <- matriz_conf["Débil", "1"] / sum(matriz_conf[, "1"])  # Tasa de verdaderos positivos
  especificidad <- matriz_conf["Fuerte", "0"] / sum(matriz_conf[, "0"])  # Tasa de verdaderos negativos
  
  resultados$Sensibilidad[i] <- sensibilidad
  resultados$Especificidad[i] <- especificidad
}

print(resultados)










#xd