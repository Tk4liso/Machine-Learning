#05/05/2025
#Máquinas de aprendizaje - Proyecto final
#Music Genre Classification (tres archivos csv: test, train y submission)
#Alumno: Taisen Romero Bañuelos (202055209).

submission<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\39 submission.csv")
train<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\39 train.csv")
test<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\39 test.csv")

head(submission)
head(train)
head(test)

summary(submission)
summary(train)
summary(test)

str(submission)
str(train)
str(test)

sum(is.na(submission))
sum(is.na(train))
sum(is.na(test))

#Contar filas donde todas las columnas valen 0 en submission
filas_ceros <- sum(rowSums(submission) == 0)
filas_ceros
submission[rowSums(submission) == 0, ]

#as.factor(train$Class)

#¿Por qué solo unas pocas observaciones en submission fueron clasificadas 
#(con un 1 en alguna columna), mientras la gran mayoría no recibió ninguna clase?

table(rowSums(submission))  #Cuántas filas tienen todo ceros o un 1

#Explorar las 11 filas clasificadas
which(rowSums(submission) == 1)  # Índices
test_clasificadas <- test[rowSums(submission) == 1, ]
test_clasificadas

submission[rowSums(submission) == 1, ] #Ver a qué géneros corresponden
summary(test_clasificadas)




# ==== Knowledge Discovery in Databases (KDD) ====
#1. Data Selection
#2. Data Cleaning and Preprocessing
#3. Data Transformation and Reduction
#4. Data Mining
#5. Evaluation and Interpretation of Results

#Nota: luego del paso 3 pasar al EDA para un mejor flujo.

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 1. DATA SELECTION

# ---> Unir test y train
train$source <- "train"
test$source <- "test"
test$Class <- NA  #Para igualar estructura

#Reordenar columnas para que coincidan
test <- test[, names(train)]

datos_completos <- rbind(train, test)
head(datos_completos)

#train + test
17996 + 7713

str(datos_completos) #Coincide el # de observaciones con la suma manual

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 2. DATA CLEANING AND PROCESSING

#Convertir a factor vars. categ.
datos_completos$Class <- factor(datos_completos$Class,
                                levels = 0:10,
                                labels = c("Acoustic/Folk", "Alt_Music", "Blues", "Bollywood", "Country",
                                           "HipHop", "Indie/Alt", "Instrumental", "Metal", "Pop", "Rock"))

datos_completos$key <- factor(datos_completos$key)
datos_completos$mode <- factor(datos_completos$mode, levels = c(0,1), labels = c("Minor", "Major"))
datos_completos$time_signature <- factor(datos_completos$time_signature)

str(datos_completos)

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# 3. DATA TRANSFORMATION AND REDUCTION

#Ver proporción de NAs
sum(is.na(datos_completos))
colSums(is.na(datos_completos))
colSums(is.na(datos_completos)) / nrow(datos_completos) #Los NA's de class nosotros los pusimos porque test no tenía class


# ---> Averiguar el tipo de datos faltantes que tenemos
library(VIM)
library(naniar)

# - Ver patrones de NA -
aggr(datos_completos[, c("key", "instrumentalness", "Popularity")],
     col = c("skyblue", "red"), numbers = TRUE, sortVars = TRUE,
     labels = names(datos_completos), cex.axis = 0.7,
     gap = 3, ylab = c("Frecuencia de NA", "Patrón de NA"))


# - Ver si los datos faltantes están relacionados con otras variables (¿MAR?) -
datos_completos$inst_missing <- is.na(datos_completos$instrumentalness) #Crear variable binaria: tiene o no tiene NA

#Comparar con otras vars.
boxplot(danceability ~ inst_missing, data = datos_completos, main = "Danceability vs NA en Instrumentalness")
boxplot(energy ~ inst_missing, data = datos_completos, main = "Energy vs NA en Instrumentalness")


# - Otro plot sobre el patrón de datos faltantes -
vis_miss(datos_completos[, c("key", "instrumentalness", "Popularity")])

# --->

# ---> Aplicar missForest
library(missForest)

datos_mf <- datos_completos #Copia de seguridad

#Convertir columnas de texto a factor
#datos_mf$Artist.Name <- as.factor(datos_mf$Artist.Name)
#datos_mf$Track.Name <- as.factor(datos_mf$Track.Name)

datos_mf$source <- NULL
datos_mf$inst_missing <- NULL
datos_mf$Class <- NULL

#Eliminar columnas con muchos niveles categóricos
datos_mf$Artist.Name <- NULL
datos_mf$Track.Name <- NULL

head(datos_mf)


#Aplicar missForest
set.seed(777)
#imputacion <- missForest(datos_mf, maxiter = 5)
imputacion <- missForest(datos_mf)
imputacion

imputacion$OOBerror  # error promedio de imputación (0 = perfecto)

#Extraer el dataset imputado
datos_imputados <- imputacion$ximp
colSums(is.na(datos_imputados))

# --->


#Conversión de duración de milisegundos a minutos
library(dplyr)
datos_imputados$duration_minutes <- datos_imputados$duration_in.min.ms / 60000
#head(datos_completos)
datos_imputados <- datos_imputados %>% select(-duration_in.min.ms)
head(datos_imputados)


# ---> Z-score y tratamiento de outliers
summary(datos_imputados) #Buscar vars. con outliers

#Func. para winsorizar
winsorizar <- function(df, columnas, p_inf = 0.01, p_sup = 0.99) {
  for (col in columnas) {
    limites <- quantile(df[[col]], probs = c(p_inf, p_sup), na.rm = TRUE)
    df[[col]][df[[col]] < limites[1]] <- limites[1]
    df[[col]][df[[col]] > limites[2]] <- limites[2]
  }
  return(df)
}

#Aplicar a vars. seleccionadas
variables_outliers <- c("Popularity", "loudness", "instrumentalness", "tempo", "duration_minutes")
data_clean <- winsorizar(datos_imputados, variables_outliers)

summary(data_clean)


# -> Z-score <-
vars_numericas <- names(data_clean)[sapply(data_clean, is.numeric)]
data_z <- data_clean #Copia de seguridad

#Aplicar Z-score
data_z[vars_numericas] <- scale(data_z[vars_numericas])
summary(data_z)
#summary(data_clean)

# --->

# ---> Separar los datos otra vez

#Recuperar la columna 'source' y 'artist.name' | Track name no se recupera porque no parece relevante para el análisis (a menos que se haga NLP)
data_z$source <- datos_completos$source
data_z$Artist.Name <- datos_completos$Artist.Name
data_z$Class <- datos_completos$Class

#Dividir de nuevo según 'source'
train_clean <- data_z[data_z$source == "train", ]
test_clean  <- data_z[data_z$source == "test", ]

train_clean$source <- NULL
test_clean$source  <- NULL
test_clean$Class  <- NULL

dim(train)
dim(test)

dim(train_clean)
dim(test_clean)

# --->

# ---> Aplicar PCA

# -> PCA para el EDA <-
library(FactoMineR)
library(factoextra)

vars_numericas <- names(train_clean)[sapply(train_clean, is.numeric)]
#vars_numericas <- setdiff(vars_numericas, "time_signature")  #Se podría remover, probablemente no sea un predictor fuerte

pca_result <- PCA(train_clean[, vars_numericas], graph = FALSE)
#plot(pca_result)

#Scree plot (porcentaje de varianza por componente)
fviz_eig(pca_result, addlabels = TRUE, ylim = c(0, 50))

#Ver cuántos componentes acumulan 80–90% de la varianza
var_explicada <- cumsum(pca_result$eig[, 2])
var_explicada  #varianza acumulada

#Plot en 2D
fviz_pca_ind(pca_result,
             geom.ind = "point",
             col.ind = train_clean$Class,  #Colorea por clase real
             palette = "jco",
             addEllipses = TRUE,
             legend.title = "Género")

#Variables que más influyen en PC1 y PC2
fviz_pca_var(pca_result, col.var = "contrib", gradient.cols = c("blue", "orange", "red"))



# -> PCA para el modelado <-
library(caret)

#Aplicar PCA solo en el conjunto de entrenamiento
preproc_pca <- preProcess(train_clean[, -which(names(train_clean) == "Class")],
                          method = "pca", thresh = 0.9)

#Transformar training y testing con el mismo PCA
train_pca <- predict(preproc_pca, train_clean)
test_pca  <- predict(preproc_pca, test_clean)

names(train_pca)

dim(train_pca)
dim(test_pca)

#Guardar datasets
#write.csv(train_pca, "39 train_pca.csv", row.names = FALSE)
#write.csv(test_pca, "39 test_pca.csv", row.names = FALSE)


# ==== Análisis exploratorio de datos (EDA) ====
library(ggplot2)
library(reshape2)
library(corrplot)
library(plotly)
library(dplyr)

str(train_pca)
str(test_pca)


ggplot(train_pca, aes(x = PC1, y = PC2, color = Class)) +
  geom_point(alpha = 0.6) +
  labs(title = "Distribución de canciones por género (PC1 vs PC2)") +
  theme_minimal()

#Boxplot facetado
data_long <- melt(train_pca, id.vars = "Class", measure.vars = paste0("PC", 1:6))  #Ampliar a PC10 OPCIONAL
ggplot(data_long, aes(x = Class, y = value, fill = Class)) +
  geom_boxplot() +
  facet_wrap(~variable, scales = "free_y") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Distribución de componentes principales por género")


#Distribuciones de variables numéricas
numeric_vars <- train_pca %>% select(starts_with("PC"))

for (col in names(numeric_vars)) {
  p <- ggplot(train_pca, aes_string(x = col)) +
    geom_histogram(aes(y = ..density..), bins = 30, fill = "skyblue", color = "white") +
    geom_density(color = "red") +
    ggtitle(paste("Distribución de", col)) +
    theme_minimal()
  print(p)
}

#Distribución de variables categóricas
ggplot(train_pca, aes(x = key, fill = Class)) +
  geom_bar(position = "fill") +
  labs(title = "Distribución de la variable 'key' por género",
       y = "Proporción", x = "key") +
  theme_minimal()


#Matriz de correlación
pc_data <- train_pca %>% select(starts_with("PC"))
cor_matrix <- cor(pc_data)
print(round(cor_matrix, 3))

#ANTES DE APLICAR Z-SCORE
numeric_vars <- train_clean %>% select(where(is.numeric))
cor_matrix_original <- cor(numeric_vars)
print(round(cor_matrix_original, 2))


#Plot en 3D
plot_ly(train_pca, x = ~PC1, y = ~PC2, z = ~PC3, color = ~Class, colors = "Set3",
        type = "scatter3d", mode = "markers") %>%
  layout(title = "Visualización 3D: PC1 vs PC2 vs PC3 por género")



# ---> Calcular distribución de géneros por artista (en train_pca)

#Obtener los 7 artistas con más canciones
top_artists <- train_pca %>%
  count(Artist.Name) %>%
  top_n(7, wt = n) %>%
  pull(Artist.Name)

#Filtrar train_pca para esos artistas y calcular proporciones por género
artist_genre_plotdata <- train_pca %>%
  filter(Artist.Name %in% top_artists) %>%
  count(Artist.Name, Class) %>%
  group_by(Artist.Name) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

#Plot de barras apiladas (proporción de géneros por artista)
ggplot(artist_genre_plotdata, aes(x = reorder(Artist.Name, -prop), y = prop, fill = Class)) +
  geom_bar(stat = "identity") +
  labs(title = "Distribución de géneros musicales por artista (Top 7)",
       x = "Artista", y = "Proporción", fill = "Género") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# -> Etiquetar cada artista con su género más frecuente <-
artist_genre_counts <- train_pca %>%
  count(Artist.Name, Class) %>%
  group_by(Artist.Name) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

main_genre_artist <- artist_genre_counts %>%
  group_by(Artist.Name) %>%
  slice_max(prop, n = 1, with_ties = FALSE) %>%
  select(Artist.Name, main_genre = Class)

train_pca <- train_pca %>%
  left_join(main_genre_artist, by = "Artist.Name")

str(train_pca)

#Algunos artistas en test no estarán en train. Para esos casos:
test_pca <- test_pca %>%
  left_join(main_genre_artist, by = "Artist.Name") %>%
  mutate(main_genre = ifelse(is.na(main_genre), "Unknown", main_genre),
         main_genre = as.factor(main_genre))



# ==== Modelado ====

str(train_pca)
str(test_pca)

train_copy<-train_pca
test_copy<-test_pca

str(train_copy)
str(test_copy)

library(dplyr)
library(arules)

# --- Discretizar las PCs --- 

#Discretización en 4 bins
train_rules <- train_pca %>%
  mutate(across(starts_with("PC"), ~ cut(.x, breaks = 4, labels = FALSE, include.lowest = TRUE), .names = "bin_{.col}"))

#Seleccionar variables relevantes
# Convertimos todo a factor (obligatorio para arules)
train_rules <- train_rules %>%
  mutate(across(c(key, mode, time_signature, main_genre, starts_with("bin_"), Class), as.factor)) %>%
  select(key, mode, time_signature, main_genre, starts_with("bin_"), Class)

#Convertir a transacciones
transacciones <- as(train_rules, "transactions")
summary(transacciones)


#Generar reglas (consectuente=Class)
reglas <- apriori(transacciones,
                  parameter = list(supp = 0.001, conf = 0.6, minlen = 2, target = "rules"),
                  appearance = list(rhs = grep("^Class=", itemLabels(transacciones), value = TRUE),
                                    default = "lhs"), maxlen = 15)
#summary(reglas)

#Filtrar reglas útiles por lift y confianza alta
reglas_utiles <- reglas %>%
  subset(size(lhs) <= 5 & confidence > 0.7 & lift > 1.1)

inspect(head(sort(reglas_utiles, by = "confidence"), 10)) #Las top 10 por confianza

summary(reglas)


# ---> Predicciones
library(stringr)
library(caret)

#Esta sirve para datos "reales". NO USAR SI SE QUIERE CALCULAR KAPPA O SIMILARES
predecir_con_reglas <- function(df_nuevo, reglas_utiles) {
  
  #Discretización igual que en entrenamiento
  df_binned <- df_nuevo %>%
    mutate(across(starts_with("PC"), ~ cut(.x, breaks = 4, labels = FALSE, include.lowest = TRUE), .names = "bin_{.col}")) %>%
    mutate(across(c(key, mode, time_signature, main_genre, starts_with("bin_")), as.factor))
  
  #Obtener el universo válido de ítems
  items_validos <- itemLabels(reglas_utiles)
  
  predicciones <- character(nrow(df_binned))
  
  for (i in 1:nrow(df_binned)) {
    
    items_instancia <- paste0(names(df_binned), "=", as.character(df_binned[i, ])) #Construir items
    
    #Filtrar solo los que están en itemLabels(reglas)
    items_instancia <- items_instancia[items_instancia %in% items_validos]
    
    #Buscar reglas cuyo LHS sea subconjunto de los ítems válidos
    reglas_aplicables <- subset(reglas_utiles, subset = lhs %in% items_instancia)
    
    if (length(reglas_aplicables) > 0) {
      mejor_regla <- reglas_aplicables[which.max(quality(reglas_aplicables)$confidence)]
      predicciones[i] <- gsub("\\{Class=|\\}", "", labels(rhs(mejor_regla)))
    } else {
      #Fallback seguro
      predicciones[i] <- as.character(df_binned$main_genre[i])
    }
  }
  
  return(predicciones)
}



#Hacer predicción
pred_clases <- predecir_con_reglas(test_pca, reglas_utiles)
table(pred_clases)


# ---> Evaluación
library(dplyr)
library(arules)
library(caret)
library(stringr)

#Función para entrenar reglas y predecir en un fold 
predecir_fold <- function(train_fold, test_fold, supp = 0.001, conf = 0.6) {
  
  # Discretización
  train_rules <- train_fold %>%
    mutate(across(starts_with("PC"), ~ cut(.x, breaks = 4, labels = FALSE, include.lowest = TRUE), .names = "bin_{.col}")) %>%
    mutate(across(c(key, mode, time_signature, main_genre, starts_with("bin_"), Class), as.factor)) %>%
    select(key, mode, time_signature, main_genre, starts_with("bin_"), Class)
  
  transacciones <- as(train_rules, "transactions")
  
  # Generar reglas
  reglas <- apriori(transacciones,
                    parameter = list(supp = supp, conf = conf, minlen = 2, target = "rules"),
                    appearance = list(rhs = grep("^Class=", itemLabels(transacciones), value = TRUE),
                                      default = "lhs"),
                    control = list(verbose = FALSE),
                    maxlen = 15)
  
  # Filtrar reglas útiles
  reglas_utiles <- subset(reglas, size(lhs) <= 5 & confidence > 0.7 & lift > 1.1)
  
  # Predicción en el test_fold
  df_binned <- test_fold %>%
    mutate(across(starts_with("PC"), ~ cut(.x, breaks = 4, labels = FALSE, include.lowest = TRUE), .names = "bin_{.col}")) %>%
    mutate(across(c(key, mode, time_signature, main_genre, starts_with("bin_")), as.factor))
  
  items_validos <- itemLabels(reglas_utiles)
  predicciones <- character(nrow(df_binned))
  
  for (i in 1:nrow(df_binned)) {
    items_instancia <- paste0(names(df_binned), "=", as.character(df_binned[i, ]))
    items_instancia <- items_instancia[items_instancia %in% items_validos]
    
    reglas_aplicables <- subset(reglas_utiles, subset = lhs %in% items_instancia)
    
    if (length(reglas_aplicables) > 0) {
      mejor_regla <- reglas_aplicables[which.max(quality(reglas_aplicables)$confidence)]
      predicciones[i] <- gsub("\\{Class=|\\}", "", labels(rhs(mejor_regla)))
    } else {
      predicciones[i] <- as.character(df_binned$main_genre[i])
    }
  }
  
  # Evaluación con caret
  verdaderas <- factor(test_fold$Class)
  predichas <- factor(predicciones, levels = levels(verdaderas))
  
  kappa <- confusionMatrix(predichas, verdaderas)$overall["Kappa"]
  return(kappa)
}


set.seed(777)
folds <- createFolds(train_pca$Class, k = 7) #Crear folds estratificados

#Ejecutar CV
kappas <- sapply(folds, function(test_indices) {
  test_fold <- train_pca[test_indices, ]
  train_fold <- train_pca[-test_indices, ]
  predecir_fold(train_fold, test_fold)
})

mean_kappa <- mean(kappas)
mean_kappa



# ----> Uso de Naïve Bayes con submuestreo estratificado
library(dplyr)
library(caret)
library(e1071)

train_copy<-train_clean
test_copy<-test_clean

#Submuestreo estratificado (50%) del dataset
set.seed(777)
subtrain <- train_clean %>%
  group_by(Class) %>%
  sample_frac(0.5) %>%
  ungroup()

vars_numericas <- c("Popularity", "danceability", "energy", "loudness",
                    "speechiness", "acousticness", "instrumentalness", 
                    "liveness", "valence", "tempo", "duration_minutes")

#5 folds estratificados
set.seed(17)
folds <- createFolds(subtrain$Class, k = 5)
kappas <- numeric(length(folds)) #vector para guardar el Kappa de cada fold

#Loop sobre los folds
for (i in seq_along(folds)) {
  
  #Dividir en train y test para este fold
  test_idx <- folds[[i]]
  train_fold <- subtrain[-test_idx, ]
  test_fold  <- subtrain[test_idx, ]
  
  #Aplicar PCA solo a train_fold
  preproc <- preProcess(train_fold[, vars_numericas], method = c("center", "scale", "pca"))
  
  #Transformar train y test con el preprocesador (mismo espacio PCA)
  train_pca <- predict(preproc, train_fold[, vars_numericas])
  test_pca  <- predict(preproc, test_fold[, vars_numericas])
  
  #Añadir la variable objetivo (Class)
  train_pca$Class <- train_fold$Class
  test_pca$Class  <- test_fold$Class
  
  #Entrenar Naïve Bayes
  modelo_nb <- naiveBayes(Class ~ ., data = train_pca)
  
  #Predecir y evaluar
  pred <- predict(modelo_nb, test_pca)
  cm <- confusionMatrix(pred, test_pca$Class)
  kappas[i] <- cm$overall["Kappa"]
  
  cat("Fold", i, "- Kappa:", round(kappas[i], 4), "\n")
}


mean_kappa <- mean(kappas)
cat("\nKappa promedio (Naïve Bayes, PCA, submuestreo 50%):", round(mean_kappa, 4), "\n")



# ---> Bayes pero sin PCA y con 10-Fold CV <---

set.seed(456)
folds <- createFolds(subtrain$Class, k = 10)
kappas_no_pca <- numeric(length(folds))

for (i in seq_along(folds)) {
  
  test_idx <- folds[[i]]
  train_fold <- subtrain[-test_idx, ]
  test_fold  <- subtrain[test_idx, ]
  
  #Escalar y centrar
  preproc <- preProcess(train_fold[, vars_numericas], method = c("center", "scale"))
  train_scaled <- predict(preproc, train_fold[, vars_numericas])
  test_scaled  <- predict(preproc, test_fold[, vars_numericas])
  
  #Agregar variable objetivo
  train_scaled$Class <- train_fold$Class
  test_scaled$Class  <- test_fold$Class
  
  #Entrenar Naïve Bayes
  modelo_nb <- naiveBayes(Class ~ ., data = train_scaled)
  
  #Predecir y evaluar
  pred <- predict(modelo_nb, test_scaled)
  cm <- confusionMatrix(pred, test_scaled$Class)
  kappas_no_pca[i] <- cm$overall["Kappa"]
  
  cat("Fold", i, "- Kappa:", round(kappas_no_pca[i], 4), "\n")
}


mean_kappa_no_pca <- mean(kappas_no_pca)
cat("\nKappa promedio (Naïve Bayes, sin PCA, 10-fold):", round(mean_kappa_no_pca, 4), "\n")



# ==== Mejoras ====
#Técnicamente lo que hice con Naïve bayes anteriormente fue una mejora del modelo
#pero prefiero mantener esta división ya que a continuación vendrán las mejoras más significativas

# ----> Bayes con vars categóricas
library(dplyr)
library(caret)
library(e1071)

# Recuperar main_genre
str(train_clean)
str(test_clean)

artist_genre_counts <- train_clean %>%
  count(Artist.Name, Class) %>%
  group_by(Artist.Name) %>%
  mutate(prop = n / sum(n)) %>%
  ungroup()

main_genre_artist <- artist_genre_counts %>%
  group_by(Artist.Name) %>%
  slice_max(prop, n = 1, with_ties = FALSE) %>%
  select(Artist.Name, main_genre = Class)

train_clean <- train_clean %>%
  left_join(main_genre_artist, by = "Artist.Name")

str(train_clean)

#Algunos artistas en test no estarán en train. Para esos casos:
test_clean <- test_clean %>%
  left_join(main_genre_artist, by = "Artist.Name") %>%
  mutate(main_genre = ifelse(is.na(main_genre), "Unknown", main_genre),
         main_genre = as.factor(main_genre))


# ->Bayes
set.seed(123)
subtrain <- train_clean %>%
  group_by(Class) %>%
  sample_frac(0.5) %>%
  ungroup()

vars_numericas <- c("Popularity", "danceability", "energy", "loudness",
                    "speechiness", "acousticness", "instrumentalness", 
                    "liveness", "valence", "tempo", "duration_minutes")

vars_categoricas <- c("key", "mode", "time_signature", "main_genre")

#10 folds estratificados
set.seed(456)
folds <- createFolds(subtrain$Class, k = 10)
kappas_nb_cat <- numeric(length(folds))


for (i in seq_along(folds)) {
  
  test_idx <- folds[[i]]
  train_fold <- subtrain[-test_idx, ]
  test_fold  <- subtrain[test_idx, ]
  
  # Preprocesamiento 
  preproc <- preProcess(train_fold[, vars_numericas], method = c("center", "scale"))
  train_num <- predict(preproc, train_fold[, vars_numericas])
  test_num  <- predict(preproc, test_fold[, vars_numericas])
  
  train_final <- cbind(train_num, train_fold[, vars_categoricas], Class = train_fold$Class)
  test_final  <- cbind(test_num,  test_fold[, vars_categoricas],  Class = test_fold$Class)
  
  #Asegurarse de que las categóricas sean factores
  train_final <- train_final %>%
    mutate(across(all_of(vars_categoricas), as.factor))
  
  test_final <- test_final %>%
    mutate(across(all_of(vars_categoricas), as.factor))
  
  #Entrenar Naïve Bayes
  modelo_nb <- naiveBayes(Class ~ ., data = train_final)
  
  pred <- predict(modelo_nb, test_final)
  cm <- confusionMatrix(pred, test_final$Class)
  kappas_nb_cat[i] <- cm$overall["Kappa"]
  
  cat("Fold", i, "- Kappa:", round(kappas_nb_cat[i], 4), "\n")
}


mean_kappa_nb_cat <- mean(kappas_nb_cat)
cat("\nKappa promedio (Naïve Bayes, categóricas incluidas, 10-fold):", round(mean_kappa_nb_cat, 4), "\n")



# ----> SVM
library(dplyr)
library(caret)
library(e1071)

#Submuestreo estratificado al 50%
set.seed(123)
subtrain <- train_clean %>%
  group_by(Class) %>%
  sample_frac(0.5) %>%
  ungroup()

#Variables numéricas + categóricas
vars_utiles <- c("Popularity", "danceability", "energy", "loudness", "speechiness",
                 "acousticness", "instrumentalness", "liveness", "valence", "tempo",
                 "duration_minutes", "key", "mode", "time_signature", "main_genre")

#10 folds estratificados
set.seed(456)
folds <- createFolds(subtrain$Class, k = 10)
kappas_svm_cat <- numeric(length(folds))


for (i in seq_along(folds)) {
  
  test_idx <- folds[[i]]
  train_fold <- subtrain[-test_idx, ]
  test_fold  <- subtrain[test_idx, ]
  
  #one-hot para categóricas
  dummy_encoder <- dummyVars(Class ~ ., data = train_fold[, c(vars_utiles, "Class")])
  
  train_encoded <- predict(dummy_encoder, newdata = train_fold)
  test_encoded  <- predict(dummy_encoder, newdata = test_fold)
  
  train_encoded <- as.data.frame(train_encoded)
  test_encoded  <- as.data.frame(test_encoded)
  
  #Centrado y escalado
  scaler <- preProcess(train_encoded, method = c("center", "scale"))
  train_scaled <- predict(scaler, train_encoded)
  test_scaled  <- predict(scaler, test_encoded)
  
  #gregar variable objetivo
  train_scaled$Class <- train_fold$Class
  test_scaled$Class  <- test_fold$Class
  
  #Entrenar SVM
  modelo_svm <- train(
    Class ~ ., data = train_scaled,
    method = "svmRadial",
    trControl = trainControl(method = "none"),
    tuneLength = 1  # rápido para primera prueba
  )
  
  pred <- predict(modelo_svm, test_scaled)
  cm <- confusionMatrix(pred, test_scaled$Class)
  kappas_svm_cat[i] <- cm$overall["Kappa"]
  
  cat("Fold", i, "- Kappa:", round(kappas_svm_cat[i], 4), "\n")
}


mean_kappa_svm_cat <- mean(kappas_svm_cat)
cat("\nKappa promedio (SVM, categóricas incluidas, 10-fold):", round(mean_kappa_svm_cat, 4), "\n")





#xd