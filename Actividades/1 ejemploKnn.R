# Ejemplo K-NN

library(class)
library(gmodels)

wbcd<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\1 wisc_bc_data.csv", stringsAsFactors = FALSE)
str(wbcd)

wbcd<-wbcd[-1]

table(wbcd$diagnosis)

#Renombrar y convertir a porcentaje
wbcd$diagnosis<-factor(wbcd$diagnosis, levels = c("B","M"), labels=c("Benign", "Malignant"))
round(prop.table(table(wbcd$diagnosis)) * 100,digits = 1)

summary(wbcd[c("radius_mean","area_mean","smoothness_mean")])

#Normalizar datos numéricos (el área mucho mayor que la suavidad puede sesgar los resultados)
normalize<-function(x){
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1,2,3,4,5))
normalize(c(10,20,30,40,50))

wbcd_n<-as.data.frame(lapply(wbcd[2:31],normalize))
summary(wbcd_n$area_mean)

# = Preparación de datos =
#Dividir conjunto en entrenamiento y pruebas
wbcd_train<-wbcd_n[1:469,]
wbcd_test<-wbcd_n[470:569,]

wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

# = Aplicar K-NN =
wbcd_test_pred<-knn(train=wbcd_train,test=wbcd_test, cl=wbcd_train_labels,k=21)
CrossTable(x=wbcd_test_labels, y=wbcd_test_pred, prop.chisq = FALSE)

#Mejorar el rendimiento
wbcd_z<-as.data.frame(scale(wbcd[-1]))
summary(wbcd_z$area_mean)

wbcd_train<-wbcd_z[1:469,]
wbcd_test<-wbcd_z[470:569,]
wbcd_train_labels<-wbcd[1:469,1]
wbcd_test_labels<-wbcd[470:569,1]

#k=21 (k sugerido)
wbcd_test_pred<-knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=21)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

#k=1
wbcd_test_pred<-knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=1)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

#k=5
wbcd_test_pred<-knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=1)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

#k=11
wbcd_test_pred<-knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=1)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

#k=15
wbcd_test_pred<-knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=15)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

#k=27
wbcd_test_pred<-knn(train = wbcd_train, test=wbcd_test, cl=wbcd_train_labels,k=27)
CrossTable(x=wbcd_test_labels,y=wbcd_test_pred, prop.chisq = FALSE)

# ----> Crear gráfico facetado <----
library(ggplot2)
library(dplyr)
library(tidyr)

# Evaluar diferentes valores de k
k_values <- c(1, 5, 11, 15, 21, 27)
results <- data.frame()

for (k in k_values) {
  #Aplicar K-NN
  wbcd_test_pred <- knn(train = wbcd_train, test = wbcd_test, cl = wbcd_train_labels, k = k)
  
  #Calcular métricas
  confusion_matrix <- table(True = wbcd_test_labels, Predicted = wbcd_test_pred)
  
  #Evitar errores si no hay clasificaciones en ciertas celdas
  false_negatives <- ifelse("Malignant" %in% rownames(confusion_matrix) & 
                              "Benign" %in% colnames(confusion_matrix),
                            confusion_matrix["Malignant", "Benign"], 0)
  false_positives <- ifelse("Benign" %in% rownames(confusion_matrix) & 
                              "Malignant" %in% colnames(confusion_matrix),
                            confusion_matrix["Benign", "Malignant"], 0)
  
  total_incorrect <- sum(false_negatives, false_positives)
  total <- sum(confusion_matrix)
  incorrect_percentage <- (total_incorrect / total) * 100
  
  #Guardar resultados
  results <- rbind(results, data.frame(
    k = k,
    False_Negatives = false_negatives,
    False_Positives = false_positives,
    Incorrect_Percentage = incorrect_percentage
  ))
}

#Ajustar datos para graficar
results_long <- results %>%
  pivot_longer(
    cols = c(False_Negatives, False_Positives, Incorrect_Percentage),
    names_to = "Metric",
    values_to = "Value"
  )

#Graficar
ggplot(results_long, aes(x = Metric, y = Value, fill = Metric)) +
  geom_bar(stat = "identity", position = "dodge") +
  facet_wrap(~ k, scales = "free_y", labeller = "label_both") +
  geom_text(aes(label = ifelse(Metric == "Incorrect_Percentage", 
                               paste0(round(Value, 1), "%"), 
                               round(Value, 0))), 
            vjust = -0.5, position = position_dodge(width = 0.9)) +
  labs(
    title = "Resultados de K-NN para diferentes valores de k",
    x = "Métrica",
    y = "Valor",
    fill = "Métrica"
  ) +
  theme_minimal()









#