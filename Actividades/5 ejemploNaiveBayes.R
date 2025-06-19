# Ejemplo Naive Bayes (Filtrado de spam de teléfonos móviles con el algoritmo naïve Bayes )

sms_raw<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\5 sms_spam.csv")
head(sms_raw)
str(sms_raw)
summary(sms_raw)

sms_raw$type<-factor(sms_raw$type)
str(sms_raw$type)
table(sms_raw$type)

# ==== Preparación de datos - limpieza y estandarización de datos de texto ==== 
library(tm)

sms_corpus<-VCorpus(VectorSource(sms_raw$text))
print(sms_corpus)
inspect(sms_corpus[1:2]) 
as.character(sms_corpus[[1]]) 

lapply(sms_corpus[1:2], as.character) 

#Limpiar texto y estandarizar palabras
sms_corpus_clean <- tm_map(sms_corpus, 
                           content_transformer(tolower)) #Fun. para convertir a minúsculas

as.character(sms_corpus[[1]]) 
as.character(sms_corpus_clean[[1]]) 

sms_corpus_clean <- tm_map(sms_corpus_clean, removeNumbers)  #Eliminar números

sms_corpus_clean <- tm_map(sms_corpus_clean, 
                           removeWords, stopwords()) #Eliminar palabras vacías

sms_corpus_clean <- tm_map(sms_corpus_clean, removePunctuation) #Eliminar puntuación
removePunctuation("hello...world") 

#Func. personalizada para remover puntuación
replacePunctuation <- function(x) { 
     gsub("[[:punct:]]+", " ", x) 
  } 

#Derivación de palabras (reducir las palabras a su forma raíz)
library(SnowballC)

wordStem(c("learn", "learned", "learning", "learns"))
sms_corpus_clean <- tm_map(sms_corpus_clean, stemDocument)

#Eliminar espacios en blanco adicionales
sms_corpus_clean <- tm_map(sms_corpus_clean, stripWhitespace) 


#Antes vs. Después
as.character(sms_corpus[1:3])        #Antes
as.character(sms_corpus_clean[1:3])  #Después

# ==== Preparación de datos - división de documentos de texto en palabras ====

#creación de una matriz dispersa DTM a partir de un corpus tm 
sms_dtm <- DocumentTermMatrix(sms_corpus_clean) 

# crear un DTM directamente a partir del corpus SMS sin procesar
sms_dtm2 <- DocumentTermMatrix(sms_corpus, control = list( 
    tolower = TRUE, 
    removeNumbers = TRUE, 
    stopwords = TRUE, 
    removePunctuation = TRUE, 
    stemming = TRUE 
   ))

#Comparación de dtm's
sms_dtm 
sms_dtm2

#Creación de conjuntos de datos de prueba y de entrenamiento
sms_dtm_train <- sms_dtm[1:4169, ] 
sms_dtm_test <- sms_dtm[4170:5559, ] 

sms_train_labels <- sms_raw[1:4169, ]$type
sms_test_labels <- sms_raw[4170:5559, ]$type

prop.table(table(sms_train_labels)) 
prop.table(table(sms_test_labels)) 

#Visualización de datos de texto
library(wordcloud)

wordcloud(sms_corpus_clean, min.freq = 50, random.order = FALSE)

spam <- subset(sms_raw, type == "spam")
ham <- subset(sms_raw, type == "ham")

wordcloud(spam$text, max.words = 40, scale = c(3, 0.5))
wordcloud(ham$text, max.words = 40, scale = c(3, 0.5)) 

#Creación de características indicadoras para palabras frecuentes
findFreqTerms(sms_dtm_train, 5) 
sms_freq_words <- findFreqTerms(sms_dtm_train, 5) 
str(sms_freq_words) 

sms_dtm_freq_train <- sms_dtm_train[ , sms_freq_words]
sms_dtm_freq_test <- sms_dtm_test[ , sms_freq_words]

#convertir los recuentos en cadenas de Sí o No
convert_counts <- function(x) { 
  x <- ifelse(x > 0, "Yes", "No") 
}

sms_train <- apply(sms_dtm_freq_train, MARGIN = 2, convert_counts)
sms_test <- apply(sms_dtm_freq_test, MARGIN = 2, convert_counts) 

#Entrenamiento de un modelo con los datos
library(naivebayes)

sms_classifier <- naive_bayes(sms_train, sms_train_labels)
warnings()

#Evaluación del rendimiento del modelo
library(gmodels)

sms_test_pred <- predict(sms_classifier, sms_test) 

CrossTable(sms_test_pred, sms_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted', 'actual')) 

#Mejorar el rendimiento del modelo 
sms_classifier2 <- naive_bayes(sms_train, sms_train_labels, 
                               laplace = 1) 
sms_test_pred2 <- predict(sms_classifier2, sms_test)

CrossTable(sms_test_pred2, sms_test_labels, 
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE, 
           dnn = c('predicted', 'actual'))







#