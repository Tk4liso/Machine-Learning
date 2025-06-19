#OCR con SVM

letters<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\20 letterdata.csv", stringsAsFactors = TRUE)
str(letters)

#Dividir los daros en 80:20 (ya están aleatorizados)
letters_train<-letters[1:16000,] 
letters_test<-letters[16001:20000,] 

#Entrenamiento del modelo
library(kernlab)

letter_classifier <- ksvm(letter ~ ., data = letters_train,kernel = "vanilladot") 
letter_classifier

#Evaluación del modelo
letter_predictions <- predict(letter_classifier, letters_test) 
head(letter_predictions)

table(letter_predictions, letters_test$letter)

agreement<-letter_predictions==letters_test$letter
table(agreement)
prop.table(table(agreement))

#Mejorar el rendimiento del modelo
RNGversion("3.5.2")
set.seed(12345)
letter_classifier_rbf<-ksvm(letter ~ ., data=letters_train, kernel="rbfdot")

letter_predictions_rbf<-predict(letter_classifier_rbf, letters_test)  #Predicciones
agreement_rbf<-letter_predictions_rbf == letters_test$letter          #Precisión
table(agreement_rbf)
prop.table(table(agreement_rbf))


#Identificación del mejor parámetro de costo de SVM
cost_values<-c(1, seq(from=5, to=40, by=5))
RNGversion("3.5.2")
accuracy_values<-sapply(cost_values, function(x){
  set.seed(12345)
  m<-ksvm(letter ~., data=letters_train, kernel="rbfdot", C=x)
  
  pred<-predict(m, letters_test)
  agree<-ifelse(pred==letters_test$letter,1,0)
  accuracy<-sum(agree)/nrow(letters_test)
  return(accuracy)
})

plot(cost_values,accuracy_values,type = "b")












#xd