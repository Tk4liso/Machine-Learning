#Ejemplo del árbol de decisión (Identificación de préstamos bancarios riesgosos mediante árboles de decisión C5.0 )

#plot(modelo)

library(C50)

credit<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\8 credit.csv", stringsAsFactors = TRUE)
str(credit)

table(credit$checking_balance)
table(credit$savings_balance)

summary(credit$months_loan_duration)
summary(credit$amount)

table(credit$default)

# ==== Preparación de los datos ====
set.seed(9829)
train_sample<-sample(1000,900)
str(train_sample)

credit_train<-credit[train_sample,]
credit_test<-credit[-train_sample,]

prop.table(table(credit_train$default))
prop.table(table(credit_test$default))


# ==== Entrenamiento y prueba del modelo ====
credit_model<-C5.0(default ~ .,data=credit_train)
credit_model
summary(credit_model)

#Evaluación
library(gmodels)

credit_pred<-predict(credit_model, credit_test)
CrossTable(credit_test$default, credit_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))


#Mejora (boosting) del desempeño del modelo
credit_boost10<-C5.0(default ~ ., data=credit_train,
                     trails=10)
credit_boost10
summary(credit_boost10)

credit_boost_pred10<-predict(credit_boost10, credit_test)
CrossTable(credit_test$default, credit_boost_pred10,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

#Matríz de costos
matrix_dimensions<-list(c("no","yes"), c("no","yes"))
names(matrix_dimensions)<-c("predicted","actual")
matrix_dimensions

error_cost<-matrix(c(0,1,4,0),nrow = 2, dimnames = matrix_dimensions) #Supongamos que creemos que un incumplimiento de préstamo le cuesta al banco cuatro veces más que una oportunidad perdida
error_cost

credit_cost<-C5.0(default ~ ., data = credit_train,
                  costs=error_cost)
credit_cost_pred<-predict(credit_cost, credit_test)

CrossTable(credit_test$default, credit_cost_pred,
           prop.chisq = FALSE, prop.c = FALSE, prop.r = FALSE,
           dnn = c('actual default', 'predicted default'))

plot(credit_cost)
summary(credit_cost)





#