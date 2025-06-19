#Ejemplo de regresión logística

# ==== Regresión logistica ====

churn_data<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\16 insurance_churn.csv")
prop.table(table(churn_data$churn))

churn_model <- glm(churn ~ . - member_id, data = churn_data, family = binomial(link = "logit")) 
summary(churn_model)

churn_test<-read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\16 insurance_churn_test.csv")

churn_test$churn_prob<-predict(churn_model, churn_test, type="response")
summary(churn_test$churn_prob)

churn_order<-order(churn_test$churn_prob, decreasing = TRUE)
head(churn_test[churn_order, c("member_id","churn_prob")], n=5)


# ==== Regresión lineal ====
 
lin_model<-lm(churn ~ .,- member_id, data = churn_data)
churn_test$churn_pred_lin <- predict(lin_model, churn_test)

#Convertir predicciones a probabilidades
churn_test$churn_prob_lin <- pmin(pmax(churn_test$churn_pred_lin, 0), 1)
churn_test$churn_prob_lin

# ==== Comparacción lineal vs logistica ====
head(churn_test$churn_prob)       #logistica
head(churn_test$churn_prob_lin)   #lineal

summary(churn_test$churn_prob)      #logistica
summary(churn_test$churn_prob_lin)  #lineal






#xd