#Otro ejemplo de regresión logística
library(tidyverse)

heart_data<-readr::read_csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\18 heart_data.csv")

#Enfermedad cardiaca vs gasto anual en comida rápida
ggplot(heart_data) +
  geom_point(aes(fast_food_spend, factor(heart_disease)), color="blue") +
  labs(x='Fast Food Spend', y='Heart Disease')

#Regresión lineal - ¿Qué probabilidad hay de que una persona sufra una enfermedad cardíaca a medida que aumenta el gasto en comida rápida?
lin_reg_model<-lm(heart_disease ~ fast_food_spend, data=heart_data)
summary(lin_reg_model)

ggplot(heart_data) +
  geom_point(aes(fast_food_spend, heart_disease), color="blue") +
  geom_abline(slope = coef(lin_reg_model)[2], intercept = coef(lin_reg_model)[1], color="red") +
  labs(x=",y=")

plot(lin_reg_model, which = 1)
plot(lin_reg_model, which = 2)

#Regresión logística - Examinación de los datos
heart_data %>% 
  dplyr::mutate(heart_disease=ifelse(heart_disease ==1, "Yes","No")) %>%
  ggplot(aes(x=heart_disease, y=fast_food_spend, fill=heart_disease)) +
  geom_boxplot() +
  xlab("Hear Disease (Y/N)") +
  ylab("Annual Fast Food Spend") +
  ggtitle("Figure 1: Food Spend and Heart Disease")

heart_data %>% 
  dplyr::mutate(heart_disease=ifelse(heart_disease==1, "Yes","No")) %>%
  ggplot(aes(x=heart_disease, y=income, fill = heart_disease)) +
  geom_boxplot() +
  xlab("Heart Disease (Y/N)") +
  ylab("Income") +
  ggtitle("Figure 2: Income and Heart Disease")

heart_data %>%  
  dplyr::mutate(heart_disease = ifelse(heart_disease == 1, "Yes", "No"), 
                   coffee_drinker = ifelse(coffee_drinker == 1, "Yes", "No")) %>%  
  dplyr::group_by(heart_disease, coffee_drinker) %>%  
  dplyr::summarise(count = n()) %>% 
  ungroup() %>%  
  dplyr::group_by(coffee_drinker) %>%  
  dplyr::mutate(total_coffee_drinkers = sum(count)) %>%  
  ungroup() %>% 
  dplyr::mutate(proportion = count / total_coffee_drinkers) %>% 
  dplyr::filter(heart_disease == "Yes") %>%  
  ggplot(aes(x = coffee_drinker, y = proportion, fill = coffee_drinker)) + 
  geom_bar(stat = "identity") + 
  theme_minimal() + 
  ylab("Percent with Heart Disease") + 
  xlab("Coffee Drinker (Y/N)") + 
  ggtitle("Figure 3: Percent of Coffee Drinkers with Heart Disease") + 
  labs(fill = "Coffee Drinker") + 
  scale_y_continuous(labels = scales::percent) 


#Ajuste del modelo
log_reg<-glm(
  formula = factor(heart_disease) ~ factor(coffee_drinker) + fast_food_spend + income,
  data=heart_data,
  family=binomial(link = 'logit')
)
summary(log_reg)

#Predecir las probabilidades de que una persona sufra una enfermedad cardiaca
test_obs<-data.frame(
  coffee_drinker=1,
  fast_food_spend=5000,
  income=60000
)

#Predicción para persona hipotética
predict(log_reg, type = "response", test_obs)

#Predicción para dataset
pred_probs<-predict(log_reg, type="response")
head(round(pred_probs,3), n=10) # Print first 10 probabilities


#Evaluación del modelo
thresh_1 <-  0.01
thresh_2 <- 0.25 
thresh_3 <- 0.50 
thresh_4 <- 1.00 

# número total de predicciones correctas realizadas, dividido por el número total de registros
accuracy_1<-sum(ifelse(pred_probs > thresh_1, 1, 0)==heart_data$heart_disease) / nrow(heart_data)
accuracy_2 <- sum(ifelse(pred_probs > thresh_2, 1, 0) == heart_data$heart_disease) / nrow(heart_data) 
accuracy_3 <- sum(ifelse(pred_probs > thresh_3, 1, 0) == heart_data$heart_disease) / nrow(heart_data) 
accuracy_4 <- sum(ifelse(pred_probs > thresh_4, 1, 0) == heart_data$heart_disease) / nrow(heart_data) 

accuracy_1 
accuracy_2 
accuracy_3 
accuracy_4 

#Matriz de confusion
# --- 0.01 Threshold ---
data.frame(
  actual=heart_data$heart_disease,
  predicted=ifelse(pred_probs > thresh_1,1,0)
) %>% table()

# Sensitivity (la proporción de verdaderos positivos)
321 / (321 + 12)

# Specificity (la proporción de verdaderos negativos)
7163 / (7163 + 2504) 

# --- 0.25 Threshold --- 
data.frame( 
  actual = heart_data$heart_disease, 
  predicted = ifelse(pred_probs > thresh_2, 1, 0) 
  ) %>% table()

# Sensitivity 
184 / (184 + 149) 

# Specificity 
9477 / (9477 + 190) 


#Curva ROC y AUC
library(PRROC)
obj<-roc.curve(pred_probs, weights.class0 = heart_data$heart_disease, curve = T)
plot(obj, color=F)







#xd