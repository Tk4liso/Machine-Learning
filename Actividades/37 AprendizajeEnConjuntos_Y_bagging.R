# ==== Introducción al aprendizaje por conjuntos ====
set.seed(10)
y<-c(1:1000) 
x1<-c(1:1000)*runif(1000,min=0,max=2) 
x2<-(c(1:1000)*runif(1000,min=0,max=2))^2 
x3<-log(c(1:1000)*runif(1000,min=0,max=2)) 

lm_fit<-lm(y ~ x1 + x2 + x3)
summary(lm_fit)

set.seed(10) 
all_data<-data.frame(y,x1,x2,x3) 
positions <- sample(nrow(all_data),size=floor((nrow(all_data)/4)*3)) 
training<- all_data[positions,] 
testing<- all_data[-positions,] 
lm_fit<-lm(y~x1+x2+x3,data=training) 
predictions<-predict(lm_fit,newdata=testing) 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error 

library(foreach) 
length_divisor<-6 
iterations<-5000

predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor))) 
  train_pos<-1:nrow(training) %in% training_positions 
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,]) 
  predict(lm_fit,newdata=testing) 
} 

predictions<-rowMeans(predictions) 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error

#Aplicar Random Forest
library(randomForest) 
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500) 
predictions<-predict(rf_fit,newdata=testing) 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error 


length_divisor<-6 
iterations<-5000 

predictions<-foreach(m=1:iterations,.combine=cbind) %do% {
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor))) 
  train_pos<-1:nrow(training) %in% training_positions  
  lm_fit<-lm(y~x1+x2+x3,data=training[train_pos,]) 
  predict(lm_fit,newdata=testing) 
} 
lm_predictions<-rowMeans(predictions) 

library(randomForest) 
rf_fit<-randomForest(y~x1+x2+x3,data=training,ntree=500) 
rf_predictions<-predict(rf_fit,newdata=testing) 
predictions<-(lm_predictions+rf_predictions)/2 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error 


# ----> Mejorando el rendimiento
predictions<-(lm_predictions+rf_predictions*9)/10 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error 

#Aplicar SVM
library(e1071) 
svm_fit<-svm(y~x1+x2+x3,data=training) 
svm_predictions<-predict(svm_fit,newdata=testing) 
error<-sqrt((sum((testing$y-svm_predictions)^2))/nrow(testing)) 
error 

#Usar SVM con la función de bagging
length_divisor<-6 
iterations<-5000 

predictions<-foreach(m=1:iterations,.combine=cbind) %do% { 
  training_positions <- sample(nrow(training), size=floor((nrow(training)/length_divisor))) 
  train_pos<-1:nrow(training) %in% training_positions 
  svm_fit<-svm(y~x1+x2+x3,data=training[train_pos,]) 
  predict(svm_fit,newdata=testing) 
} 

svm2_predictions<-rowMeans(predictions) 
error<-sqrt((sum((testing$y-svm2_predictions)^2))/nrow(testing)) 
error 

predictions<-(svm_predictions+rf_predictions)/2 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error 

predictions<-(svm_predictions*2+rf_predictions)/3 
error<-sqrt((sum((testing$y-predictions)^2))/nrow(testing)) 
error 



# ==== Meta aprendizaje: Boosting y bagging para pronósticos de series temporales ====
library(tidyverse) 
library(tsibble) 
library(readxl) 

df <- read_excel("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\37 gasoline.xlsx") 
str(df) 

#Building the dataset
df_tidy <- df %>%
  mutate(gasoline = case_when(
    gasoline - lag(gasoline) < 0 ~ "down",
    gasoline - lag(gasoline) > 0 ~ "up",
    TRUE ~ "steady"
  ) %>% as.factor(),
  xe_lag = lag(xe),
  brent_lag = lag(brent),
  date = as.Date(date)) %>%
  as_tibble() %>%
  complete(date = seq(min(date), max(date), by = "day")) %>%  # Fill gaps for all days
  fill(gasoline, xe, brent, .direction = "down") %>%  # Fill missing values for the columns
  na.omit() %>%
  as.data.frame()


#Treemap of factor 
library(treemap) 

df_tidy %>%
  count(gasoline) %>% 
  mutate(label=paste(gasoline,scales::percent(n/sum(n)),sep = "\n")) %>% 
  treemap(
    index="label", 
    vSize="n", 
    title="", 
    palette="Accent", 
    border.col=c("black"), 
    border.lwds=1, 
    fontcolor.labels="white", 
    fontface.labels=1, 
    inflate.labels=F) 


library(adabag) #Boosting 
library(ipred) #Bagging 
library(caret) #Bagging control object 
library(vcd) #Kappa 
library(plotly) #interactive plot 

#Bagging 
ctrl <- trainControl(method = "cv", number = 10) 
kappa_bagg <-
  lapply(
    1:20, 
    function(x){
      set.seed(x) 
      train(gasoline ~ .,
            data = df_tidy, 
            method = "treebag", 
            trControl = ctrl)$results[["Kappa"]]} 
    ) %>%  
    unlist()


#Boosting 
kappa_boost <-
  lapply(
    1:20, 
    function(x){
      set.seed(x) 
      boosting.cv(gasoline ~ ., data = df_tidy) %>%
        
        .$confusion %>% 
        Kappa() %>%  
        .$Unweighted %>% 
        .[[1]]} 
    ) %>% 
  unlist() 


#Kappa simulation on a plotly chart  
kappa_plot <-
  data.frame(
    seed=rep(1:20, 2),  
    kappa= c(kappa_bagg, kappa_boost),  
    ensembles=c(rep("Bagging", 20), rep("Boosting", 20)) 
    ) %>%
  ggplot(aes(x=seed, y=kappa, color=ensembles))+ 
  geom_point(size=3)+  
  geom_line(size=1)+  
  theme_minimal()+  
  theme(panel.background = element_rect(fill = "midnightblue", color = NA),
        panel.grid.minor.y = element_blank(),   
        panel.grid.minor.x = element_blank())


ggplotly(kappa_plot) %>%
  layout(legend=list(orientation="v", y=0.5)) 










#xd