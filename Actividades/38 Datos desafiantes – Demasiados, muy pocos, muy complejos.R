#Datos desafiantes – Demasiados, muy pocos, muy complejos

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# PARTE I
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ==== Ejemplo - Uso de la regresión por pasos para la selección de características  ====

titanic_train <- read_csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\38 titanic_train.csv") |> 
  mutate( 
    Age_MVI = if_else(is.na(Age), 1, 0), 
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age), 
    Cabin = if_else(is.na(Cabin), "X", Cabin), 
    Embarked = factor(if_else(is.na(Embarked), "X", Embarked)), 
    Sex = factor(Sex)
  )

simple_model <- glm(Survived ~ 1, family = binomial,
                    data = titanic_train)

full_model <- glm(Survived ~ Age + Age_MVI + Embarked + Sex + Pclass + SibSp + Fare, 
                  family = binomial, data = titanic_train) 

# -> Eliminación hacia adelante
sw_forward <- stats::step(simple_model, 
                          scope = formula(full_model), 
                          direction = "forward")

formula(sw_forward)
sw_forward$coefficients # coeficientes de regresión estimados del modelo final

# -> Eliminación hacia atrás
sw_backward <- stats::step(full_model, direction = "backward") 


# ==== Ejemplo - Uso de Boruta para la selección de características ====
#Palabras clave: Características sombra y caract. originales

set.seed(12345) 
titanic_train$rand_vals <- runif(n = 891, min = 1, max = 100) 

library(Boruta) 
titanic_boruta <- Boruta(Survived ~ PassengerId + Age +
                           Sex + Pclass + SibSp + rand_vals, 
                           data = titanic_train, doTrace = 1) 

titanic_boruta 
plot(titanic_boruta) 


# ==== Ejemplo - Uso de PCA para reducir datos de redes sociales altamente dimensionales ====

library(tidyverse) 
sns_data <- read_csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\38 snsdata.csv") 
sns_terms <- sns_data |> select(basketball:drugs) 

set.seed(2023) 
library(irlba) 
sns_pca <- sns_terms |> 
  prcomp_irlba(n = 10, center = TRUE, scale = TRUE) 

screeplot(sns_pca, npcs = 10, type = "lines",
          main = "Scree Plot of SNS Data Principal Components") 

summary(sns_pca)
str(sns_pca$x) 
head(sns_pca$x) 

sns_pca_long <- tibble(SNS_Term = colnames(sns_terms), 
                       as_tibble(sns_pca$rotation)) |>
  pivot_longer(PC1:PC10, names_to = "PC", values_to = "Contribution") 

sns_pca_long |>
  filter(PC == "PC3") |> 
  top_n(15, abs(Contribution)) |> 
  mutate(SNS_Term = reorder(SNS_Term, Contribution)) |> 
  ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) + 
  geom_col(show.legend = FALSE, alpha = 0.8) + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1, 
                                   vjust = 0.5), axis.ticks.x = element_blank()) +  
  labs(x = "Social Media Term", 
       y = "Relative Importance to Principal Component", 
       title = "Top 15 Contributors to PC3") 


# crea una función para visualizar los otros cuatro componentes 
plot_pca <- function(component) { 
  sns_pca_long |>
    filter(PC == component) |>
    top_n(15, abs(Contribution)) |> 
    mutate(SNS_Term = reorder(SNS_Term, Contribution)) |>  
    ggplot(aes(SNS_Term, Contribution, fill = SNS_Term)) + 
    geom_col(show.legend = FALSE, alpha = 0.8) +  
    theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),  
          axis.ticks.x = element_blank()) + 
    labs(x = "Social Media Term", 
         y = "Relative Importance to Principal Component", 
         title = paste("Top 15 Contributors to", component))
}

plot_pca("PC1") 
plot_pca("PC2") 
plot_pca("PC4") 
plot_pca("PC5") 


sns_data_pca <- cbind(sns_data[1:4], sns_pca$x) 

m <- lm(friends ~ PC1 + PC2 + PC3 + PC4 + PC5, data = sns_data_pca) 
m 



# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ---- PARTE II ----
# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# ==== Ejemplo - Remapeo de datos categóricos dispersos ====
library(tidyverse) 
library(forcats) 

titanic_train <- read_csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\38 titanic_train.csv") |>
  mutate(Title = str_extract(Name, ", [A-z]+\\.")) |> 
  mutate(Title = str_replace_all(Title, "[, \\.]", "")) 

table(titanic_train$Title, useNA = "ifany") 

#crear una asignación m de n
titanic_train <- titanic_train |> 
  mutate(TitleGroup = fct_collapse(Title,
                                   Mr = "Mr", 
                                   Mrs = "Mrs",  
                                   Master = "Master",  
                                   Miss = c("Miss", "Mlle", "Mme", "Ms"),  
                                   Noble = c("Don", "Sir", "Jonkheer", "Lady"),  
                                   Military = c("Capt", "Col", "Major"),  
                                   Doctor = "Dr",  
                                   Clergy = "Rev",  
                                   other_level = "Other")  
         ) |>  
  mutate(TitleGroup = fct_na_value_to_level(TitleGroup,
                                            level = "Unknown")) 

table(titanic_train$TitleGroup)
                                     
fct_count(titanic_train$Title, sort = TRUE, prop = TRUE)

table(fct_lump_n(titanic_train$Title, n = 3)) 
table(fct_lump_prop(titanic_train$Title, prop = 0.01)) 
table(fct_lump_min(titanic_train$Title, min = 5)) 



# ==== Ejemplo – Agrupamiento (clasificación, binning) de datos numéricos dispersos ====
head(titanic_train$Fare)
summary(titanic_train$Fare) 

titanic_train <- titanic_train |> mutate( 
  fare_firstclass = if_else(Fare >= 31, 1, 0, missing = 0) 
)

table(titanic_train$fare_firstclass) 

titanic_train <- titanic_train |> 
  mutate(
    fare_class = case_when( 
    Fare >= 31 ~ "1st Class", 
    Fare >= 15 ~ "2nd Class", 
    TRUE ~ "3rd Class"))
        
table(titanic_train$fare_class) 

table(cut(titanic_train$Fare, breaks = c(0, 15, 31, Inf), 
          right = FALSE)) 


table(cut(titanic_train$Fare, right = FALSE, 
          breaks = seq(from = 0, to = 550, by = 50))) 


table(cut(titanic_train$Fare, right = FALSE, 
          breaks = quantile(titanic_train$Fare,
                            probs = seq(0, 1, 0.20)))) 

table(ntile(titanic_train$Fare, n = 5)) 

titanic_train <- titanic_train |> 
  mutate(fare_level = factor(ntile(Fare, n = 11))) 

table(titanic_train$fare_level)



# ==== Imputación de valores faltantes ====

titanic_train <- titanic_train |> 
  mutate(
    Cabin = if_else(is.na(Cabin), "X", Cabin), 
    Embarked = if_else(is.na(Embarked), "Unknown", Embarked) 
    ) 

# ==== Imputación simple con indicadores de valores faltantes ====

titanic_train <- titanic_train |> 
  mutate(
    Age_MVI = if_else(is.na(Age), 1, 0), 
    Age = if_else(is.na(Age), mean(Age, na.rm = TRUE), Age)
  )


# ==== Estrategias sencillas para reequilibrar datos ====

library(tidyverse) 
library(forcats) 

snsdata <- read_csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\38 snsdata.csv") |>
  mutate( 
      gender = fct_recode(gender, Female = "F", Male = "M"), 
      gender = fct_na_value_to_level(gender, level = "Unknown"), 
    age = ifelse(age < 13 | age > 20, NA, age)
    ) |> 
  group_by(gradyear) |>  
  mutate(age_imp = if_else(is.na(age),
                           median(age, na.rm = TRUE), age)) |> 
  ungroup() |>  
  select(gender, friends, gradyear, age_imp, basketball:drugs) 

fct_count(snsdata$gender, prop = TRUE) 


library(caret) 
sns_undersample <- downSample(x = snsdata[2:40],
                                y = snsdata$gender, 
                                yname = "gender") 
fct_count(sns_undersample$gender, prop = TRUE) 

sns_oversample <- upSample(x = snsdata[2:40], 
                           y = snsdata$gender, 
                           yname = "gender") 

fct_count(sns_oversample$gender, prop = TRUE)



# ==== Ejemplo - Aplicación del algoritmo SMOTE en R  ====
library(themis) 

sns_balanced <- snsdata |> smote("gender") 

table(sns_balanced$gender) 

normalize <- function(x) {
  return ((x - min(x)) / (max(x) - min(x))) 
} 

unnormalize <- function(norm_vals, col_name) { 
  
  old_vals <- snsdata[col_name] 
  unnormalized_vals <- norm_vals * (max(old_vals) - min(old_vals)) + min(old_vals) 
  rounded_vals <- if(col_name != "age_imp"){ 
    round(unnormalized_vals)
    } 
    else {unnormalized_vals} 
  
  return (rounded_vals) 
  
} 

snsdata_balanced <- snsdata |> 
  mutate(across(where(is.numeric), normalize)) |> 
  smote("gender") |> 
  mutate(across(where(is.numeric), ~unnormalize(.x, cur_column()))) 

table(snsdata$gender)
table(snsdata_balanced$gender)



#xd