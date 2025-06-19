#Análisis exploratorio de datos

#C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\

#Tipos de datos R 
as.integer(10.0)
as.character(10.1) 
10 == 2 

#Analizar una única variable de datos 
marketing <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0.5 marketing.csv", stringsAsFactors = TRUE) 
str(marketing) 

marketing$pop_density <- factor(marketing$pop_density, 
                                ordered = TRUE, 
                                levels = c("Low", "Medium", "High")) 

#Exploración tabular 
summary(marketing$google_adwords) 

mean(marketing$google_adwords) 
sd(marketing$google_adwords) 
var(marketing$google_adwords) 

summary(marketing$pop_density) 

#Exploración gráfica
data("anscombe") 
anscombe 

sapply(anscombe, mean) 
sapply(anscombe, sd) 
sapply(anscombe, var)

plot(anscombe) 
plot(anscombe$y1,anscombe$x1) 
plot(anscombe$y1,anscombe$x1, col="green", pch=19, cex=1.5) 

plot(marketing$pop_density) 

boxplot(marketing$google_adwords, ylab = "Expenditures") 
hist(marketing$google_adwords, main = NULL) 

summary(marketing$twitter)
boxplot(marketing$twitter, ylab = "Expenditures", col = "gray") 
hist(marketing$twitter, main = NULL, col = "blue")

#Análisis de dos variables juntas
summary(marketing)
marketing$emp_factor <- cut(marketing$employees, 2) 
marketing$emp_factor <- NULL 

table(marketing$emp_factor, marketing$pop_density) 

mosaicplot(table(marketing$pop_density, marketing$emp_factor), 
           col = c("gray","black"), main = "Factor / Factor") 

boxplot(marketing$marketing_total ~ marketing$pop_density, 
          main = "Factor / Numeric") 

plot(marketing$google_adwords, marketing$revenues, 
       main = "Numeric / Numeric") 


cor(marketing$google_adwords, marketing$revenues) 
cor(marketing$google_adwords, marketing$facebook) 

cor.test(marketing$google_adwords, marketing$revenues) 

cor.test(marketing$twitter, marketing$revenues) 
cor.test(marketing$facebook, marketing$revenues) 

cheese <- c(9.3, 9.7, 9.7, 9.7, 9.9, 10.2, 10.5, 11, 10.6, 10.6) 
degrees <- c(480, 501, 540, 552, 547, 622, 655, 701, 712, 708) 
cor(cheese, degrees) 
cor.test(cheese, degrees) 
cor.test(marketing$google_adwords, marketing$facebook)

plot(marketing$google_adwords, marketing$revenues) 
plot(marketing$google_adwords, marketing$facebook) 
plot(marketing$marketing_total, marketing$revenues)

marketing$emp_factor <- NULL 


#Correlación
cor(marketing[,1:6]) 
cor(marketing$google_adwords, marketing$revenues) 
cor(marketing$google_adwords, marketing$facebook) 

library(psych) 
corr.test (marketing[ ,1:6]) 

library(corrgram) 
corrgram(marketing[ ,1:6], order = FALSE, 
           main = "Correlogram of Marketing Data, Unordered", 
           lower.panel = panel.conf, upper.panel = panel.ellipse, 
           diag.panel = panel.minmax, text.panel = panel.txt) 

corrgram(marketing[ ,1:6], order = TRUE, 
         main = "Correlogram of Marketing Data, Ordered", 
         lower.panel = panel.shade, upper.panel = panel.pie, 
         diag.panel = panel.minmax, text.panel = panel.txt) 


bike<-read.csv("clean_bike_sharing_data.csv", stringsAsFactors = TRUE) 
bike$season <- factor(bike$season, ordered = TRUE, 
                        levels = c("spring","summer","fall","winter")) 
bike$weather <- factor(bike$weather, ordered = TRUE, 
                         levels = c("clr_part_cloud", 
                                        "mist_cloudy", 
                                        "lt_rain_snow", 
                                        "hvy_rain_snow")) 

library(lubridate) 
bike$datetime <- ymd_hms(bike$datetime)











#xd