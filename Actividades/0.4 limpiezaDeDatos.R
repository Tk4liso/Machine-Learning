#Limpieza de datos

bike <- read.csv("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\0.4 raw_bikeshare_data.csv", stringsAsFactors = FALSE) 
str(bike)
table(is.na(bike))
dim(table)

library(stringr)

str_detect(bike,"NA")
bad_data <- str_subset(bike$humidity, "[a-z A-Z]")
bad_data

#Corrección de errores en conjuntos de datos 
location <- str_detect(bike$humidity, bad_data) 
bike[location, ] 
bike$humidity <- str_replace_all(bike$humidity, bad_data, "61") 
bike[location, ] 

#Conversión de entradas a tipos de datos adecuados para el análisis
bike$humidity <- as.numeric(bike$humidity)
bike$holiday <- factor(bike$holiday, levels = c(0, 1), 
                       labels = c("no", "yes")) 

bike$workingday <- factor(bike$workingday, levels = c(0, 1), 
                            labels = c("no", "yes"))

bike$season <- factor(bike$season, levels = c(1, 2, 3, 4), 
                      labels = c("spring", "summer", "fall", "winter"), 
                      ordered = TRUE) 

bike$weather <- factor(bike$weather, levels = c(1, 2, 3, 4), 
                       labels = c("clr_part_cloud","mist_cloudy","lt_rain_snow","hvy_rain_snow"), 
                       ordered = TRUE) 

#Conversiones de fecha y hora
library(lubridate)

bike$datetime <- mdy_hm(bike$datetime) 
str(bike)

#Adaptación de variables de cadena a un estándar 
unique(bike$sources) 

bike$sources <- tolower(bike$sources) 
bike$sources <- str_trim(bike$sources) 
na_loc <- is.na(bike$sources) 
bike$sources[na_loc] <- "unknown"

unique(bike$sources) 


library(DataCombine) 

web_sites <- "(www.[a-z]*.[a-z]*)" 
current <- unique(str_subset(bike$sources, web_sites)) 
replace <- rep("web", length(current))

replacements <- data.frame(from = current, to = replace)
replacements

bike <- FindReplace(data = bike, Var = "sources", replacements, 
                    from = "from", to = "to", exact = FALSE) 
unique(bike$sources) 
bike$sources <- as.factor(bike$sources) 
str(bike) 

write.csv(bike,"clean_bike_sharing_data.csv", 
          row.names = FALSE) 








#xd