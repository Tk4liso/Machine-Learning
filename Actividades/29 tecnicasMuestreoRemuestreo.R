#Técnicas de muestreo y remuestreo 

library(data.table) 

data <- fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 ccFraud.csv",header=T, verbose = FALSE, showProgress = FALSE) 
US_state <- fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 US_State_Code_Mapping.csv",header=T, showProgress = FALSE ) 
data<-merge(data, US_state, by = 'state') 
Gender_map<-fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 Gender Map.csv",header=T) 
data<-merge(data, Gender_map, by = 'gender') 

Credit_line<-fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 credit line map.csv",header=T) 
data<-merge(data, Credit_line, by = 'creditLine') 

setnames(data,"custID","CustomerID") 
setnames(data,"code","Gender") 
setnames(data,"numTrans","DomesTransc") 
setnames(data,"numIntlTrans","IntTransc") 
setnames(data,"fraudRisk","FraudFlag") 
setnames(data,"cardholder","NumOfCards") 
setnames(data,"balance","OutsBal") 
setnames(data,"StateName","State") 
str(data) 

data$creditLine <- NULL 
data$gender <- NULL 
data$state <- NULL 
data$PostalCode <- NULL 

# Ejecuta el siguiente código si deseas almacenar los datos transformados. 
write.csv(data,"C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 Credit Card Fraud Dataset.csv",row.names = FALSE) 
#Describe los datos 
str(data) 


# ----> Media poblacional
Population_Mean_P <-mean(data$OutsBal) 
cat("The average outstanding balance on cards is ",Population_Mean_P) 

# ----> Varianza poblacional
#Cuanto menor sea la varianza, más cerca estarán los números de la media, y cuanto mayor sea la varianza, más lejos estarán los números de la media

Population_Variance_P <-var(data$OutsBal) 
cat("The variance in the average outstanding balance is ",Population_Variance_P) 
cat("Standard deviation of outstanding balance is", sqrt(Population_Variance_P)) 

# ----> Media y varianza agrupadas
library(knitr) 

#creamos cinco muestras aleatorias usando sample()
set.seed(937) 
i<-1 
n<-rbind(10000,20000,40000,80000,100000) 
Sampling_Fraction<-n/nrow(data) 
sample_mean<-numeric() 
sample_variance<-numeric() 
for(i in 1:5){
  sample_100K <-data[sample(nrow(data),size=n[i], replace =FALSE, prob =NULL),] 
  sample_mean[i]<-round(mean(sample_100K$OutsBal),2) 
  sample_variance[i] <-round(var(sample_100K$OutsBal),2) 
} 

Sample_statistics <-cbind (1:5,c('10K','20K','40K','80K','100K'), sample_mean,sample_variance,round(sqrt(sample_variance),2),Sampling_Fraction) 

knitr::kable(Sample_statistics, col.names =c("S.No.", "Size","Sample_ Mean", "Sample_Variance","Sample SD","Sample_Fraction")) 


#se usa la fórmula de media y varianza agrupadas para calcular la media 
#poblacional de las cinco muestras que extrajimos de la población y luego se comparan
#con la media y la varianza poblacionales.

i<-1 
Population_mean_Num<-0 
Population_mean_Den<-0 

for(i in 1:5){
  Population_mean_Num =Population_mean_Num +sample_mean[i]*n[i] 
  Population_mean_Den =Population_mean_Den +n[i] 
}

Population_Mean_S<-Population_mean_Num/Population_mean_Den 
cat("The pooled mean ( estimate of population mean) is",Population_Mean_S) 

#Aplicamos este mismo proceso para calcular la varianza agrupada de las muestras
i<-1 
Population_variance_Num<-0 
Population_variance_Den<-0 

for(i in 1:5){
  Population_variance_Num =Population_variance_Num +(sample_variance[i])*(n[i] -1) 
  Population_variance_Den =Population_variance_Den +n[i] -1 
} 

Population_Variance_S<-Population_variance_Num/Population_variance_Den 
Population_SD_S<-sqrt(Population_Variance_S) 
cat("The pooled variance (estimate of population variance) is", Population_Variance_S) 

cat("The pooled standard deviation (estimate of population standard deviation) is", sqrt(Population_Variance_S)) 

#Comparación de la media poblacional y la varianza con cada muestra individual.
SamplingError_percent_mean<-round((Population_Mean_P -sample_mean)/Population_Mean_P,3) 
SamplingError_percent_variance<-round((Population_Variance_P -sample_variance)/Population_Variance_P,3) 

Com_Table_1<-cbind(1:5,c('10K','20K','40K','80K','100K'),Sampling_Fraction, SamplingError_percent_mean, SamplingError_percent_variance) 
knitr::kable(Com_Table_1, col.names =c("S.No.","Size","Sampling_Frac", "Sampling_Error_Mean(%)","Sampling_Error_Variance(%)")) 

#Media poblacional y diferencia de medias muestrales | Varianza poblacional y varianza muestral
SamplingError_percent_mean<-(Population_Mean_P -Population_Mean_S)/Population_Mean_P 
SamplingError_percent_variance<-(Population_Variance_P -Population_Variance_S)/Population_Variance_P 

Com_Table_2 <-cbind(Population_Mean_P,Population_Mean_S,SamplingError_percent_mean) 
Com_Table_3 <-cbind(Population_Variance_P,Population_Variance_S, SamplingError_percent_variance) 

knitr::kable(Com_Table_2) 
knitr::kable(Com_Table_3) 



# ==== Muestreo probabilístico y no probabilístico ====

# - - - -  Técnicas de Muestreo no probabilístico - - - - 

# ----> Simulación de un volado para La Ley de los Grandes Números

# Set parameters for a binomial distribution Binomial(n, p) 
# n -> no. of toss 
# p -> probability of getting a head 
n <-100 
p <-0.6 

#Create a data frame with 100 values selected samples from Binomial(1,p) 
set.seed(917); 
dt <-data.table(binomial=rbinom(n, 1, p) ,count_of_heads=0, mean=0) 

# Setting the first observation in the data frame 
ifelse(dt$binomial[1] ==1, dt[1, 2:3] <-1, 0) 

# Ejecutemos un experimento una gran cantidad de veces (hasta n) 
# y veamos cómo el promedio de caras -> probabilidad de cara convergen a un valor 
for (i in 2 :n){
  dt$count_of_heads[i] <-ifelse(dt$binomial[i] ==1, dt$count_of_heads[i]<-dt$count_of_heads[i -1]+1, dt$count_of_heads[i -1]) 
  dt$mean[i] <-dt$count_of_heads[i] /i 
}

# Plot the average no. of heads -> probability of heads at each experiment stage 
plot(dt$mean, type='l', main ="Simulation of average no. of heads", xlab="Size of Sample", ylab="Sample mean of no. of Heads") 
abline(h = p, col="red") 



# ----> Simulación del Teorema del Límite Central (TLC)
#Number of samples 
r<-5000 
#Size of each sample 
n<-10000 

# Generar una matriz de observaciones con n columnas y r filas.  
# Cada fila representa una muestra. 
lambda<-0.6 
Exponential_Samples =matrix(rexp(n*r,lambda),r) #Exponential_Samples contiene la serie de muestras i.i.d. (independientes e idénticamente distribuidas)

#suma, la media y la varianza de todas las muestras i.i.d
all.sample.sums <-apply(Exponential_Samples,1,sum) 
all.sample.means <-apply(Exponential_Samples,1,mean) 
all.sample.vars <-apply(Exponential_Samples,1,var) 

par(mfrow=c(2,2)) 
hist(Exponential_Samples[1,],col="gray",main="Distribution of One Sample") 
hist(all.sample.sums,col="gray",main="Sampling Distribution of the Sum") 
hist(all.sample.means,col="gray",main="Sampling Distribution of the Mean") 
hist(all.sample.vars,col="gray",main="Sampling Distribution of the Variance") 

#Se recomienda utilizar la siguiente distribución para validar el Teorema del Límite Central.
# no basarse en la inspección visual y realizar pruebas formales para inferir las propiedades de la distribución

#Normal_Samples =matrix(rnorm(n*r,param1,param2),r),
#Uniform_Samples =matrix(runif(n*r,param1,param2),r),  
#Poisson_Samples =matrix(rpois(n*r,param1),r),  
#Cauchy_Samples =matrix(rcauchy(n*r,param1,param2),r),  
#Bionomial_Samples =matrix(rbinom(n*r,param1,param2),r),  
#Gamma_Samples =matrix(rgamma(n*r,param1,param2),r),  
#ChiSqr_Samples =matrix(rchisq(n*r,param1),r),  
#StudentT_Samples =matrix(rt(n*r,param1),r)) 


#Do a formal test of normality on the distribution of sample means 
Mean_of_sample_means <-mean (all.sample.means) 
Variance_of_sample_means <-var(all.sample.means) 

# testing normality by Shapiro  wilk test 
shapiro.test(all.sample.means) 

#Distribución de medias muestrales con líneas de densidad normal
x <-all.sample.means 
h<-hist(x, breaks=20, col="red", xlab="Sample Means",
        main="Histogram with Normal Curve") 

xfit<-seq(min(x),max(x),length=40) 
yfit<-dnorm(xfit,mean=Mean_of_sample_means,sd=sqrt(Variance_of_sample_means)) 
yfit <-yfit*diff(h$mids[1:2])*length(x) 
lines(xfit, yfit, col="blue", lwd=2) 



# - - - -  Técnicas de Muestreo probabilístico - - - - 
library(data.table) 

data <- fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 ccFraud.csv",header=T, verbose = FALSE, showProgress = FALSE) 
US_state <- fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 US_State_Code_Mapping.csv",header=T, showProgress = FALSE ) 
data<-merge(data, US_state, by = 'state') 
Gender_map<-fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 Gender Map.csv",header=T) 
data<-merge(data, Gender_map, by = 'gender') 

Credit_line<-fread("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\29 credit line map.csv",header=T) 
data<-merge(data, Credit_line, by = 'creditLine') 

setnames(data,"custID","CustomerID") 
setnames(data,"code","Gender") 
setnames(data,"numTrans","DomesTransc") 
setnames(data,"numIntlTrans","IntTransc") 
setnames(data,"fraudRisk","FraudFlag") 
setnames(data,"cardholder","NumOfCards") 
setnames(data,"balance","OutsBal") 
setnames(data,"StateName","State") 

# -> 1. Dimensiones de los datos de población
str(data) 

# -> 2. Media de la población para las medidas
mean_outstanding_balance <- mean(data$OutsBal) 
mean_outstanding_balance 

mean_international_trans <- mean(data$IntTransc) 
mean_international_trans 

mean_domestic_trans <- mean(data$DomesTransc) 
mean_domestic_trans 

# -> 3. Varianza de la población para las medidas
Var_outstanding_balance <- var(data$OutsBal) 
Var_outstanding_balance 

Var_international_trans <- var(data$IntTransc) 
Var_international_trans 

Var_domestic_trans <- var(data$DomesTransc) 
Var_domestic_trans

# -> 4. Histograma
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance", 
     main="Distribution of Outstanding Balance") 

hist(data$IntTransc, breaks=20, col="blue", xlab="Number of International Transactions", 
     main="Distribution of International Transactions") 

hist(data$DomesTransc, breaks=20, col="green", xlab="Number of Domestic Transactions", 
     main="Distribution of Domestic Transactions") 


# ----> Muestreo aleatorio simple
library(dplyr)

#Datos de población: Distribución del saldo pendiente según el tipo de tarjeta 
summarise(group_by(data,CardType), Population_OutstandingBalance=mean(OutsBal))

set.seed(937)
#Simple Random Sampling Without Replacement 
library(base)
sample_SOR_100K<-data[sample(nrow(data), size=100000,
                             replace = FALSE, prob=NULL)]

#Sample Data : Distribution of Outstanding Balance across Card Type
summarise(group_by(sample_SOR_100K, CardType), Sample_OutstandingBalance=mean(OutsBal))

# Comprobar si los datos muestreados provienen de la población o no.  
# Esto garantiza que el muestreo no altere la distribución original. 
ks.test(data$OutsBal,sample_SOR_100K$OutsBal,alternative="two.sided") 

par(mfrow =c(1,2)) 
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance", 
     main="Population") 
hist(sample_SOR_100K$OutsBal, breaks=20, col="green", xlab="Outstanding Balance", 
     main="Random Sample Data (without replacement)") 

# Realicemos también una prueba t para la media de la población y la muestra. 
t.test(data$OutsBal, sample_SOR_100K$OutsBal) 

set.seed(937) 
# Simple Random Sampling With Replacement 
sample_SR_100K <-data[sample(nrow(data),size=100000, replace =TRUE, prob =NULL),] 

ks.test(data$OutsBal,sample_SR_100K$OutsBal,alternative="two.sided")

par(mfrow =c(1,2)) 
hist(data$OutsBal, breaks=20, col="red", xlab="Outstanding Balance", main="Population ") 
hist(sample_SR_100K$OutsBal, breaks=20, col="green", xlab="Outstanding Balance", main=" Random Sample Data ( WR)")


library(knitr) 
population_summary <-summarise(group_by(data,CardType), 
                                 OutstandingBalance_Population=mean(OutsBal)) 
random_WOR_summary<-summarise(group_by(sample_SOR_100K,CardType), 
                                OutstandingBalance_Random_WOR=mean(OutsBal)) 
random_WR_summary<-summarise(group_by(sample_SR_100K,CardType), 
                               OutstandingBalance_Random_WR=mean(OutsBal)) 
compare_population_WOR<-merge(population_summary,random_WOR_summary, 
                                by="CardType") 
compare_population_WR <-merge(population_summary,random_WR_summary, 
                                by="CardType")

summary_compare<-cbind(compare_population_WOR,compare_population_WR$OutstandingBalance_Random_WR)
colnames(summary_compare)[which(names(summary_compare) == "V2")] <- "OutstandingBalance_Random_WR"
knitr::kable(summary_compare,col.names = c("C_Type","OutBal_Population","OutBal_Random_WOR","OutBal_Random_WR"))



# ----> Muestreo aleatorio sistemático

Data_Subset <-subset(data, IntTransc==0&DomesTransc<=3) 
summarise(group_by(Data_Subset,CardType),OutstandingBalance=mean(OutsBal)) 

#Size of population ( here the size of card holders from Data Subset) 
Population_Size_N<-length(Data_Subset$OutsBal) 
# Set a the size of sample to pull (should be less than N), n. We will 
# assume n=5000 
Sample_Size_n<-5000 

#Calculate the skip factor 
k =ceiling(Population_Size_N/Sample_Size_n) 
#ceiling(x) rounds to the nearest integer thatâ<U+0080><U+0099>s larger than x. 
#This means ceiling (2.3) = 3 
cat("The skip factor for systematic sampling is ",k)

r =sample(1:k, 1) 
systematic_sample_index =seq(r, r +k*(Sample_Size_n-1), k) 

systematic_sample_5K<-Data_Subset[systematic_sample_index,]

set.seed(937) 
# Simple Random Sampling Without Replacement 
sample_Random_5K <-Data_Subset[sample(nrow(Data_Subset),size=5000, 
                                          replace =FALSE, prob =NULL),] 

sys_summary <-summarise(group_by(systematic_sample_5K,CardType), 
                        OutstandingBalance_Sys=mean(OutsBal)) 
random_summary<-summarise(group_by(sample_Random_5K,CardType), 
                          OutstandingBalance_Random=mean(OutsBal)) 
summary_mean_compare<-merge(sys_summary,random_summary, by="CardType") 
print(summary_mean_compare) 

ks.test(Data_Subset$OutsBal,systematic_sample_5K$OutsBal,alternative="two.sided") 

par(mfrow =c(1,2)) 
hist(Data_Subset$OutsBal, breaks=50, col="red", xlab="Outstanding Balance", 
       main="Homogenous Subset Data") 
hist(systematic_sample_5K$OutsBal, breaks=50, col="green", xlab="Outstanding Balance", 
       main="Systematic Sample") 



# ----> Muestreo aleatorio estratificado




















#xd