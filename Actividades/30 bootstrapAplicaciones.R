#Aplicaciones con bootstrap


# ==== Introducción al Bootstrap con aplicaciones a modelos de efectos mixtos ====

set.seed(20151101) 
height<-rnorm(100,175,6) 

t0<-median(height) #se usará la altura de 100 personas
t<-sapply(1:1000,function(x) median(sample(x=height,size=100,replace=TRUE))) 
hist(t) 
abline(v=t0,col="orange",lwd=3) 

# -> Bootstrap no paramétrico con boot
library(boot)

b1<-boot(data=height, statistic = function(x,i) median(x[i]), R=100)
boot.ci(b1)

# -> Bootstrap paramétrico con boot
x<-runif(100, -2, 2)
y<-rnorm(100, 1+2*x, 1)
dat<-data.frame(x=x, y=y)

m<-lm(y ~ x)

#Obtener los intervalos de confianza para el coeficiente del modelo
foo<-function(out){
  m<-lm(y~x, out)
  coef(m)
}

#Genera un nuevo vector de respuesta a partir del modelo
rgen<-function(dat,mle){
  out<-dat
  out$y<-unlist(simulate(mle))
  return(out)
}

#Generar 1000 muestras de bootstrap
b2<-boot(dat, foo, R=1000, sim="parametric", ran.gen=rgen, mle=m)

#Calcular los intervalos de confianza para los dos coeficientes
boot.ci(b2, type="perc", index=1)
boot.ci(b2, type="perc", index=2) 


# ----> Bootstrap aplicado a modelos de efectos mixtos
library(lme4)

dat<-data.frame(x=runif(100,-2,2), ind=gl(n=10, k=10))
dat$y<-1+2 * dat$x + rnorm(10,0,1.2)[dat$ind] + rnorm(100,0,0.5)
m<-lmer(y~x + (1|ind), dat)

#Obtener los intervalos de confianza bootstrap para los parámetros del modelo
b_par<-bootMer(x=m, FUN = fixef, nsim=200)
boot.ci(b_par, type = "perc", index = 1)
boot.ci(b_par, type = "perc", index = 2)

confint(m,parm=c(3,4),method="boot",nsim=200,boot.type="perc") 

#Obtener los intervalos de confianza de bootstrap alrededor de las curvas de regresión
new_dat<-data.frame(x=seq(-2,2,length=20))
mm<-model.matrix(~x, data=new_dat)
predFun<-function(.)mm%*%fixef(.)
bb<-bootMer(m, FUN = predFun, nsim = 200) #do this 200 times

bb_se<-apply(bb$t,2,function(x) x[order(x)][c(5,195)]) 
new_dat$LC<-bb_se[1,] 
new_dat$UC<-bb_se[2,] 
new_dat$pred<-predict(m,newdata=new_dat,re.form=~0) 

plot(y~x,dat,pch=16) 
lines(pred~x,new_dat,lwd=2,col="orange") 
lines(LC~x,new_dat,lty=2,col="orange") 
lines(UC~x,new_dat,lty=2,col="orange") 

#Obtener los p-valores bootstrap de la prueba de razón de verosimilitud entre dos modelos
library(pbkrtest) 

m_0<-update(m,.~.-x) 
PBmodcomp(m,m_0,nsim=200) 



# ==== Evaluación bootstrap de clústeres ====

protein <- read.table("C:\\Users\\Tacos\\OneDrive\\Documentos\\Universidad\\9. Noveno Semestre\\Máquinas de aprendizaje\\Actividades\\30 protein.txt", header=TRUE, sep="\t") 
summary(protein) 

# Use all the columns except the first (Country). 
vars.to.use <- colnames(protein)[-1]  

# Scale the data columns to be zero mean and unit variance. 
# The output of scale() is a matrix. 
pmatrix <- scale(protein[,vars.to.use])  

# optionally, store the centers and standard deviations of the original data, 
# so you can "unscale" it later. 
pcenter <- attr(pmatrix, "scaled:center") 
pscale <- attr(pmatrix, "scaled:scale") 

#Agrupar los datos mediante agrupamiento jerárquico (método de Ward)
# Create the distance matrix. 
d <- dist(pmatrix, method="euclidean")  

# Do the clustering.  
pfit <- hclust(d, method="ward.D2") 

# Plot the dendrogram. 
plot(pfit, labels=protein$Country) 
rect.hclust(pfit, k=5)

#Extraer los países de cada grupo junto con los valores de consumo de carne roja, pescado y frutas/verduras
print_clusters <- function(labels, k){
  for(i in 1:k){
    print(paste("cluster", i)) 
    print(protein[labels==i,c("Country","RedMeat","Fish","Fr.Veg")]) 
  } 
} 

groups <- cutree(pfit, k=5) # get the cluster labels 
print_clusters(groups, 5) # --- results –


#Con clusterboot()
library(fpc)
# set the desired number of clusters  
kbest.p<-5  
#   Run clusterboot() with hclust  
#   ('clustermethod=hclustCBI') using Ward's method  
#   ('method="ward"') and kbest.p clusters  
#   ('k=kbest.p'). Return the results in an object  
#   called cboot.hclust. 

cboot.hclust <- clusterboot(pmatrix,clustermethod=hclustCBI, 
                            method="ward.D2", k=kbest.p)

groups<-cboot.hclust$result$partition   
# -- results -- 
print_clusters(groups, kbest.p) 

# The vector of cluster stabilities. 
# Values close to 1 indicate stable clusters 
cboot.hclust$bootmean 
 
# The count of how many times each cluster was dissolved. By default  
# clusterboot() runs 100 bootstrap iterations. 
# Clusters that are dissolved often are unstable.  
cboot.hclust$bootbrd



#xd