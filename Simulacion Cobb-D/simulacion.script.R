#Ejercicio Simulacion 1

#1) Creo una funcion que genere el ingreso mensual de un hogar siguiendo una distribucion chi con k grados de libertad

set.seed(73)
simular_ingreso<-function(n,k){
  ingreso<-rchisq(n,k)
  return(ingreso)}
K<-c(1:11)
n<-1
ingresos_K<-sapply(K, simular_ingreso,n=n)
plot(K, ingresos_K, type = "b", 
     xlab="k(grados de libertad)", ylab = "ingreso simulado",
     col="violet",
     pch=19,
     main = "simulacion ingreso al cambiar k ",
     cex.main= 1
)
#2) 
#defino vector de precios y preferencias
p<-c("p1","p2")
alfa<-c("alfa1","alfa2")
T<-c(1,2)
#defino funcion que devuelva demandas optimas de bienes 1 y 2 dados los precios, ingreso y preferencias
demanda_cd<-function(n,k){
  ingreso<-simular_ingreso(n,k)
  demandas<-sapply(T,function(t){
    (alfa[t]*ingreso)/p[t]})
  return(demandas)}
# defno funcion que devuelva utilidad indirecta

utilidad_indirecta <- function(n,k){
  demandas<- demanda_cd(n,k)
  a<-((demandas[,1])^(alfa[1]))*((demandas[,2])^(alfa[2]))
  return(a)}

#3) 

#defino precios y preferencias
p <- c(1,2)
alfa<- c(0.5,0.5)
k<-8
n<-10000
#Simulacion 10.000 hogares utilizando las funciones previamente definidas
set.seed(73)
dem<-demanda_cd(10000,8)
ut<- utilidad_indirecta(10000,8)
#Histograma de la distribucion de la simulacion
hist(ut, col = "pink", main = "histograma de u.indirecta")
hist(dem[,1],col = "lightblue", main = "histograma x1")
hist(dem[,2],col = "lightgreen", main = "histograma x2")

#medias y cuantiles
mediax1<-mean(dem[,1])
mediax2<-mean(dem[,2])
med_ut<- mean(ut) 
cuant1<-quantile(dem[,1])
cuant2<-quantile(dem[,2])
cuant_ut<-quantile(ut)

#4)
#defino funcion que estime la probabilidad de que el consumo de un bien sea por debajo de un umbral "C"

prob_bajo_consumo<- function(n,k,j,c){
  demandas<- demanda_cd(n,k)
  xj<- demandas[,j]
  proba<-mean(xj<c)
  return(proba)}

#5) simulo una suba de 20% en el precio del bien 1
set.seed(73)
p<-c(1.2,2)

dem2<- demanda_cd(10000,8)#nueva demanda
ut2<- utilidad_indirecta(1000,8) #nueva utilidad indirecta

#histograma para comparar con el previo al shock
hist(dem2[,1],col = "blue", main = "histograma x1 despues del shock", 
     cex.main=1)
abline(v=mean(dem2[,1],lwd=2, lty=2))
hist(dem[,1],col = "lightblue", main = "histograma x1 antes")
abline(v=mean(dem[,1],lwd=2, lty=2))

#6) Histogramas superpuestos marcando la media para facilitar interpretacion

hist(dem2[,1],col = "blue", main = "histograma antes vs despues shock", 
     cex.main=1)
hist(dem[,1],col = "lightblue",add=TRUE)
med_ut2<- mean(ut2)
print(med_ut2)
print(med_ut)
# 7)  Incorporacion de Heterogeneidad en preferencias

#defino parametros
N <- 10000

a_param <- 2
b_param <- 3
p1_base<-2
p2_base<-4
p1_shock<-1.2*p1_base
ing<- simular_ingreso(N,k)
# Genero preferencias heterogéneas
alpha1_hetero <- rbeta(N, shape1 = a_param, shape2 = b_param)
alpha2_hetero <- 1 - alpha1_hetero

# Pre-Shock 
x1_pre <- (alpha1_hetero * ing) / p1_base
x2_pre <- (alpha2_hetero * ing) / p2_base
U_pre <- (x1_pre^alpha1_hetero) * (x2_pre^alpha2_hetero)

sim_hetero_pre <- data.frame(x1 = x1_pre, x2 = x2_pre, U = U_pre)

# Post-Shock
x1_post <- (alpha1_hetero * ing) / p1_shock
x2_post <- (alpha2_hetero * ing) / p2_base
U_post <- (x1_post^alpha1_hetero) * (x2_post^alpha2_hetero)

sim_hetero_post <- data.frame(x1 = x1_post, x2 = x2_post, U = U_post)

# Evaluación de Probabilidades de Bajo Consumo
c_umbral <- 0.5

prob_pre <- mean(sim_hetero_pre$x1 < c_umbral)
prob_post <- mean(sim_hetero_post$x1 < c_umbral)

cat("Prob. x1 < 0.5 Pre-Shock:", prob_pre, "\n")
cat("Prob. x1 < 0.5 Post-Shock:", prob_post, "\n")