rm(list=ls())
setwd("C:/Users/New/Documents/documentos-maestria/IAE")

datos_rayos <- read.table("http://astrostatistics.psu.edu/datasets/GRB_afterglow.dat",
           header=T, skip=1)
View(datos_rayos)

plot(datos_rayos$f, rep(5, length(datos_rayos$f)))
length(datos_rayos$f)  
     
hist(datos_rayos$f, breaks=seq(0,140,5) )  

##Ejercicio 1
prob_menor_40 <- mean(datos_rayos$f <=40)
print(prob_menor_40)

##Ejercicio 2
?ecdf

acumulada_X <- ecdf(datos_rayos$f)

plot(acumulada_X)

##Ejercicio 3
hist(datos_rayos$f, breaks=seq(0,140,10) )
##se asemeja levemente a una exponencial


?runif


##Análisis de datos Buffalo
View(datos_buffalo)

##Ejercicio 4
hist(datos_buffalo)
hist(datos_buffalo, breaks = seq(20,140, 10))
hist(datos_buffalo, breaks = seq(20,140, 5))
##Con el primer histograma parece semejante a una distribución
##normal. CUando se achican los bins, aparecen otras modas.

##Ejercicio 5
hist(datos_buffalo, breaks = seq(10,130, 10))
hist(datos_buffalo, breaks = seq(12,132, 10))
hist(datos_buffalo, breaks = seq(14,134, 10))

##Ejercicio 6
datos_buffalo

Probabilidad_pertenecer <- function(x,dat,h){
  probabilidad <- mean( dat>=(x-h) & dat<=(x+h))
  probabilidad
  }
Probabilidad_pertenecer(40,datos_buffalo,10)

##Ejercicio 7
probabilidades_h_10 <- rep(NA, 63)
probabilidades_h_20 <- rep(NA, 63)
probabilidades_h_30 <- rep(NA, 63)


for( i in 1:length(probabilidades_h_10) ) {
  probabilidades_h_10[i] <- Probabilidad_pertenecer(datos_buffalo[i], datos_buffalo, 10)
  probabilidades_h_20[i] <- Probabilidad_pertenecer(datos_buffalo[i], datos_buffalo, 20)
  probabilidades_h_30[i] <- Probabilidad_pertenecer(datos_buffalo[i], datos_buffalo, 30)

}

hist(probabilidades_h_10)
hist(probabilidades_h_20)
hist(probabilidades_h_30)

##Ejercicio 8
densidad.est.parzen <- function(x,dat,h){
  estimacion_densidad <- mean( dat>=(x-h) & dat<=(x+h) ) /(2*h)
  estimacion_densidad
}

densidad.est.parzen(40,datos_buffalo,10)

##Ejercicio 9
grilla <- seq(25, 126.4, length.out=200 )

estimacion_grilla <- rep(NA, length(grilla))
for(i in 1:length(estimacion_grilla)){
estimacion_grilla[i] <- densidad.est.parzen(grilla[i], datos_buffalo, 10) 
}
plot(grilla, estimacion_grilla, pch=20, type="l")

##Ejercicio 11
estimacion_grilla_10 <- estimacion_grilla
estimacion_grilla_20 <- rep(NA, length(grilla))
estimacion_grilla_30 <- rep(NA, length(grilla))
for(i in 1:length(estimacion_grilla)){
  estimacion_grilla_20[i] <- densidad.est.parzen(grilla[i], datos_buffalo, 20) 
  estimacion_grilla_30[i] <- densidad.est.parzen(grilla[i], datos_buffalo, 30)
}

hist(datos_buffalo, freq=FALSE)
points(estimacion_grilla_10, type="l")




##Ejercicio clase
densidad.est.parzen(datos_buffalo, 10, 80)

density(datos_buffalo, kernel="rectangular", bw=10/sqrt(3), from=80, to=80, n=1)$y

estimador_unif <- density(datos_buffalo, kernel="rectangular", bw=10/sqrt(3))
approxfun(estimbuffunif)(80)

##Para trabajar con los datos menos el 17
density(buff[-17])
##Devuelve el valor de h óptimo por convalidación cruzada.
bw.ucv(buff)



##La primera forma calcula en un punto. La segunda calcula en 512 puntos, y en la segunda línea
## interpola los dos valores que están antes y después del buscado

datos_peso <- read.csv("datos_20_n_100.csv")
hist(datos_peso$x, freq=FALSE, ylim = c(0,0.3))

estimador_gauss <- density(datos_peso$x, kernel="gaussian", bw=0.5)

approxfun(estimador_gauss)(16)

# Borro todo
rm(list=ls())

# Defino directorio de trabajo
setwd("C:/Users/damia/Downloads/Posgrado/Introducción al aprendizaje estadístico/Clase 3/Práctica")

df <- read.csv("datos_20_n_100.csv")

hist(df$x, freq = FALSE, ylim=c(0,0.30))

densidad_h2 <- density(df$x, kernel = "gaussian", bw = 2)
densidad_h4 <- density(df$x, kernel = "gaussian", bw = 4)
densidad_h1 <- density(df$x, kernel = "gaussian", bw = 1)
densidad_h0.5 <- density(df$x, kernel = "gaussian", bw = 0.5)



grilla <- seq(min(df$x), max(df$x), length.out = 100)

hist(df$x, breaks = 30,freq = FALSE, ylim=c(0,0.30))
lines(densidad_h0.5, col = "purple")
lines(densidad_h1, col = "red")
lines(densidad_h2, col = "blue")
lines(densidad_h4, col = "green")

approxfun(densidad_h0.5)(16)
approxfun(densidad_h0.5)(18)
approxfun(densidad_h0.5)(20)
approxfun(densidad_h0.5)(22)



plot(densidad_h0.5)











media <- mean(datos_rayos$f)
desvio <- sd(datos_rayos$f, )

hist(pp)
hist(pp, breaks=seq(20,140,10) )
hist(pp, breaks=seq(20,140,1) )
hist(pp, breaks=seq(20,140,2) )
hist(pp, breaks=seq(21,141,2) )
hist(pp, breaks=seq(20,140,3) )
hist(pp, breaks=seq(20,140,4) )
hist(pp, breaks=seq(20,140,5) )
hist(pp, breaks=seq(20,140,6) )

hist(pp, by=6 )
hist(pp, breaks=seq(20,140,7) )
hist(pp, breaks=seq(20,140,8) )
hist(pp, breaks=seq(20,140,9) )
hist(pp, breaks=seq(20,140,10) )
hist(pp, breaks=seq(20,140,11) )
hist(pp, breaks=seq(20,140,12) )
hist(pp, breaks=seq(20,140,13) )
hist(pp, breaks=seq(20,140,14) )
hist(pp, breaks=seq(20,140,15) )

acumulada_Y <- ecdf(pp)
acumulada_Y(55)-acumulada_Y(25)
